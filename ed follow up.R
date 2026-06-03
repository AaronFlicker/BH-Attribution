library(tidyverse)
library(lubridate)
library(odbc)

con <- dbConnect(odbc(), "CaboodleProd")

ed <- dbGetQuery(con, "
 SELECT DISTINCT pt.DurableKey AS PatientDurableKey
        				,pt.PrimaryMRN AS MRN
        				,pt.Name
        				,pt.BirthDate
        				,pr2.Name AS PCP
        				,enc.EncounterEpicCSN AS CSN
        				,enc.AdmissionType
        				,enc.PrimaryCoverageKey
        				,cov.PayorFinancialClass
        				,d.DepartmentName
        				,dd.DateValue AS EDVisitDate
        				,dd2.DateValue AS EDDischargeDate
        				,enc.DischargeDisposition
        				,dur.Years AS Age
        				,pr1.Name AS Provider
        				,ct.Role
        				,ct.IsPrimaryCareRole
        				,ct.StartInstant AS CareTeamStartInstant
        				,ct.EndInstant AS CareTeamEndInstant
        				,CASE WHEN pr1.Name LIKE '%beech acres%' THEN 'Beech Acres'
        				  WHEN pr1.Name LIKE '%best point%' THEN 'Best Point'
        				  WHEN pr1.Name LIKE '%camelot%' THEN 'Camelot'
        				  WHEN pr1.Name LIKE '%community behavioral health%' 
        				    THEN 'Community Behavioral Health'
        				  WHEN pr1.Name LIKE '%focus on youth%' THEN 'Focus on Youth'
        				  WHEN pr1.Name LIKE '%greater cin bh svc%'
        				    THEN 'Greater Cin BH SVC'
        				  WHEN pr1.Name LIKE '%integrated%' THEN 'Integrated SVC BH'
        				  WHEN pr1.Name LIKE '%lighthouse%' THEN 'Lighthouse'
        				  WHEN pr1.Name LIKE '%newpath%' THEN 'NewPath'
        				  WHEN pr1.Name LIKE '%talbert%' THEN 'Talbert House'
        				  WHEN pr1.Name LIKE '%transitions%' THEN 'Transitions'
        				  ELSE NULL END AS BehavioralHealthProvider
	FROM Caboodle.dbo.EncounterFact enc
		INNER JOIN Caboodle.dbo.PatientDim pt
			ON enc.PatientDurableKey = pt.DurableKey
		INNER JOIN Caboodle.dbo.DurationDim dur
		  ON enc.AgeKey = dur.DurationKey
		INNER JOIN Caboodle.dbo.DiagnosisEventFact dev
			ON dev.EncounterKey = enc.EncounterKey
				AND dev.Type = 'encounter diagnosis'
		INNER JOIN Caboodle.dbo.DepartmentDim d
			ON d.DepartmentKey = enc.DepartmentKey
				AND d.DepartmentEpicID IN ('30001001','30010001')
		INNER JOIN Caboodle.dbo.DiagnosisTerminologyDim dtg
			ON dtg.DiagnosisKey = dev.DiagnosisKey
		INNER JOIN Caboodle.dbo.DiagnosisTerminologyDim AS y
			ON y.DiagnosisKey = dev.DiagnosisKey
		INNER JOIN Caboodle.dbo.TerminologyConceptSetDim AS x
			ON x.TerminologyConceptKey = y.TerminologyConceptKey
				AND x.Concept = y.Value
				AND y.Type = 'ICD-10-CM'
				AND x.StandardName = 'ICD-10-CM'
				AND x.Name IN (
						'HP HEDIS V1 2023 MENTAL HEALTH DIAGNOSIS 2023-03-31',
						'HP HEDIS V1 2023 INTENTIONAL SELF-HARM 2023-03-31'
							)
		INNER JOIN Caboodle.dbo.DateDim dd
			ON dd.DateKey = enc.DateKey  
				AND dd.DateValue >= '2026-01-01'
		INNER JOIN Caboodle.dbo.DateDim dd2
			ON dd2.DateKey = enc.DischargeDateKey 
		LEFT JOIN Caboodle.dbo.CareTeamFact ct
			ON ct.PatientKey = pt.PatientKey
			AND ct.Role = 'Behavioral Health Service'
			AND (
				dd.DateValue BETWEEN ct.StartInstant AND ct.EndInstant
				OR dd2.DateValue BETWEEN ct.StartInstant AND ct.EndInstant
				)
		LEFT JOIN Caboodle.dbo.ProviderDim pr1
			ON ct.ProviderKey = pr1.ProviderKey
		LEFT JOIN Caboodle.dbo.ProviderDim pr2
		  ON pt.PrimaryCareProviderKey = pr2.ProviderKey
		LEFT JOIN Caboodle.dbo.CoverageDim cov
			ON enc.PrimaryCoverageKey = cov.CoverageKey
		WHERE enc.IsHospitalAdmission = 0
			AND enc.AdmissionType IN ('Emergency', 'Urgent') 
			AND dev._IsDeleted = 0
			AND dev.IsPrimary = 1
			AND pt.IsCurrent = 1
			AND enc.DischargeDisposition IN (
			  'Discharged to Home or Self Care (Routine Discharge)',
			  'Home or Self Care'
			)
")

ed_unique <- ed |>
  distinct(
    PatientDurableKey, 
    MRN, 
    Name, 
    BirthDate, 
    PCP,
    CSN, 
    PayorFinancialClass, 
    DepartmentName, 
    EDVisitDate, 
    EDDischargeDate, 
    Age
    ) 

hosp <- dbGetQuery(con, "
  SELECT DISTINCT pt.DurableKey AS PatientDurableKey
				,pt.PrimaryMRN AS MRN
				,enc.EncounterEpicCSN AS CSN
				,enc.AdmissionType
				,d.DepartmentName
				,dd.DateValue AS HospDate
	FROM Caboodle.dbo.EncounterFact enc
		INNER JOIN Caboodle.dbo.PatientDim pt
			ON enc.PatientDurableKey = pt.DurableKey
				AND pt.IsCurrent = 1
		INNER JOIN Caboodle.dbo.DepartmentDim d
			ON d.DepartmentKey = enc.DepartmentKey
		INNER JOIN Caboodle.dbo.DateDim dd
			ON dd.DateKey = enc.DateKey  
				AND dd.DateValue >= '2026-01-01'
	WHERE (enc.IsHospitalAdmission = 1 OR enc.IsEDVisit = 1)
		AND AdmissionType != 'Urgent'
		AND enc.EncounterEpicCSN IS NOT NULL                 
   ")

exclude <- inner_join(
  ed_unique,
  hosp,
  join_by(PatientDurableKey, EDDischargeDate <= HospDate)
 ) |>
  mutate(diff = as.numeric(HospDate - EDDischargeDate)) |>
  filter(
    CSN.x != CSN.y,
    diff <= 7
    ) 

ed_denom <- anti_join(ed_unique, exclude, join_by(CSN == CSN.x)) 

op <- dbGetQuery(con, "
  SELECT DISTINCT pt.DurableKey AS PatientDurableKey
          				,pt.PrimaryMRN AS MRN
          				,enc.VisitType
          				,enc.IsOutpatientFaceToFaceVisit
          				,enc.EncounterEpicCSN AS CSN
          				,enc.AdmissionType
          				,d.DepartmentName
          				,dd.DateValue AS VisitDate
          				,y.Value AS DXCode
          				,enc.DischargeDisposition
	FROM Caboodle.dbo.EncounterFact enc
		INNER JOIN Caboodle.dbo.PatientDim pt
			ON enc.PatientDurableKey = pt.DurableKey
		INNER JOIN Caboodle.dbo.DiagnosisEventFact dev
			ON dev.EncounterKey = enc.EncounterKey
		INNER JOIN Caboodle.dbo.DepartmentDim d
			ON d.DepartmentKey = enc.DepartmentKey
		INNER JOIN Caboodle.dbo.DiagnosisTerminologyDim dtg
			ON dtg.DiagnosisKey = dev.DiagnosisKey
		INNER JOIN Caboodle.dbo.DiagnosisTerminologyDim y
			ON y.DiagnosisKey = dev.DiagnosisKey
		INNER JOIN Caboodle.dbo.TerminologyConceptSetDim x
			ON x.TerminologyConceptKey = y.TerminologyConceptKey
				AND x.Concept = y.Value
				AND y.Type = 'ICD-10-CM'
				AND x.StandardName = 'ICD-10-CM'
				AND x.Name IN (
						'HP HEDIS V1 2023 MENTAL HEALTH DIAGNOSIS 2023-03-31',
						'HP HEDIS V1 2023 INTENTIONAL SELF-HARM 2023-03-31'
							)
		INNER JOIN Caboodle.dbo.DateDim dd
			ON dd.DateKey = enc.DateKey 
	WHERE enc.IsHospitalAdmission = '0' 
		AND enc.AdmissionType IN ('*Not Applicable')  
		AND dev.Type = 'encounter diagnosis'
		AND dev._IsDeleted = 0
		AND dev.IsPrimary = 1
		AND dd.DateValue >= '2026-01-01'               
   ")

setwd("//chmccorp/root1/File-Hub/IS/HealthVine_Operations/Internal/Behavioral Health")
hvlist <- list.files()
hvlist <- hvlist[str_ends(hvlist, ".csv")]
hv <- read_csv(hvlist[1], col_types = "ccccc") |>
  mutate(
    FixedDate = str_replace_all(LAST_SEEN_DTS, "/", "-"),
    VisitDate = case_when(
      str_detect(LAST_SEEN_DTS, "/") ~ mdy(FixedDate),
      TRUE ~ ymd(FixedDate)
    )
  )

for(i in 2:length(hvlist)){
  x <- read_csv(hvlist[i], col_types = "ccccc") |>
    mutate(
      FixedDate = str_replace_all(LAST_SEEN_DTS, "/", "-"),
      VisitDate = case_when(
        str_detect(LAST_SEEN_DTS, "/") ~ mdy(FixedDate),
        TRUE ~ ymd(FixedDate)
      )
    )
  hv <- rbind(hv, x)
}

hv2 <- hv |>
  distinct(MRN, PRACTICE_NAME, VisitDate) |>
  filter(VisitDate >= "2026-01-01")

opall <- select(op, MRN, VisitDate, PRACTICE_NAME = DepartmentName) |>
  rbind(hv2) |>
  unique()

success <- inner_join(
  ed_denom, 
  opall, 
  join_by(MRN, EDDischargeDate <= VisitDate), 
  relationship = "many-to-many"
  ) |>
  mutate(diff = as.numeric(VisitDate - EDDischargeDate)) |>
  filter(diff <= 7) |>
  distinct(CSN, VisitDate, PRACTICE_NAME
    ) |>
  group_by(CSN, PRACTICE_NAME) |>
  filter(VisitDate == min(VisitDate)) |>
  ungroup() |>
  right_join(ed_denom) |>
  mutate(Success = ifelse(is.na(VisitDate), 0, 1)) |>
  filter(EDVisitDate < today() - 14)

attrib <- inner_join(success |> distinct(CSN), select(ed, CSN, Provider)) |>
  mutate(
    Provider = case_when(
      str_detect(Provider, "BEECH ACRES") ~ "Beech Acres",
      str_detect(Provider, "BEST POINT") ~ "Best Point",
      str_detect(Provider, "CAMELOT") ~ "Camelot",
      str_detect(Provider, "COMMUNITY") ~ "Community Behavioral Health",
      str_detect(Provider, "FOCUS") ~ "Focus on Youth",
      str_detect(Provider, "GREATER") ~ "Greater Cincinnati BH SVC",
      str_detect(Provider, "INTEGRATED") ~ "Integrated SVC BH",
      str_detect(Provider, "LIGHTHOUSE") ~ "Lighthouse",
      str_detect(Provider, "NEWPATH") ~ "NewPath",
      str_detect(Provider, "TALBERT") ~ "Talbert House",
      str_detect(Provider, "TRANSITIONS") ~ "Transitions",
      is.na(Provider) ~ "None"
    )
  ) |>
  unique()

ed_final <- success |>
  distinct(
    CSN, 
    MRN,
    Name,
    BirthDate,
    PayorFinancialClass,
    PCP,
    DepartmentName,
    EDVisitDate,
    EDDischargeDate,
    Age,
    Success
    ) |>
  left_join(attrib) |>
  group_by(CSN) |>
  mutate(rn = row_number()) |>
  ungroup() |>
  pivot_wider(
    id_cols = CSN:Success, 
    names_from = rn, 
    names_prefix = "BHProvider",
    values_from = Provider
    ) |>
  mutate(
    Month = case_when(
      floor_date(EDVisitDate, "month") < floor_date(today() - 14, "month") ~ 
        floor_date(EDVisitDate, "month")
    ),
    PayorFinancialClass = ifelse(
      PayorFinancialClass == "HMO Medicaid Cap",
      "HMO Medicaid Cap (HealthVine)",
      PayorFinancialClass
      ),
    Failure = ifelse(Success == 1, 0, 1),
    PCP = ifelse(
      PCP %in% c(
        "FAIRFIELD, PRIMARY CARE CENTER GROUP", 
        "HOPPLE STREET, HEALTH CENTER GROUP", 
        "HUGHES CENTER HS, PRIMARY CARE CENTER", "PEDIATRIC, PRIMARY CARE", 
        "ROCKDALE ACADEMY, PRIMARY CARE CENTER", 
        "SOUTH AVONDALE ELEMENTARY, PRIMARY CARE CENTER"
        ),
      paste0(("*"), PCP),
      PCP
      )
    ) 

fu <- success |>
  distinct(CSN, VisitDate, PRACTICE_NAME)

setwd("~/Behavioral Health/Attribution/BH-Attribution")
write_csv(ed_final, "ed visits.csv")
write_csv(attrib, "bh providers.csv")
write_csv(fu, "fu providers.csv")
