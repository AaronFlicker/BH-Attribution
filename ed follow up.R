library(tidyverse)
library(lubridate)
library(odbc)

con <- dbConnect(odbc(), "CaboodleProd")

ed <- dbGetQuery(con, "
 SELECT DISTINCT pt.DurableKey AS PatientDurableKey
        				,pt.PrimaryMRN AS MRN
        				,pt.Name
        				,pt.BirthDate
        				,enc.EncounterEpicCSN
        				,enc.AdmissionType
        				,enc.PrimaryCoverageKey
        				,cov.PayorFinancialClass
        				,d.DepartmentName
        				,dd.DateValue AS EncStartDate
        				,dd2.DateValue AS EncEndDate
        				,enc.DischargeDisposition
        				,dur.Years AS Age
        				,pr.Name AS BehavioralHealthProvider
        				,ct.StartInstant AS CareTeamStartInstant
        				,ct.EndInstant AS CareTeamEndInstant
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
			ON dd.DateKey = enc.DateKey  -- admit date
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
		LEFT JOIN Caboodle.dbo.ProviderDim pr
			ON ct.ProviderKey = pr.ProviderKey
		LEFT JOIN Caboodle.dbo.CoverageDim cov
			ON enc.PrimaryCoverageKey = cov.CoverageKey
		WHERE enc.IsHospitalAdmission = 0
			AND enc.AdmissionType IN ('Emergency', 'Urgent') 
			AND dev._IsDeleted = 0
			AND dev.IsPrimary = 1
			AND pt.IsCurrent = 1
			AND enc.DischargeDisposition IN (
			  'Discharged to Home or Self Care (Routine Discharge)',
			  '',
			  'Home or Self Care'
			)
") |>
  arrange(EncounterEpicCSN, CareTeamStartInstant) |>
  group_by(EncounterEpicCSN) |>
  mutate(rn = row_number()) |>
  pivot_wider(
    id_cols = PatientDurableKey:Age,
    names_from = rn,
    names_prefix = "BehavioralHealthProvider",
    values_from = BehavioralHealthProvider
  ) 

hosp <- dbGetQuery(con, "
  SELECT DISTINCT pt.DurableKey AS PatientDurableKey
				,pt.PrimaryMRN AS MRN
				,enc.EncounterEpicCSN
				,enc.AdmissionType
				,d.DepartmentName
				,dd.DateValue AS EncStartDate
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
  ed,
  hosp,
  join_by(PatientDurableKey, EncEndDate <= EncStartDate)
 ) |>
  mutate(diff = as.numeric(EncStartDate.y - EncEndDate)) |>
  filter(
    EncounterEpicCSN.x != EncounterEpicCSN.y,
    diff <= 7
    ) |>
  mutate(EncounterEpicCSN = EncounterEpicCSN.x)

denom <- anti_join(ed, exclude, join_by(EncounterEpicCSN))

op <- dbGetQuery(con, "
  SELECT DISTINCT pt.DurableKey AS PatientDurableKey
          				,pt.PrimaryMRN AS MRN
          				,enc.VisitType
          				,enc.IsOutpatientFaceToFaceVisit
          				,enc.EncounterEpicCSN
          				,enc.AdmissionType
          				,d.DepartmentName
          				,dd.DateValue AS EncStartDate
          				,dd2.DateValue AS EncEndDate
          				,y.Value AS DXCode
          				,enc.DischargeDisposition
          				,pr.Name AS BehavioralHealthProvider
          				,ct.StartInstant AS CareTeamStartInstant
          				,ct.EndInstant AS CareTeamEndInstant
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
		INNER JOIN Caboodle.dbo.DateDim dd2
			ON dd2.DateKey = enc.DischargeDateKey 
		LEFT JOIN Caboodle.dbo.CareTeamFact ct
			ON ct.PatientKey = pt.PatientKey
				AND ct.Role = 'Behavioral Health Service'
				AND (
					dd.DateValue BETWEEN ct.StartInstant AND ct.EndInstant
					OR dd2.DateValue BETWEEN ct.StartInstant AND ct.EndInstant
					)
		LEFT JOIN Caboodle.dbo.ProviderDim pr
			ON ct.ProviderKey = pr.ProviderKey
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
hv <- read_csv(hvlist[1], col_types = "ccccc")

for(i in 2:length(hvlist)){
  x <- read_csv(hvlist[i], col_types = "ccccc")
  hv <- rbind(hv, x)
}

hv <- hv |>
  mutate(VisitDate = ymd(LAST_SEEN_DTS)) |>
  distinct(MRN, VisitDate) |>
  filter(VisitDate >= "2026-01-01")

opall <- select(op, MRN, VisitDate = EncStartDate) |>
  rbind(hv) |>
  unique()

num <- inner_join(
  denom, 
  opall, 
  join_by(MRN, EncEndDate <= VisitDate), 
  relationship = "many-to-many"
  ) |>
  mutate(diff = as.numeric(VisitDate - EncEndDate)) |>
  group_by(EncounterEpicCSN) |>
  filter(
    diff == min(diff),
    diff <= 7
    )
