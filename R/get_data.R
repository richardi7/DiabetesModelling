library(DBI)
library(odbc)
library(tidyverse)

# Connect to the database ------------------------------------------------------
con <-
  dbConnect(
    odbc(),
    Driver = "SQL Server",
    Server = "MLCSU-BI-SQL",
    Database = "EAT_Reporting_BSOL",
    Trusted_Connection = "True"
  )

# SQL query --------------------------------------------------------------------
sql_query <- "SELECT [PatientId]
      ,[AGE]
      ,[DERIVED_CLEAN_SEX]
      ,[DERIVED_CLEAN_ETHNICITY]
      ,[IMD_QUINTILE]
      ,[DERIVED_LSOA]
      ,[DERIVED_CLEAN_DIABETES_TYPE]
      ,[DERIVED_CLEAN_BIRTH_YEAR]
      ,[CLEAN_DIAGNOSIS_DATE]
      ,[DERIVED_CLEAN_DIAGNOSIS_YEAR]
      ,[DIABETES_DURATION]
      ,[CLEAN_SMI_FLAG]
      ,[CLEAN_CREATININE_VALUE]
      ,[CREATININE_DATE]
      ,[CREATININE]
      ,[CLEAN_SYSTOLIC_VALUE]
      ,[CLEAN_DIASTOLIC_VALUE]
      ,[BP_&lt;=140_80] AS [BP_<140_80]
      ,[BP_DATE]
      ,[BLOOD_PRESSURE]
      ,[CLEAN_BMI_VALUE]
      ,[BMI_DATE]
      ,[BMI]
      ,[CLEAN_CHOLESTEROL_VALUE]
      ,[CHOLESTEROL_DATE]
      ,[CHOLESTEROL]
      ,[CHOLESTEROL_&lt;5MMOL/L] AS [CHOLESTEROL_<5MMOL/L]
      ,[CLEAN_FOOT_EXAM_DATE]
      ,[FOOT_EXAM]
      ,[HBA1C]
      ,[HBA1C_&lt;=7.5%OR58MMOL] as [HBAIC_<=58MMOL]
      ,[HBA1C_DATE]
      ,[CLEAN_MMOL_HBA1C_VALUE]
      ,[CLEAN_PERCENTAGE_HBA1C_VALUE]
      ,[CLEAN_SMOKING_VALUE]
      ,[SMOKING_DATE]
      ,[SMOKING]
      ,[CLEAN_ALBUMIN_VALUE]
      ,[CLEAN_ALBUMIN_TEST]
      ,[CLEAN_ALBUMIN_STAGE]
      ,[ALBUMIN_DATE]
      ,[ALBUMIN]
      ,[CLEAN_IHD_VALUE]
      ,[ALL_3_TREATMENT_TARGETS]
      ,[ALL_8_CARE_PROCESSES]
      ,[Clean_Eye_Exam_value]
      ,[Clean_Eye_Exam_Date]
      ,[EYE_EXAM_CP]
      ,[Clean_Foot_Exam_Value]
      ,[All_9_CARE_PROCESSES]
      ,[CVD_admission]
      ,[statin_flag]
      ,[Frailty]
      ,[Frailty_Date]
FROM [LocalFeeds].[Reporting].[NationalDiabetesAudit_NDA_Core_Data] T1
INNER JOIN (
    SELECT 
          [GPPracticeCode_Original],
          [GPPracticeCode_current],
          [Locality],
          [PCN]
    FROM EAT_Reporting_BSOL.Reference.BSOL_ICS_PracticeMapped
    WHERE ICS_2223 = 'BSOL'
) T2
ON T1.DERIVED_GP_PRACTICE_CODE = T2.GPPracticeCode_Original
WHERE T1.AUDIT_YEAR = '202324E4'"

# Extract the data -------------------------------------------------------------
data <- dbGetQuery(con, sql_query)

# Save as RDS ------------------------------------------------------------------
saveRDS(data, "data/nda_raw.rds")
