library(tidyverse)
library(janitor)

# Read RDS data ----------------------------------------------------------------
raw <- readRDS("data/nda_raw.rds")

# Clean data -------------------------------------------------------------------
data <- raw %>%
  # Age category
  mutate(
    age_band = cut(
      AGE,
      breaks = c(-Inf, 17, 39, 49, 59, 69, 79, Inf),
      labels = c("< 18", "18 - 40", "40-49", "50-59", "60-69", "70-79", "≥ 80"),
      RIGHT = TRUE # Include the upper bound in each interval
    )
  ) %>% 
  # Gender category
  mutate(
    gender = case_when(
      DERIVED_CLEAN_SEX == 1 ~ "Male",
      DERIVED_CLEAN_SEX == 2 ~ "Female",
      DERIVED_CLEAN_SEX == 9 ~ "Not specified",
      TRUE ~ NA_character_
    )
  ) %>% 
  # Ethnicity category
  mutate(
    ethnicity = case_when(
      DERIVED_CLEAN_ETHNICITY %in% c('A', 'B', 'C') ~ 'White',
      DERIVED_CLEAN_ETHNICITY %in% c('D', 'E', 'F', 'G')  ~ 'Mixed',
      DERIVED_CLEAN_ETHNICITY %in% c('H', 'J', 'K', 'L') ~ 'Asian',
      DERIVED_CLEAN_ETHNICITY %in% c('M', 'N', 'P') ~ 'Black',
      DERIVED_CLEAN_ETHNICITY %in% c('R', 'S', 'S2', 'S3') ~ 'Other',
      DERIVED_CLEAN_ETHNICITY %in% c('Z') ~ 'NK',
      TRUE ~ NA_character_
    )
  ) %>% 
  # IMD quintile
  mutate(
    IMD_QUINTILE = as.integer(IMD_QUINTILE)
  ) %>% 
  # Diabetes type
  mutate(
    diabetes_type = case_when(
      DERIVED_CLEAN_DIABETES_TYPE == 1 ~ "Type 1",
      DERIVED_CLEAN_DIABETES_TYPE == 2 ~ "Type 2",
      TRUE ~ "Other"
    )
  ) %>% 
  # Diabetes duration (continous)
  mutate(
    CLEAN_DIAGNOSIS_DATE = as.Date(CLEAN_DIAGNOSIS_DATE),
    DIABETES_DURATION = ifelse(!(is.na(DIABETES_DURATION)), 
                               floor(as.numeric(difftime("2024-03-31", CLEAN_DIAGNOSIS_DATE, units = "days")) / 365.25),
                               NA_integer_
                               )
  ) %>% 
  # Diabetes duration (categorical)
  mutate(
    diabetes_duration_category = case_when(
      DIABETES_DURATION < 1                              ~ "<1",
      DIABETES_DURATION >= 1  & DIABETES_DURATION <= 2   ~ "1-2",
      DIABETES_DURATION >= 3  & DIABETES_DURATION <= 4   ~ "3-4",
      DIABETES_DURATION >= 5  & DIABETES_DURATION <= 9   ~ "5-9",
      DIABETES_DURATION >= 10 & DIABETES_DURATION <= 14  ~ "10-14",
      DIABETES_DURATION >= 15 & DIABETES_DURATION <= 19  ~ "15-19",
      DIABETES_DURATION >= 20                            ~ "≥20",
      TRUE                                               ~ NA_character_ 
    )
  ) %>% 
  # BMI
  mutate(
    CLEAN_BMI_VALUE = as.numeric(CLEAN_BMI_VALUE),
    BMI_category = case_when(
      # need to include ethnicity criteria as South Asian BMI is 27.5 
      # https://www.researchgate.net/figure/Classification-of-weight-according-to-BMI-for-Caucasian-South-Asian-and-Chinese_tbl1_340960106
      CLEAN_BMI_VALUE < 18.5                            ~ "Underweight",
      CLEAN_BMI_VALUE >= 18.5 & CLEAN_BMI_VALUE <= 24.9 ~ "Normal weight",
      CLEAN_BMI_VALUE >= 25 & CLEAN_BMI_VALUE <= 29.9   ~ "Overweight",
      CLEAN_BMI_VALUE >= 30 & CLEAN_BMI_VALUE <= 39.9   ~ "Obesity",
      CLEAN_BMI_VALUE >= 40                             ~ "Severe Obesity",
      TRUE                                              ~ NA_character_ 
    )
  ) %>% 
  # HBAIC category (mmol/mol)
  mutate(
    CLEAN_MMOL_HBA1C_VALUE = as.numeric(CLEAN_MMOL_HBA1C_VALUE),
    HBA1C_MMOL_category = case_when(
      CLEAN_MMOL_HBA1C_VALUE < 48 ~ "<48",
      CLEAN_MMOL_HBA1C_VALUE >= 48 & CLEAN_MMOL_HBA1C_VALUE <= 53 ~ "48-53",
      CLEAN_MMOL_HBA1C_VALUE >= 54 & CLEAN_MMOL_HBA1C_VALUE <= 58 ~ "54-58",
      CLEAN_MMOL_HBA1C_VALUE >= 59 & CLEAN_MMOL_HBA1C_VALUE <= 74 ~ "59-74",
      CLEAN_MMOL_HBA1C_VALUE >= 75 & CLEAN_MMOL_HBA1C_VALUE <= 85 ~ "75-85",
      CLEAN_MMOL_HBA1C_VALUE >= 86                               ~ "≥86",
      TRUE ~ NA_character_
      
    )
  ) %>% 
  # BP NICE target met
  mutate(
    `BP_<140_80` = as.integer(`BP_<140_80`)
  ) %>% 
  # Cholestrol category (mmol/L)
  mutate(
    CLEAN_CHOLESTEROL_VALUE = as.numeric(CLEAN_CHOLESTEROL_VALUE),
    cholesterol_category = case_when(
      CLEAN_CHOLESTEROL_VALUE <= 5 ~ "≤5",
      CLEAN_CHOLESTEROL_VALUE > 5  ~ ">5",
      TRUE ~ NA
    )
  ) %>% 
  # On Statins
  mutate(
    statin_flag = as.integer(statin_flag)
  ) %>% 
  # Smoking status
  mutate(
    smoking_category = case_when(
      CLEAN_SMOKING_VALUE == 1 ~ "Current smoker",
      CLEAN_SMOKING_VALUE == 2 ~ "Ex-smoker",
      CLEAN_SMOKING_VALUE == 3 ~ "Non-smoke history unknown",
      CLEAN_SMOKING_VALUE == 4 ~ "Never smoked",
      CLEAN_SMOKING_VALUE == 9 ~ "Unknown",
      TRUE ~ NA_character_
    )
  ) %>%
  # Albumin Test
  mutate(
    CLEAN_ALBUMIN_TEST = as.integer(CLEAN_ALBUMIN_TEST),
    CLEAN_ALBUMIN_TEST = case_when(
      CLEAN_ALBUMIN_TEST  == 1 ~ "Albumin concentration (mg/L)",
      CLEAN_ALBUMIN_TEST  == 2 ~ "ACR (mg/mmol)",
      CLEAN_ALBUMIN_TEST  == 3 ~ "Timed overnight albumin (ug/min)",
      CLEAN_ALBUMIN_TEST  == 4 ~ "24hr albumin excretion (mg/24hr)",
      TRUE ~ as.character(CLEAN_ALBUMIN_TEST)
    )
  ) %>%
  # Albumin Stage
  mutate(
    CLEAN_ALBUMIN_STAGE = as.integer(CLEAN_ALBUMIN_STAGE),
    CLEAN_ALBUMIN_STAGE= case_when(
      CLEAN_ALBUMIN_STAGE  == 1 ~ "Normoalbuminuria",
      CLEAN_ALBUMIN_STAGE  == 2 ~ "Microalbuminuria",
      CLEAN_ALBUMIN_STAGE  == 3 ~ "Macroalbuminuria",
      TRUE ~ as.character(CLEAN_ALBUMIN_STAGE)
    )
  ) %>%
  # Ensure data type for each care process is integer
  mutate(
    across(c(HBA1C, CREATININE, CHOLESTEROL, ALBUMIN, BLOOD_PRESSURE, 
             BMI, FOOT_EXAM, EYE_EXAM_CP, SMOKING), ~as.integer(as.numeric(.)))
  ) %>% 
  # Number of care processes completed
  mutate(
    care_processes_completed = rowSums(select(., HBA1C, CREATININE, CHOLESTEROL, ALBUMIN, BLOOD_PRESSURE, 
                                              BMI, FOOT_EXAM, EYE_EXAM_CP, SMOKING))
  ) %>% 
  # Ensure data type for each treatment target is integer
  mutate(
    across(c(`BP_<140_80`, `CHOLESTEROL_<5MMOL/L`, `HBAIC_<=58MMOL`), ~as.integer(as.numeric(.)))
  ) %>% 
  # Number of treatment targets achieved
  mutate(
    treatment_targets_achieved = rowSums(select(., `BP_<140_80`, `CHOLESTEROL_<5MMOL/L`, `HBAIC_<=58MMOL`))
  ) %>% 
  # Ensure data type for other columns are correct
  mutate(
    across(c(CLEAN_CREATININE_VALUE, CLEAN_SYSTOLIC_VALUE , CLEAN_DIASTOLIC_VALUE,
             CLEAN_ALBUMIN_VALUE, Frailty, All_9_CARE_PROCESSES), ~as.integer(as.numeric(.)))
  )

# Select variables of interest
clean_data <- data %>% 
  select(PatientId, AGE, age_band, gender, ethnicity, IMD_QUINTILE, DERIVED_LSOA, 
         diabetes_type, DIABETES_DURATION, diabetes_duration_category, CLEAN_BMI_VALUE, BMI_category, CLEAN_MMOL_HBA1C_VALUE ,HBA1C_MMOL_category,
         CLEAN_SYSTOLIC_VALUE, CLEAN_DIASTOLIC_VALUE,`BP_<140_80`, CLEAN_CHOLESTEROL_VALUE, cholesterol_category,
         statin_flag, smoking_category, ALL_8_CARE_PROCESSES, All_9_CARE_PROCESSES, care_processes_completed, 
         ALL_3_TREATMENT_TARGETS, treatment_targets_achieved) %>% 
  janitor::clean_names()

# Check for any duplicates
clean_data %>% 
  count(patient_id) %>% 
  filter(n > 1)

# Save clean data as RDS
saveRDS(clean_data, "data/nda_clean.rds")
  



