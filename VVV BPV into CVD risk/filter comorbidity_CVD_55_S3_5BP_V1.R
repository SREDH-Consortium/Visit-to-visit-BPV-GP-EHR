library("dplyr")
library("lubridate")
library("eeptools")
library("data.table")
library("stringr")
library("writexl")
library("xlsx")


data <- `Demographics_BloodPRessure`
h_outcome <- Hospital_Admissions
gp_outcome <- BPV_CardiacEvents
med <- Medications
com <- Comorbidities
lab <- LabValues
bmi <- `Height+Weight+BMI`

# reason_list <- com %>%
#   distinct(Reason)
# 
# write_xlsx(reason_list, "distinct_reasons.xlsx")
# 
# 
# reason_list <- h_outcome %>%
#   distinct(DIAGNOSIS_DESCRIPTION)
# 
# write_xlsx(reason_list, "distinct_reasons_HOSPITAL.xlsx")

####### HT comorbidities GP ###################

keywords_com_ht <- c(
  "hypertension", "ht\\(hypertension\\)", "ht/diabetes", "ht/eds", "ht/gp care", "diastolic ht", "htn", "diebetes / ht / care plan review",
  "ht / gp care plan 721"
)

filtered_com_ht <- gp_outcome_mix %>%
  mutate(Reason_lower = tolower(Reason)) %>%
  filter(str_detect(Reason_lower, str_c(keywords_com_ht, collapse = "|"))) %>%
  group_by(Patient_UUID) %>%
  slice_min(order_by = VisitDate, with_ties = FALSE) %>%
  ungroup()

unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
filtered_com_ht <- filtered_com_ht %>%
  left_join(unique_dob, by = "Patient_UUID")



filtered_com_ht <- filtered_com_ht %>%
  mutate(
    visitDate = as.Date(VisitDate),
    DateOfBirth = as.Date(DateOfBirth),
    dx_age = time_length(interval(DateOfBirth, VisitDate), unit = "years")
  )


under_55_ht <- filtered_com_ht %>%
  filter(dx_age < 55) %>%
  distinct(Patient_UUID)%>%
  mutate (ht = 1)

# icc_analysis_5 <- eligible_analysis_CVD_55_S3_final_5 %>%
#   filter(Patient_UUID %in% BPV_55_CVD_S3_5BP_final$Patient_UUID)

BPV_55_CVD_5BP_med_com <- BPV_55_CVD_5BP_med %>%
  left_join(under_55_ht %>% select(Patient_UUID, ht), by = "Patient_UUID") %>%
  mutate(ht = if_else(is.na(ht), 0L, ht))

#####################################################
####### HT comorbidities hospital ###################

# keywords_com_ht_hsp <- c(
#   "hypertension", "Hypertensive heart", "chronic hypertension",
#   "Secondary hypertension", "Pre-existing essential hypertension", "Pre-existing hypertension", "antihypertensive"
# )

ht_com_hsp <- h_outcome %>%
  filter(DIAGNOSIS_CODE %in% c("I10", "I13.1")) %>%
  group_by(Patient_UUID) %>%
  slice_min(ENCOUNTER_DATE, with_ties = FALSE) %>%
  ungroup()


unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
ht_com_hsp <- ht_com_hsp %>%
  left_join(unique_dob, by = "Patient_UUID")



ht_com_hsp <- ht_com_hsp %>%
  mutate(
    ENCOUNTER_DATE = as.Date(ENCOUNTER_DATE),
    DateOfBirth = as.Date(DateOfBirth),
    dx_age = time_length(interval(DateOfBirth, ENCOUNTER_DATE), unit = "years")
  )


under_55_ht_hsp <- ht_com_hsp %>%
  filter(dx_age < 55) %>%
  distinct(Patient_UUID)%>%
  mutate (ht_hsp = 1)


BPV_55_CVD_5BP_med_com <- BPV_55_CVD_5BP_med_com %>%
  left_join(under_55_ht_hsp %>% select(Patient_UUID, ht_hsp), by = "Patient_UUID") %>%
  mutate(ht_hsp = if_else(is.na(ht_hsp), 0L, ht_hsp))


BPV_55_CVD_5BP_med_com <- BPV_55_CVD_5BP_med_com %>%
  mutate(com_ht = if_else(med_ht == 1 | ht == 1 | ht_hsp == 1, 1, 0))


############ DM comorbidity ###########################


keywords_com_dm <- c(
  "dm", "dm f/u", "dm with bp not to target", "iddm", "niddm", "t2dm", "type 1 dm", "diabetes"
)

filtered_com_dm <- gp_outcome_mix %>%
  mutate(Reason_lower = tolower(Reason)) %>%
  filter(str_detect(Reason_lower, str_c(keywords_com_dm, collapse = "|"))) %>%
  group_by(Patient_UUID) %>%
  slice_min(order_by = VisitDate, with_ties = FALSE) %>%
  ungroup()

unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
filtered_com_dm <- filtered_com_dm %>%
  left_join(unique_dob, by = "Patient_UUID")

filtered_com_dm <- filtered_com_dm %>%
  mutate(
    visitDate = as.Date(VisitDate),
    DateOfBirth = as.Date(DateOfBirth),
    dx_age_dm = time_length(interval(DateOfBirth, VisitDate), unit = "years")
  )


under_55_dm <- filtered_com_dm %>%
  filter(dx_age_dm < 55) %>%
  distinct(Patient_UUID)%>%
  mutate (dm = 1)


BPV_55_CVD_5BP_med_com <- BPV_55_CVD_5BP_med_com %>%
  left_join(under_55_dm %>% select(Patient_UUID, dm), by = "Patient_UUID") %>%
  mutate(dm = if_else(is.na(dm), 0L, dm))


keywords_com_dm_hsp <- c("diabetes")

filtered_com_dm_hsp <- h_outcome %>%
  mutate(DIAGNOSIS_DESCRIPTION_lower = tolower(DIAGNOSIS_DESCRIPTION)) %>%
  filter(str_detect(DIAGNOSIS_DESCRIPTION_lower, str_c(keywords_com_dm_hsp, collapse = "|"))) %>%
  group_by(Patient_UUID) %>%
  slice_min(order_by = ENCOUNTER_DATE, with_ties = FALSE) %>%
  ungroup()



keywords_dm_hsp <- c(
  "E11.64", "E11.11", "O24.42", "E10.11", "E11.73", "E11.9", "E11.51", "E11.72", "E11.52", "E11.22", "E11.29", "E10.65",
  "E13.64", "E11.02", "E10.73", "E11.65", "E11.01", "E11.42", "E10.64", "E10.22", "E11.41", "O24.43", "E11.39", "E13", "E11.40", "E10.9"
)

com_dm_hsp <- h_outcome %>%
  filter(DIAGNOSIS_CODE %in% keywords_dm_hsp)

com_dm_hsp <- com_dm_hsp %>%
  group_by(Patient_UUID) %>%
  summarize(earliest_VisitDate = min(ENCOUNTER_DATE, na.rm = TRUE), .groups = 'drop')

unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
com_dm_hsp <- com_dm_hsp %>%
  left_join(unique_dob, by = "Patient_UUID")

com_dm_hsp <- com_dm_hsp %>%
  mutate(
    earliest_VisitDate = as.Date(earliest_VisitDate),
    DateOfBirth = as.Date(DateOfBirth),
    dx_age_dm = time_length(interval(DateOfBirth, earliest_VisitDate), unit = "years")
  )


under_55_dm_hsp <- com_dm_hsp %>%
  filter(dx_age_dm < 55) %>%
  distinct(Patient_UUID)%>%
  mutate (dm_hsp = 1)


BPV_55_CVD_5BP_med_com <- BPV_55_CVD_5BP_med_com %>%
  left_join(under_55_dm_hsp %>% select(Patient_UUID, dm_hsp), by = "Patient_UUID") %>%
  mutate(dm_hsp = if_else(is.na(dm_hsp), 0L, dm_hsp))

BPV_55_CVD_5BP_med_com <- BPV_55_CVD_5BP_med_com %>%
  mutate(com_dm = if_else(med_dm == 1 | dm == 1 | dm_hsp == 1, 1, 0))


############## DISLIPIDEMIA COMORBIDITY ###############

keywords_com_lip <- c(
  "hyperlipidaemia", "dyslipidaemia", "unsatisfactory lipid"
)

filtered_com_lip <- gp_outcome_mix %>%
  mutate(Reason_lower = tolower(Reason)) %>%
  filter(str_detect(Reason_lower, str_c(keywords_com_lip, collapse = "|"))) %>%
  group_by(Patient_UUID) %>%
  slice_min(order_by = VisitDate, with_ties = FALSE) %>%
  ungroup()

unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
filtered_com_lip <- filtered_com_lip %>%
  left_join(unique_dob, by = "Patient_UUID")

filtered_com_lip <- filtered_com_lip %>%
  mutate(
    visitDate = as.Date(VisitDate),
    DateOfBirth = as.Date(DateOfBirth),
    dx_age_lip = time_length(interval(DateOfBirth, VisitDate), unit = "years")
  )


under_55_lip <- filtered_com_lip %>%
  filter(dx_age_lip < 55) %>%
  distinct(Patient_UUID)%>%
  mutate (lip = 1)


BPV_55_CVD_5BP_med_com <- BPV_55_CVD_5BP_med_com %>%
  left_join(under_55_lip %>% select(Patient_UUID, lip), by = "Patient_UUID") %>%
  mutate(lip = if_else(is.na(lip), 0L, lip))



keywords_lip_hsp <- c("E78.5")

com_lip_hsp <- h_outcome %>%
  filter(DIAGNOSIS_CODE %in% keywords_lip_hsp)

com_lip_hsp <- com_lip_hsp %>%
  group_by(Patient_UUID) %>%
  summarize(earliest_VisitDate = min(ENCOUNTER_DATE, na.rm = TRUE), .groups = 'drop')

unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
com_lip_hsp <- com_lip_hsp %>%
  left_join(unique_dob, by = "Patient_UUID")

com_lip_hsp <- com_lip_hsp %>%
  mutate(
    earliest_VisitDate = as.Date(earliest_VisitDate),
    DateOfBirth = as.Date(DateOfBirth),
    dx_age_lip = time_length(interval(DateOfBirth, earliest_VisitDate), unit = "years")
  )


under_55_lip_hsp <- com_lip_hsp %>%
  filter(dx_age_lip < 55) %>%
  distinct(Patient_UUID)%>%
  mutate (lip_hsp = 1)


BPV_55_CVD_5BP_med_com <- BPV_55_CVD_5BP_med_com %>%
  left_join(under_55_lip_hsp %>% select(Patient_UUID, lip_hsp), by = "Patient_UUID") %>%
  mutate(lip_hsp = if_else(is.na(lip_hsp), 0L, lip_hsp))

BPV_55_CVD_5BP_med_com <- BPV_55_CVD_5BP_med_com %>%
  mutate(com_lip = if_else(med_lipid == 1 | lip == 1 | lip_hsp == 1, 1, 0))



################### Smoking ################

distinct_RiskFactor <- RiskFactor %>%
  distinct (Patient_UUID)

# Perform the join
BPV_55_CVD_5BP_med_com_RF <- BPV_55_CVD_5BP_med_com %>%
  left_join(RiskFactor, by = "Patient_UUID")

BPV_55_CVD_5BP_med_com_RF %>%
  count(com_dm)

BPV_55_CVD_5BP_med_com_RF %>%
  count(com_ht)

BPV_55_CVD_5BP_med_com_RF %>%
  count(com_lip)

################### BMI ################

bmi <- bmi %>%
  mutate(ObservationDate = parse_date_time(ObservationDate, orders = c("dmy", "ymd", "mdy")))

unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
bmi <- bmi %>%
  left_join(unique_dob, by = "Patient_UUID")

bmi <- bmi %>%
  mutate(
    TestDate = as.Date(ObservationDate),
    DateOfBirth = as.Date(DateOfBirth),
    bmi_age = time_length(interval(DateOfBirth, ObservationDate), unit = "years")
  )

bmi <- bmi %>%
  select (-tbmi_age)

under_55_bmi <- bmi %>%
  filter(bmi_age < 55)


under_55_bmi_latest <- under_55_bmi %>%
  group_by(Patient_UUID) %>%
  slice_max(order_by = ObservationDate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Patient_UUID, ObservationDate, ObservationValue, bmi_age)


under_55_bmi_latest <- under_55_bmi_latest %>%
  rename (bmi = ObservationValue)


# Get intersecting Patient_UUIDs
intersecting_patients <- intersect(BPV_55_CVD_5BP_med_com_RF$Patient_UUID, under_55_bmi_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

under_55_bmi_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)


BPV_55_CVD_5BP_med_com_RF <- BPV_55_CVD_5BP_med_com_RF %>%
  left_join (under_55_bmi_latest%>% select(Patient_UUID, bmi), by = "Patient_UUID")

BPV_55_CVD_5BP_med_com_RF %>%
  filter(!is.na(bmi)) %>%
  summarise(count_with_bmi = n_distinct(Patient_UUID))

BPV_55_CVD_5BP_med_com_RF %>%
  filter(is.na(bmi)) %>%
  summarise(count_with_NA_bmi = n_distinct(Patient_UUID))

BPV_55_CVD_5BP_med_com_RF %>%
  filter(is.na(bmi)) %>%
  count(outcome)


################### FAMILY HISTORY ################


patient_FH <- FamilyHistory %>%
  filter(Patient_UUID %in% BPV_55_CVD_S3_5BP_final$Patient_UUID)

distinct.patient_FH <- patient_FH %>%
  distinct (Patient_UUID)


cvd_keywords <- "\\bihd\\b|\\bheart attack\\(s\\)\\b|\\bangina\\b|\\bami\\b|\\bcva\\b|\\bcoronary artery disease\\b|
                  \\bischaemic heart disease\\b|\\btia\\b|\\bcabg\\b|\\bstroke\\b|\\bMI\\b|\\bacute MI\\b|
                  \\bmyocardial infarction\\b|\\bcardiac arrest\\b|\\bcardiac\\b|\\bCAD\\b|\\bheart failure\\b|
                  \\bbypass\\b|\\bintracranial haemorrhage\\b|\\bheart disease\\b"

zero_keywords <- "\\bnil significant\\b|\\bnil sig\\b|\\bnil\\b|\\bno significant\\b"

patient_FH_dedup <- patient_FH_dedup %>%
  mutate(
    Reason_clean = ifelse(str_trim(Reason) == "", NA, str_trim(Reason)),  # empty -> NA
    FH = case_when(
      is.na(Reason_clean) ~ NA_real_,  # completely empty -> NA
      str_detect(Reason_clean, regex(cvd_keywords, ignore_case = TRUE)) ~ 1,  # CVD keywords -> 1
      TRUE ~ 0  # everything else (including "nil significant") -> 0
    )
  )

patient_FH_dedup <- patient_FH_dedup %>%
  select (-Reason_clean)

# Vector of Patient_UUIDs to reclassify
reclassify_ids <- c(
  "07AF5AF9-FE42-4505-BA2C-0FC57F3620E0",
  "15E33498-159A-4382-91CE-2521A75F3D67",
  "572EF0F8-8116-4E82-99E9-EB9B2613C7D5",
  "588F02C9-BB03-47EE-A453-DD19549EBF24",
  "5C9CBEA0-09A0-4B9F-832D-7521F43776DE",
  "5E607D89-8653-460E-A06D-8408AEF353E6",
  "6EBCA4F7-C3B5-47C9-A35F-0872991128A9",
  "7CC57C33-0D33-4DD8-84E0-C547D575A26C",
  "9FF2A739-7194-4928-AD61-8D7CFBED99C0",
  "B0007119-DDBB-4EF7-8DDB-8507F8E7102A",
  "BF5AEEA7-F2DD-466F-B4B6-610561E37CC7",
  "C21960E8-EAE7-4BF6-97B5-F1E4704283FF",
  "C36111C7-E9C4-4E51-B494-0853421DB8AA",
  "CF446CE5-8F81-4865-9D8D-2A81C2D1ADE7",
  "E8C7037D-D8CC-4E38-AE60-7530D5ED85A6",
  "EBC3A78F-B2B8-44F4-8CDB-93133A8B1AFB",
  "EBD0EBD9-C433-47FB-8414-DC91FE881E25",
  "032A980C-6129-4DE7-A47A-6E2663776B37",
  "9F0BCDEB-B64A-47DD-A6C4-070BF9B42381",
  "D5501675-C65A-40E1-9AF4-09C05468E181",
  "DFA6A203-6601-481A-9142-07A9AAD96BBF",
  "4E78E90C-A81A-4AAF-A500-29A5188C6C4B"
)

# Update FH to 1 for those Patient_UUIDs
patient_FH_dedup <- patient_FH_dedup %>%
  mutate(FH = if_else(Patient_UUID %in% reclassify_ids, 1, FH))

patient_FH_dedup <- patient_FH_dedup %>%
  select (-Reason)

BPV_55_CVD_5BP_med_com_RF_lab <- BPV_55_CVD_5BP_med_com_RF_lab %>%
  left_join(patient_FH_dedup, by = "Patient_UUID")


