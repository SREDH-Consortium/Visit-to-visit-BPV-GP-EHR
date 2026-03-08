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

reason_list <- com %>%
  distinct(Reason)

write_xlsx(reason_list, "distinct_reasons.xlsx")


reason_list <- h_outcome %>%
  distinct(DIAGNOSIS_DESCRIPTION)

write_xlsx(reason_list, "distinct_reasons_HOSPITAL.xlsx")

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


BPV_55_med_com <- BPV_55_med %>%
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


BPV_55_med_com <- BPV_55_med_com %>%
  left_join(under_55_ht_hsp %>% select(Patient_UUID, ht_hsp), by = "Patient_UUID") %>%
  mutate(ht_hsp = if_else(is.na(ht_hsp), 0L, ht_hsp))


BPV_55_med_com <- BPV_55_med_com %>%
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


BPV_55_med_com <- BPV_55_med_com %>%
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


BPV_55_med_com <- BPV_55_med_com %>%
  left_join(under_55_dm_hsp %>% select(Patient_UUID, dm_hsp), by = "Patient_UUID") %>%
  mutate(dm_hsp = if_else(is.na(dm_hsp), 0L, dm_hsp))

BPV_55_med_com <- BPV_55_med_com %>%
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


BPV_55_med_com <- BPV_55_med_com %>%
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


BPV_55_med_com <- BPV_55_med_com %>%
  left_join(under_55_lip_hsp %>% select(Patient_UUID, lip_hsp), by = "Patient_UUID") %>%
  mutate(lip_hsp = if_else(is.na(lip_hsp), 0L, lip_hsp))

BPV_55_med_com <- BPV_55_med_com %>%
  mutate(com_lip = if_else(med_lipid == 1 | lip == 1 | lip_hsp == 1, 1, 0))



################### Smoking ################

distinct_RiskFactor <- RiskFactor %>%
  distinct (Patient_UUID)

# Perform the join
BPV_55_med_com_RF <- BPV_55_med_com %>%
  left_join(RiskFactor, by = "Patient_UUID")

BPV_55_med_com_RF_lab %>%
  count(com_dm)

BPV_55_med_com_RF_lab %>%
  count(com_ht)

BPV_55_med_com_RF_lab %>%
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
intersecting_patients <- intersect(BPV_55_med_com_RF_lab$Patient_UUID, under_55_bmi_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

under_55_bmi_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)


BPV_55_med_com_RF_lab <- BPV_55_med_com_RF_lab %>%
  left_join (under_55_bmi_latest%>% select(Patient_UUID, bmi), by = "Patient_UUID")

BPV_55_med_com_RF_lab %>%
  filter(!is.na(bmi)) %>%
  summarise(count_with_bmi = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(bmi)) %>%
  summarise(count_with_NA_bmi = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(bmi)) %>%
  count(outcome)


# Perform the join
BPV_55_med_com_RF <- BPV_55_med_com %>%
  left_join(RiskFactor, by = "Patient_UUID")

BPV_55_med_com_RF_lab %>%
  count(com_dm)

BPV_55_med_com_RF_lab %>%
  count(com_ht)

BPV_55_med_com_RF_lab %>%
  count(com_lip)
