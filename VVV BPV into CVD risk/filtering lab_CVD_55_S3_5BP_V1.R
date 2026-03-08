library("dplyr")
library("lubridate")
library("eeptools")
library("data.table")
library("stringr")
library("writexl")
library("xlsx")
library(stringr)


data <- `Demographics_BloodPRessure`
lab <- LabValues

lab$TestDate <- as.Date(lab$TestDate, format = "%Y-%m-%d")


###########FBG##############

keywords_fasting_glucose <- c(
  "\\bfasting(\\s|,|\\(|\\)|_)?glucose\\b",
  "\\bglucose(\\s|,|\\(|\\)|_)?fasting\\b",
  "\\bglucose(\\s|,|\\(|\\)|_)?plasma(\\s|,|\\(|\\)|_)?fasting\\b",
  "\\bplasma(\\s|,|\\(|\\)|_)?fasting(\\s|,|\\(|\\)|_)?glucose\\b"
)

lab_fglucose <- lab %>%
  filter(str_detect(str_to_lower(LabTest),
                    regex(paste(keywords_fasting_glucose, collapse = "|"), ignore_case = TRUE)))

lab_fglucose_55 <- lab_fglucose %>%
  filter(Patient_UUID %in% BPV_55_CVD_5BP_med_com_RF$Patient_UUID)


unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
lab_fglucose_55 <- lab_fglucose_55 %>%
  left_join(unique_dob, by = "Patient_UUID")

lab_fglucose_55 <- lab_fglucose_55 %>%
  mutate(
    TestDate = as.Date(TestDate),
    DateOfBirth = as.Date(DateOfBirth),
    test_age_fglucose = time_length(interval(DateOfBirth, TestDate), unit = "years")
  )

under_55_lab_fglucose <- lab_fglucose_55 %>%
  filter(test_age_fglucose < 55)


lab_55_fglucose_latest <- under_55_lab_fglucose %>%
  group_by(Patient_UUID) %>%
  filter(TestDate == max(TestDate, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Patient_UUID, TestDate, LabTest, Value, test_age_fglucose)

lab_55_fglucose_latest <- under_55_lab_fglucose %>%
  group_by(Patient_UUID) %>%
  slice_max(order_by = TestDate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Patient_UUID, TestDate, LabTest, Value, test_age_fglucose)


lab_55_fglucose_latest <- lab_55_fglucose_latest %>%
  rename (Value_fglucose = Value)


# Get intersecting Patient_UUIDs
intersecting_patients <- intersect(BPV_55_CVD_5BP_med_com_RF$Patient_UUID, lab_55_fglucose_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

lab_55_fglucose_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)

BPV_55_CVD_5BP_med_com_RF_lab <- BPV_55_CVD_5BP_med_com_RF %>%
  left_join (lab_55_fglucose_latest%>% select(Patient_UUID, Value_fglucose), by = "Patient_UUID")

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(is.na(Value_fglucose)) %>%
  summarise(count_with_NA_Value_fglucose = n_distinct(Patient_UUID))

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(is.na(Value_fglucose)) %>%
  count(outcome)


###########HbA1c##############

hba1c_keywords <- c("hba1c")

lab_hba1c <- lab %>%
  filter(str_detect(tolower(LabTest), str_c(hba1c_keywords, collapse = "|")))

lab_hba1c_55 <- lab_hba1c %>%
  filter(Patient_UUID %in% BPV_55_CVD_5BP_med_com_RF$Patient_UUID)


unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
lab_hba1c_55 <- lab_hba1c_55 %>%
  left_join(unique_dob, by = "Patient_UUID")

lab_hba1c_55 <- lab_hba1c_55 %>%
  mutate(
    TestDate = as.Date(TestDate),
    DateOfBirth = as.Date(DateOfBirth),
    test_age_hba1c = time_length(interval(DateOfBirth, TestDate), unit = "years")
  )

under_55_lab_hba1c <- lab_hba1c_55 %>%
  filter(test_age_hba1c < 55)


lab_55_hba1c_latest <- under_55_lab_hba1c %>%
  group_by(Patient_UUID) %>%
  filter(TestDate == max(TestDate, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Patient_UUID, TestDate, LabTest, Value, test_age_hba1c)

lab_55_hba1c_latest <- under_55_lab_hba1c %>%
  group_by(Patient_UUID) %>%
  slice_max(order_by = TestDate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Patient_UUID, TestDate, LabTest, Value, test_age_hba1c)


lab_55_hba1c_latest <- lab_55_hba1c_latest %>%
  rename (hba1c = Value)


# Get intersecting Patient_UUIDs
intersecting_patients <- intersect(BPV_55_CVD_5BP_med_com_RF$Patient_UUID, lab_55_hba1c_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

lab_55_hba1c_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)


BPV_55_CVD_5BP_med_com_RF_lab <- BPV_55_CVD_5BP_med_com_RF_lab %>%
  left_join (lab_55_hba1c_latest%>% select(Patient_UUID, hba1c), by = "Patient_UUID")

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(!is.na(hba1c)) %>%
  summarise(count_with_hba1c = n_distinct(Patient_UUID))

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(is.na(hba1c)) %>%
  summarise(count_with_NA_hba1c = n_distinct(Patient_UUID))

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(is.na(hba1c)) %>%
  count(com_dm)


#############uACR#############
uacr_keywords <- c(
  "alb creat ratio",
  "alb/creat (urine)",
  "albumin : creat ratio",
  "microalb/creat ratio",
  "r u-albumin/creat"
)


lab_uacr <- lab %>%
  filter(str_detect(tolower(LabTest), str_c(uacr_keywords, collapse = "|")))

lab_uacr_55 <- lab_uacr %>%
  filter(Patient_UUID %in% BPV_55_CVD_5BP_med_com_RF$Patient_UUID)


unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
lab_uacr_55 <- lab_uacr_55 %>%
  left_join(unique_dob, by = "Patient_UUID")

lab_uacr_55 <- lab_uacr_55 %>%
  mutate(
    TestDate = as.Date(TestDate),
    DateOfBirth = as.Date(DateOfBirth),
    test_age_uacr = time_length(interval(DateOfBirth, TestDate), unit = "years")
  )

under_55_lab_uacr <- lab_uacr_55 %>%
  filter(test_age_uacr < 55)


lab_55_uacr_latest <- under_55_lab_uacr %>%
  group_by(Patient_UUID) %>%
  slice_max(order_by = TestDate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Patient_UUID, TestDate, LabTest, Value, test_age_uacr)


lab_55_uacr_latest <- lab_55_uacr_latest %>%
  rename (uacr = Value)


# Get intersecting Patient_UUIDs
intersecting_patients <- intersect(BPV_55_CVD_5BP_med_com_RF_lab$Patient_UUID, lab_55_uacr_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

lab_55_uacr_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)


BPV_55_CVD_5BP_med_com_RF_lab <- BPV_55_CVD_5BP_med_com_RF_lab %>%
  left_join (lab_55_uacr_latest%>% select(Patient_UUID, uacr), by = "Patient_UUID")

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(!is.na(uacr)) %>%
  summarise(count_with_uacr = n_distinct(Patient_UUID))

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(is.na(uacr)) %>%
  summarise(count_with_NA_uacr = n_distinct(Patient_UUID))

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(is.na(uacr)) %>%
  count(outcome)



#############egfr#############
egfr_keywords <- c("egfr")


lab_egfr <- lab %>%
  filter(str_detect(tolower(LabTest), str_c(egfr_keywords, collapse = "|")))

lab_egfr_55 <- lab_egfr %>%
  filter(Patient_UUID %in% BPV_55_CVD_5BP_med_com_RF$Patient_UUID)


unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
lab_egfr_55 <- lab_egfr_55 %>%
  left_join(unique_dob, by = "Patient_UUID")

lab_egfr_55 <- lab_egfr_55 %>%
  mutate(
    TestDate = as.Date(TestDate),
    DateOfBirth = as.Date(DateOfBirth),
    test_age_egfr = time_length(interval(DateOfBirth, TestDate), unit = "years")
  )

under_55_lab_egfr <- lab_egfr_55 %>%
  filter(test_age_egfr < 55)


lab_55_egfr_latest <- under_55_lab_egfr %>%
  group_by(Patient_UUID) %>%
  slice_max(order_by = TestDate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Patient_UUID, TestDate, LabTest, Value, test_age_egfr)


lab_55_egfr_latest <- lab_55_egfr_latest %>%
  rename (egfr = Value)


# Get intersecting Patient_UUIDs
intersecting_patients <- intersect(BPV_55_CVD_5BP_med_com_RF_lab$Patient_UUID, lab_55_egfr_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

lab_55_egfr_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)


BPV_55_CVD_5BP_med_com_RF_lab <- BPV_55_CVD_5BP_med_com_RF_lab %>%
  left_join (lab_55_egfr_latest%>% select(Patient_UUID, egfr), by = "Patient_UUID")

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(!is.na(egfr)) %>%
  summarise(count_with_egfr = n_distinct(Patient_UUID))

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(is.na(egfr)) %>%
  summarise(count_with_NA_egfr = n_distinct(Patient_UUID))

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(is.na(egfr)) %>%
  count(outcome)


############TC:HDL ratio####################

keywords_tc_hdlratio <- c(
  "chol(esterol)?(\\.|\\s|_)?(in)?(\\.|\\s|_)?hdl\\/(chol(esterol)?(\\.|\\s|_)?(total|t))?",
  "hdl\\/(chol(esterol)?(\\.|\\s|_)?(total|t))",
  "chol.*hdl.*ratio",
  "hdl.*chol.*ratio",
  "risk factor \\(chol/hdl\\)"
)

lab_tc_hdl <- lab %>%
  filter(str_detect(tolower(LabTest), str_c(keywords_tc_hdlratio, collapse = "|")))

lab_tc_hdl_55 <- lab_tc_hdl %>%
  filter(Patient_UUID %in% BPV_55_CVD_5BP_med_com_RF$Patient_UUID)


unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
lab_tc_hdl_55 <- lab_tc_hdl_55 %>%
  left_join(unique_dob, by = "Patient_UUID")

lab_tc_hdl_55 <- lab_tc_hdl_55 %>%
  mutate(
    TestDate = as.Date(TestDate),
    DateOfBirth = as.Date(DateOfBirth),
    test_age_chdl_ratio = time_length(interval(DateOfBirth, TestDate), unit = "years")
  )

under_55_lab_tchdl <- lab_tc_hdl_55 %>%
  filter(test_age_chdl_ratio < 55)


lab_tc_hdl_55_latest <- under_55_lab_tchdl %>%
  group_by(Patient_UUID) %>%
  filter(TestDate == max(TestDate, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Patient_UUID, TestDate, LabTest, Value, test_age_chdl_ratio)

lab_tc_hdl_55_latest <- under_55_lab_tchdl %>%
  group_by(Patient_UUID) %>%
  slice_max(order_by = TestDate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Patient_UUID, TestDate, LabTest, Value, test_age_chdl_ratio)

write.csv(lab_tc_hdl_55_latest, "lab_tc_hdl_55_latest.csv", row.names = FALSE)
write.csv(BPV_55_CVD_5BP_med_com_RF_lab, "BPV_S3_CVD_55_5BP.csv", row.names = FALSE)


lab_tc_hdl_55_latest <- lab_tc_hdl_55_latest %>%
  rename (Value_TCHDL = Value)


# Get intersecting Patient_UUIDs
intersecting_patients <- intersect(BPV_55_CVD_5BP_med_com_RF$Patient_UUID, lab_tc_hdl_55_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

lab_tc_hdl_55_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)


BPV_55_CVD_5BP_med_com_RF_lab <- BPV_55_CVD_5BP_med_com_RF_lab %>%
  left_join (lab_tc_hdl_55_latest%>% select(Patient_UUID, Value_TCHDL), by = "Patient_UUID")

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(!is.na(Value_TCHDL)) %>%
  summarise(count_with_Value_TCHDL = n_distinct(Patient_UUID))

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(is.na(Value_TCHDL)) %>%
  summarise(count_with_NA_Value_TCHDL = n_distinct(Patient_UUID))

BPV_55_CVD_5BP_med_com_RF_lab %>%
  filter(is.na(Value_TCHDL)) %>%
  count(outcome)









clinicallyHR <- processed_lab_demo %>%
  filter(
    egfr_Value < 45 |
      (uACR > 25 & Gender == "Male") |
      (uACR > 35 & Gender == "Female")
  )

clinicallyHR %>%
  filter(
    (Gender == "Male" & uACR <= 25) |
      (Gender == "Female" & uACR <= 35)
  )

distinct.clinicallyHR <- clinicallyHR %>%
  distinct(Patient_UUID)



