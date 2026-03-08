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

lab <- lab %>%
  mutate(TestDate = parse_date_time(TestDate, orders = c("dmy", "ymd", "mdy")))


# Filter the data frame
filtered_lab <- lab %>%
  filter(grepl("\\b(chol|glucose|egfr|creat|hba1c|hdl|triglyceride|alb)\\b",
               LabTest, ignore.case = TRUE)) %>%
  select(Patient_UUID, LabTest, Value, UNITS, TestDate)

filtered_lab_55 <- filtered_lab %>%
  filter(Patient_UUID %in% BPV_55_med_com_RF$Patient_UUID)

distinct.filtered_lab_55 <- filtered_lab_55 %>%
  distinct(Patient_UUID)

processed_filtered_lab_55 <- filtered_lab_55 %>%
  mutate(
    # Combine specific LabTests under uACR
    uACR = ifelse(grepl("\\b(alb creat ratio|microalb/creat ratio|u-albumin/creat|r-u-protein/creat ratio)\\b",
                        LabTest, ignore.case = TRUE), Value, NA),

    # Combine specific LabTests under TC:HDL-C ratio
    `TC:HDL-C ratio` = ifelse(grepl("\\b(chol/hdl ratio|chol/hdl ratio \\(risk factor\\)|chol:hdl ratio|cholesterol.in hdl/cholesterol.total|cholesterol/hdl ratio|risk factor \\(chol/hdl\\)|total chol/hdl ratio)\\b",
                                    LabTest, ignore.case = TRUE), Value, NA),

    # Extract details for glucose, eGFR, and HbA1c
    glucose_Value = ifelse(grepl("\\bglucose\\b", LabTest, ignore.case = TRUE), Value, NA),
    glucose_UNITS = ifelse(grepl("\\bglucose\\b", LabTest, ignore.case = TRUE), UNITS, NA),
    glucose_TestDate = ifelse(grepl("\\bglucose\\b", LabTest, ignore.case = TRUE), TestDate, NA),

    egfr_Value = ifelse(grepl("\\begfr\\b", LabTest, ignore.case = TRUE), Value, NA),
    egfr_UNITS = ifelse(grepl("\\begfr\\b", LabTest, ignore.case = TRUE), UNITS, NA),
    egfr_TestDate = ifelse(grepl("\\begfr\\b", LabTest, ignore.case = TRUE), TestDate, NA),

    hba1c_Value = ifelse(grepl("\\bhba1c\\b", LabTest, ignore.case = TRUE), Value, NA),
    hba1c_UNITS = ifelse(grepl("\\bhba1c\\b", LabTest, ignore.case = TRUE), UNITS, NA),
    hba1c_TestDate = ifelse(grepl("\\bhba1c\\b", LabTest, ignore.case = TRUE), TestDate, NA)
  )



processed_filtered_lab_55 <- processed_filtered_lab_55 %>%
  filter(!str_detect(LabTest, regex("glucose load|urine glucose", ignore_case = TRUE)))

processed_filtered_lab_55 <- processed_filtered_lab_55 %>%
  mutate(
    TestDate = dmy(TestDate),
    glucose_TestDate = dmy(glucose_TestDate))


keywords_tc_hdlratio <- c(
    "chol/hdl ratio", "chol:hdl ratio", "cholesterol/hdl ratio", "risk factor (chol/hdl)", "total chol")


lab_55_tchdl <- lab %>%
  filter(str_detect(str_to_lower(LabTest),
                    paste0(str_to_lower(keywords_tc_hdlratio), collapse = "|")))


keywords_tc_hdlratio <- c(
  "chol(esterol)?(\\.|\\s|_)?(in)?(\\.|\\s|_)?hdl\\/(chol(esterol)?(\\.|\\s|_)?(total|t))?",
  "hdl\\/(chol(esterol)?(\\.|\\s|_)?(total|t))",
  "chol.*hdl.*ratio",
  "hdl.*chol.*ratio",
  "risk factor \\(chol/hdl\\)"
)

lab_55_tchdl <- lab %>%
  filter(str_detect(str_to_lower(LabTest),
                    regex(paste(keywords_tc_hdlratio, collapse = "|"), ignore_case = TRUE)))

unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
lab_55_tchdl <- lab_55_tchdl %>%
  left_join(unique_dob, by = "Patient_UUID")

lab_55_tchdl <- lab_55_tchdl %>%
  mutate(
    TestDate = as.Date(TestDate),
    DateOfBirth = as.Date(DateOfBirth),
    test_age_chdl_ratio = time_length(interval(DateOfBirth, TestDate), unit = "years")
  )

under_55_lab_tchdl <- lab_55_tchdl %>%
  filter(test_age_chdl_ratio < 55)


lab_55_tchdl_latest <- under_55_lab_tchdl %>%
  group_by(Patient_UUID) %>%
  filter(TestDate == max(TestDate, na.rm = TRUE)) %>%
  ungroup() %>%
  select(Patient_UUID, TestDate, LabTest, Value, test_age_chdl_ratio)

lab_55_tchdl_latest <- under_55_lab_tchdl %>%
  group_by(Patient_UUID) %>%
  slice_max(order_by = TestDate, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(Patient_UUID, TestDate, LabTest, Value, test_age_chdl_ratio)


lab_55_tchdl_latest <- lab_55_tchdl_latest %>%
  rename (ValueHDL = Value)


# Get intersecting Patient_UUIDs
intersecting_patients <- intersect(BPV_55_med_com$Patient_UUID, lab_55_tchdl_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

lab_55_tchdl_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)


BPV_55_med_com_RF_lab <- BPV_55_med_com_RF %>%
  left_join (lab_55_tchdl_latest%>% select(Patient_UUID, ValueHDL), by = "Patient_UUID")

BPV_55_med_com_RF_lab %>%
  filter(!is.na(ValueHDL)) %>%
  summarise(count_with_ValueHDL = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(ValueHDL)) %>%
  summarise(count_with_NA_ValueHDL = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(ValueHDL)) %>%
  count(outcome)

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
  filter(Patient_UUID %in% BPV_55_med_com_RF$Patient_UUID)


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
intersecting_patients <- intersect(BPV_55_med_com$Patient_UUID, lab_55_fglucose_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

lab_55_fglucose_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)

BPV_55_med_com_RF_lab <- BPV_55_med_com_RF_lab %>%
  select(-Value_fglucose.x, -Value_fglucose.y)

BPV_55_med_com_RF_lab <- BPV_55_med_com_RF_lab %>%
  left_join (lab_55_fglucose_latest%>% select(Patient_UUID, Value_fglucose), by = "Patient_UUID")

BPV_55_med_com_RF_lab %>%
  filter(!is.na(Value_fglucose)) %>%
  summarise(count_with_Value_fglucose = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(Value_fglucose)) %>%
  summarise(count_with_NA_Value_fglucose = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(Value_fglucose)) %>%
  count(outcome)


###########HbA1c##############

hba1c_keywords <- c("hba1c")

lab_hba1c <- lab %>%
  filter(str_detect(tolower(LabTest), str_c(hba1c_keywords, collapse = "|")))

lab_hba1c_55 <- lab_hba1c %>%
  filter(Patient_UUID %in% BPV_55_med_com_RF_lab$Patient_UUID)


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
intersecting_patients <- intersect(BPV_55_med_com_RF_lab$Patient_UUID, lab_55_hba1c_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

lab_55_fglucose_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)


BPV_55_med_com_RF_lab <- BPV_55_med_com_RF_lab %>%
  left_join (lab_55_hba1c_latest%>% select(Patient_UUID, hba1c), by = "Patient_UUID")

BPV_55_med_com_RF_lab %>%
  filter(!is.na(hba1c)) %>%
  summarise(count_with_hba1c = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(hba1c)) %>%
  summarise(count_with_NA_hba1c = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(hba1c)) %>%
  count(outcome)


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
  filter(Patient_UUID %in% BPV_55_med_com_RF_lab$Patient_UUID)


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
intersecting_patients <- intersect(BPV_55_med_com_RF_lab$Patient_UUID, lab_55_uacr_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

lab_55_uacr_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)


BPV_55_med_com_RF_lab <- BPV_55_med_com_RF_lab %>%
  left_join (lab_55_uacr_latest%>% select(Patient_UUID, uacr), by = "Patient_UUID")

BPV_55_med_com_RF_lab %>%
  filter(!is.na(uacr)) %>%
  summarise(count_with_uacr = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(uacr)) %>%
  summarise(count_with_NA_uacr = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(uacr)) %>%
  count(outcome)



#############egfr#############
egfr_keywords <- c("egfr")


lab_egfr <- lab %>%
  filter(str_detect(tolower(LabTest), str_c(egfr_keywords, collapse = "|")))

lab_egfr_55 <- lab_egfr %>%
  filter(Patient_UUID %in% BPV_55_med_com_RF_lab$Patient_UUID)


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
intersecting_patients <- intersect(BPV_55_med_com_RF_lab$Patient_UUID, lab_55_egfr_latest$Patient_UUID)

# Count how many
length(intersecting_patients)

lab_55_egfr_latest %>%
  count(Patient_UUID) %>%
  filter(n > 1)


BPV_55_med_com_RF_lab <- BPV_55_med_com_RF_lab %>%
  left_join (lab_55_egfr_latest%>% select(Patient_UUID, egfr), by = "Patient_UUID")

BPV_55_med_com_RF_lab %>%
  filter(!is.na(egfr)) %>%
  summarise(count_with_uacr = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(egfr)) %>%
  summarise(count_with_NA_uacr = n_distinct(Patient_UUID))

BPV_55_med_com_RF_lab %>%
  filter(is.na(egfr)) %>%
  count(outcome)












processed_lab_demo <- processed_lab_demo %>%
  mutate(
    uACR_numeric = suppressWarnings(as.numeric(uACR)) # Convert to numeric, ignoring warnings
  )



# Check for cases where the conversion produced NAs but `uACR` is not NA
problematic_uACR <- processed_lab_demo %>%
  filter(is.na(uACR_numeric) & !is.na(uACR)) # Identify unexpected cases
print(problematic_uACR)


# Ensure uACR is numeric
processed_lab_demo <- processed_lab_demo %>%
  mutate(
    uACR = as.numeric(uACR), # Convert uACR to numeric
    egfr_Value = as.numeric(egfr_Value) # Ensure egfr_Value is numeric
  )

# Filter clinically high-risk patients
clinicallyHR <- processed_lab_demo %>%
  filter(
    egfr_Value < 45 |
      (uACR > 25 & Gender == "Male") |
      (uACR > 35 & Gender == "Female")
  )



problematic_rows <- processed_lab_demo %>%
  filter(is.na(as.numeric(uACR)) & !is.na(uACR)) %>%
  select(Patient_UUID, uACR)

View(problematic_rows) # View these problematic rows

processed_lab_demo <- processed_lab_demo %>%
  mutate(
    uACR = case_when(
      str_detect(uACR, "^>") ~ as.numeric(str_replace(uACR, "^>", "")), # Handle values like ">25"
      str_detect(uACR, "^<") ~ as.numeric(str_replace(uACR, "^<", "")), # Handle values like "<25"
      TRUE ~ as.numeric(uACR) # Convert valid numeric strings
    )
  )


sum(is.na(processed_lab_demo$uACR)) # Count NAs in uACR

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



