bp_sd_less55_CVD <- bp_sd_less55_CVD_V2


# Matching with the outcome data
outcome_CVD_55_S3 <- length(intersect(bp_sd_less55_CVD$Patient_UUID, cleaned_outcome_55$Patient_UUID))

outcome_CVD_55_S3

bp_sd_CVD_55_S3_unique <- bp_sd_less55_CVD %>%
  distinct(Patient_UUID, .keep_all = TRUE)

cleaned_outcome_55_unique <- cleaned_outcome_55 %>%
  distinct(Patient_UUID, .keep_all = TRUE)

intersect_bp_outcome_55_S3 <- inner_join(
  bp_sd_CVD_55_S3_unique,
  cleaned_outcome_55_unique,
  by = "Patient_UUID"
)


# # Calculating the number of outcome
# outcome_CVD_55_S3 %>%
#   distinct(Patient_UUID, outcome) %>%
#   mutate(outcome_group = case_when(
#     is.na(outcome) ~ "NA",
#     outcome == 1 ~ "1",
#     outcome == 0 ~ "0"
#   )) %>%
#   count(outcome_group)

bp_outcome_CVD_55_S3 <- bp_sd_less55_CVD %>%
  filter(Patient_UUID %in% intersect_bp_outcome_55_S3$Patient_UUID)

# Filtering patients with BP >=5
patients_bp_outcome_CVD_55_S3_more4 <- bp_outcome_CVD_55_S3 %>%
  add_count(Patient_UUID) %>%
  filter(n >= 4)
distinct.patients_bp_outcome_CVD_55_S3_more4 <- patients_bp_outcome_CVD_55_S3_more4 %>%
  distinct(Patient_UUID)

patients_bp_outcome_CVD_55_S3_more4 <- patients_bp_outcome_CVD_55_S3_more4 %>%
  left_join(
    cleaned_outcome_55 %>%
      select(Patient_UUID, outcome) %>%
      distinct(Patient_UUID, .keep_all = TRUE),
    by = "Patient_UUID"
  )


# Calculating the number of outcome
patients_bp_outcome_CVD_55_S3_more4 %>%
  distinct(Patient_UUID, outcome) %>%
  mutate(outcome_group = case_when(
    is.na(outcome) ~ "NA",
    outcome == 1 ~ "1",
    outcome == 0 ~ "0"
  )) %>%
  count(outcome_group)

eligible_analysis_CVD_55_S3 <- patients_bp_outcome_CVD_55_S3_more4 %>%
  select (-n)

cleaned_outcome_55$outcome_date <- as.Date(cleaned_outcome_55$outcome_date, format = "%Y-%m-%d")

outcome_date_data <- cleaned_outcome_55 %>%
  select(Patient_UUID, outcome_age, outcome_date) %>%
  distinct(Patient_UUID, .keep_all = TRUE)




eligible_analysis_CVD_55_S3 <- eligible_analysis_CVD_55_S3 %>%
  left_join(outcome_date_data, by = "Patient_UUID")

# eligible_analysis_CVD_55_S3 <- eligible_analysis_CVD_55_S3 %>%
#   select (-outcome_age.x, -outcome_date.x, -outcome_age.y, -outcome_date.y)
# 
# colSums(is.na(eligible_analysis_CVD_55_S3[, c("outcome_date", "outcome_age")]))



############# check on Patient_UUID with BP before and after 55 #############

# Step 1: For each patient, count how many measurements at AgeOfMeasurement <= 55 and >= 55
patient_age_counts_55 <- bp_sd %>%
  group_by(Patient_UUID) %>%
  summarise(
    count_under_55 = sum(AgeAtMeasurement < 55, na.rm = TRUE),
    count_over_55 = sum(AgeAtMeasurement >= 55, na.rm = TRUE)
  )

number_of_patients_with_5_for55 <- patient_age_counts_55 %>%
  filter(count_under_55 == 5) %>%
  summarise(n = n())



summary_stats_patient_age_counts_55 <- patient_age_counts_55 %>%
  summarise(
    min_count55 = min(count_under_55, na.rm = TRUE),
    max_count55 = max(count_under_55, na.rm = TRUE)
  )


# Step 2: Filter patients who meet both conditions
eligible_patients_55 <- patient_age_counts_55 %>%
  filter(count_under_55 >= 4, count_over_55 >= 1) %>%
  pull(Patient_UUID)

# Step 3: Filter the original data frame for only those patients
filtered_bp_sd_4_for55 <- bp_sd %>%
  filter(Patient_UUID %in% eligible_patients_55)

distinct.filtered_bp_sd_4_for55 <- filtered_bp_sd_4_for55 %>%
  distinct(Patient_UUID)


intersect_4bp_outcome_55_CVD <- inner_join(
  cleaned_outcome_55,
  distinct.filtered_bp_sd_4_for55,
  by = "Patient_UUID"
)

distinct.intersect_4bp_outcome_55_CVD <- intersect_4bp_outcome_55_CVD %>%
  distinct(Patient_UUID)

# intersect_bp_outcome_55_CVD <- inner_join(
#   intersect_4bp_outcome_55_CVD,
#   distinct.filtered_bp_sd_4_for55,
#   by = "Patient_UUID"
# )

# All UUIDs from both data frames
all_ids_55 <- union(
  intersect_4bp_outcome_55_CVD$Patient_UUID,
  distinct.filtered_bp_sd_4_for55$Patient_UUID
)

# UUIDs that were in the inner join
intersect_ids_55 <- intersect_4bp_outcome_55_CVD$Patient_UUID

# Those excluded from the join
excluded_ids_55 <- setdiff(all_ids_55, intersect_ids_55)

excluded_rows_55 <- distinct.filtered_bp_sd_4_for55 %>%
  filter(Patient_UUID %in% excluded_ids_55)

not_in_intersect_55 <- inner_join(
  excluded_rows_55,
  outcome_1_cvd_final,
  by = "Patient_UUID"
)

anti_not_in_intersect_55 <- anti_join(
  excluded_rows_55,
  outcome_1_cvd_final,
  by = "Patient_UUID"
)

bp_anti_not_in_intersect_55 <- bp_sd %>%
  filter(Patient_UUID %in% anti_not_in_intersect_55$Patient_UUID)

bp_anti_not_in_intersect_55 <- bp_sd %>%
  filter(Patient_UUID %in% anti_not_in_intersect_55$Patient_UUID)



# summary_counts <- bp_anti_not_in_intersect_55 %>%
#   mutate(age_group = ifelse(AgeAtMeasurement < 55, "<55", ">=55")) %>%
#   group_by(Patient_UUID, age_group) %>%
#   summarise(count = n(), .groups = "drop") %>%
#   tidyr::pivot_wider(names_from = age_group, values_from = count, values_fill = 0)
# 
# summary_counts %>%
#   summarise(
#     min_under_55 = min(`<55`, na.rm = TRUE),
#     max_under_55 = max(`<55`, na.rm = TRUE),
#     min_55_and_over = min(`>=55`, na.rm = TRUE),
#     max_55_and_over = max(`>=55`, na.rm = TRUE)
#   )

########checking if patient in <55 and >=55 is listed in the patients_bp_outcome_CVD_55_S3_more4

intersect_out_55 <- inner_join(
  bp_anti_not_in_intersect_55,
  patients_bp_outcome_CVD_55_S3_more4,
  by = "Patient_UUID"
)


latest_obs_bp_anti_not_in_intersect_55 <- bp_anti_not_in_intersect_55 %>%
  group_by(Patient_UUID) %>%
  filter(ObservationDate == max(ObservationDate, na.rm = TRUE)) %>%
  select(Patient_UUID, ObservationDate, AgeAtMeasurement) %>%
  distinct()

latest_obs_bp_anti_not_in_intersect_55 <- latest_obs_bp_anti_not_in_intersect_55 %>%
  rename (outcome_date = ObservationDate)

eligible_analysis_55_add <- bp_anti_not_in_intersect_55 %>%
  mutate(outcome = 0)

eligible_analysis_55_add_final <- eligible_analysis_55_add %>%
  filter(AgeAtMeasurement < 55)

distinct_eligible_analysis_55_add_final <- eligible_analysis_55_add_final %>%
  distinct (Patient_UUID)

# latest_obs_bp_anti_not_in_intersect_55_filtered <- latest_obs_bp_anti_not_in_intersect_55 %>%
#   filter(Patient_UUID %in% eligible_analysis_55_add_final$Patient_UUID)


# latest_eligible_analysis_55_add_final <- eligible_analysis_55_add_final %>%
#   group_by(Patient_UUID) %>%
#   filter(ObservationDate == max(ObservationDate, na.rm = TRUE)) %>%
#   select(Patient_UUID, ObservationDate, AgeAtMeasurement) %>%
#   distinct()

latest_obs_bp_anti_not_in_intersect_55$outcome_date <- as.Date(latest_obs_bp_anti_not_in_intersect_55$outcome_date, format = "%Y-%m-%d")

ID_outcome_date <- latest_obs_bp_anti_not_in_intersect_55 %>%
  select (Patient_UUID, outcome_date)

eligible_analysis_55_add_final <- eligible_analysis_55_add_final %>% 
  left_join(ID_outcome_date, by = "Patient_UUID")

eligible_analysis_55_add_final <- eligible_analysis_55_add_final %>%
  mutate(
    outcome_date = as.Date(outcome_date),
    DateOfBirth = as.Date(DateOfBirth),
    outcome_age = ceiling(as.numeric(difftime(outcome_date, DateOfBirth, units = "days")) / 365.25)
  )



# eligible_analysis_CVD_55_S3_updated <- eligible_analysis_CVD_55_S3 %>%
#   select (-outcome_age.y, -outcome_date.y)
# 
# eligible_analysis_CVD_55_S3 <- eligible_analysis_CVD_55_S3_updated %>%
#   rename (outcome_age = outcome_age.x, outcome_date = outcome_date.x)

# Ensure all are Date format BEFORE binding
eligible_analysis_CVD_55_S3$ObservationDate <- as.Date(eligible_analysis_CVD_55_S3$ObservationDate)
eligible_analysis_CVD_55_S3$DateOfBirth <- as.Date(eligible_analysis_CVD_55_S3$DateOfBirth)

eligible_analysis_55_add_final$ObservationDate <- as.Date(eligible_analysis_55_add_final$ObservationDate)
eligible_analysis_55_add_final$DateOfBirth <- as.Date(eligible_analysis_55_add_final$DateOfBirth)


# Now bind safely
eligible_analysis_CVD_55_S3_final <- rbind(eligible_analysis_CVD_55_S3, eligible_analysis_55_add_final)

common_ids <- intersect(
  eligible_analysis_CVD_55_S3$Patient_UUID,
  eligible_analysis_55_add_final$Patient_UUID
)

length(common_ids)  # To see how many are common

distinct.eligible_analysis_CVD_55_S3_final <- eligible_analysis_CVD_55_S3_final %>%
  distinct (Patient_UUID)

# Optional: View the overlapping Patient_UUIDs
# common_ids_df <- data.frame(Patient_UUID = common_ids)

write.csv(eligible_analysis_CVD_55_S3_final, "eligible_analysis_CVD_55_S3_4_BP_final.csv", row.names = FALSE)

# Calculating the number of outcome
eligible_analysis_CVD_55_S3_final %>%
  distinct(Patient_UUID, outcome) %>%
  mutate(outcome_group = case_when(
    is.na(outcome) ~ "NA",
    outcome == 1 ~ "1",
    outcome == 0 ~ "0"
  )) %>%
  count(outcome_group)

na_outcome_ids <- eligible_analysis_CVD_55_S3_final %>%
  filter(is.na(DateOfBirth)) %>%
  pull(Patient_UUID)

# View the result
na_outcome_ids


########## Calculating the BPV ##############

library(dplyr)

# 1. Summary stats for Systolic (ObservationValue_S)
summary_S <- eligible_analysis_CVD_55_S3_final %>%
  group_by(Patient_UUID) %>%
  summarise(
    mean_S = mean(ObservationValue_S, na.rm = TRUE),
    sd_S = sd(ObservationValue_S, na.rm = TRUE),
    cv_S = (sd_S / mean_S) * 100,
    .groups = "drop"
  )

# 2. Summary stats for Diastolic (ObservationValue_D)
summary_D <- eligible_analysis_CVD_55_S3_final %>%
  group_by(Patient_UUID) %>%
  summarise(
    mean_D = mean(ObservationValue_D, na.rm = TRUE),
    sd_D = sd(ObservationValue_D, na.rm = TRUE),
    cv_D = (sd_D / mean_D) * 100,
    .groups = "drop"
  )

# 3. ARV for Systolic
arv_S <- eligible_analysis_CVD_55_S3_final %>%
  arrange(Patient_UUID, ObservationDate) %>%
  group_by(Patient_UUID) %>%
  mutate(
    B_S = abs(ObservationValue_S - lag(ObservationValue_S))
  ) %>%
  summarise(
    arv_S = sum(B_S, na.rm = TRUE) / (n() - 1),
    .groups = "drop"
  )

# 4. ARV for Diastolic
arv_D <- eligible_analysis_CVD_55_S3_final %>%
  arrange(Patient_UUID, ObservationDate) %>%
  group_by(Patient_UUID) %>%
  mutate(
    B_D = abs(ObservationValue_D - lag(ObservationValue_D))
  ) %>%
  summarise(
    arv_D = sum(B_D, na.rm = TRUE) / (n() - 1),
    .groups = "drop"
  )

# 5. Combine all results
bp_summary <- summary_S %>%
  left_join(summary_D, by = "Patient_UUID") %>%
  left_join(arv_S, by = "Patient_UUID") %>%
  left_join(arv_D, by = "Patient_UUID")

bp_summary <- bp_summary %>%
  mutate(
    mean_S = round(mean_S, 2),
    sd_S   = round(sd_S, 2),
    cv_S   = round(cv_S, 2),
    arv_S  = round(arv_S, 2),
    mean_D = round(mean_D, 2),
    sd_D   = round(sd_D, 2),
    cv_D   = round(cv_D, 2),
    arv_D  = round(arv_D, 2)
  )

data$DateOfBirth <- as.Date(data$DateOfBirth, format = "%Y-%m-%d")


unique_dob_gender <- data %>%
  select(Patient_UUID, DateOfBirth, Gender) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

eligible_analysis_CVD_55_S3_final$outcome_date <- as.Date(eligible_analysis_CVD_55_S3_final$outcome_date, format = "%Y-%m-%d")


unique_outcome_date <- eligible_analysis_CVD_55_S3_final %>%
  select(Patient_UUID, outcome_date, outcome) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
BPV_55_CVD_S3 <- bp_summary %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

BPV_55_CVD_S3 <- BPV_55_CVD_S3 %>%
  left_join(unique_outcome_date, by = "Patient_UUID")

BPV_55_CVD_S3 <- BPV_55_CVD_S3 %>%
  mutate(
    outcome_date = as.Date(outcome_date, format = "%Y-%m-%d"),  # adjust if format is different
    DateOfBirth = as.Date(DateOfBirth, format = "%Y-%m-%d"),    # same here
    date_turns_55 = DateOfBirth %m+% years(55),
    time_ms = ceiling(interval(date_turns_55, outcome_date) / months(1))
  )


BPV_55_neg_ms <- BPV_55_CVD_S3 %>%
  filter(time_ms < 0)

BPV_55_CVD_S3_final <- BPV_55_CVD_S3 %>%
  filter(time_ms > 0)

BPV_55_neg_ms <- BPV_55_neg_ms %>%
  filter(outcome == 1)


write.csv(BPV_55_CVD_S3_final, "BPV_55_CVD_S3_final_V1.csv", row.names = FALSE)
