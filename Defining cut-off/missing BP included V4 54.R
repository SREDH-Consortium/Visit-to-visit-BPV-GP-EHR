library(readr)
library(tidyr)
library(dplyr)
library(lubridate)

BP_missing <- read_csv("BP_unlinked_V2.csv", col_names = TRUE)

BP_missing <- BP_missing %>%
  mutate(across(starts_with("Date"), ~ dmy(.)))



# Create a new column for each Date field indicating if it's in correct yyyy-mm-dd format
BP_missing_check <- BP_missing %>%
  mutate(across(starts_with("Date"), ~ !grepl("^\\d{4}-\\d{2}-\\d{2}$", .), .names = "invalid_{.col}"))

# View rows where any date column is invalid
BP_missing_invalid_dates <- BP_missing_check %>%
  filter(if_any(starts_with("invalid_"), ~ . == TRUE))


# Step 1: Pivot systolic and diastolic together with a new column for observation type and values
long_bp_1 <- BP_missing %>%
  pivot_longer(
    cols = c(starts_with("Systolic"), starts_with("Diastolic")),
    names_to = "Observation",
    values_to = "ObservationValue"
  )


# Step 2: Extract the index number from the Observation name
long_bp_1 <- long_bp_1 %>%
  mutate(
    index = gsub("[^0-9]", "", Observation),
    Observation = gsub("[0-9]", "", Observation)
  )

# Step 3: Pivot the date columns and match dates to the corresponding observation
long_bp_1 <- long_bp_1 %>%
  pivot_longer(
    cols = starts_with("Date"),
    names_to = "DateCol",
    values_to = "ObservationDate"
  ) %>%
  mutate(date_index = gsub("[^0-9]", "", DateCol)) %>%
  filter(index == date_index) %>%   # Keep only matching date-observation pairs
  select(Patient_UUID, Observation, ObservationValue, ObservationDate)



wide_bp_1 <- long_bp_1 %>%
  mutate(Observation = ifelse(Observation == "Systolic", "ObservationValue_S", "ObservationValue_D")) %>%
  pivot_wider(
    names_from = Observation,
    values_from = ObservationValue
  )

unique_dob_gender <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
wide_bp_1 <- wide_bp_1 %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

wide_bp_1 <- wide_bp_1 %>%
  mutate(
    DateOfBirth = as.Date(DateOfBirth, format = "%Y-%m-%d")
  )

wide_bp_1 <- wide_bp_1 %>%
  mutate(
    AgeAtMeasurement = as.numeric(difftime(ObservationDate, DateOfBirth, units = "days")) / 365.25
  )

wide_bp_1 <- wide_bp_1 %>%
  select(-DateOfBirth.x, -DateOfBirth.y)

str(complete_bp_1$ObservationDate)

##############################################################################

BP_missing_V2 <- read_csv("BloodPressure_2006_2019_part3.csv", col_names = TRUE)

BP_missing_V2 <- BP_missing_V2 %>%
  mutate(across(starts_with("Date"), ~ dmy(na_if(trimws(.), ""))))


# Step 1: Pivot systolic and diastolic together with a new column for observation type and values
long_bp_0 <- BP_missing_V2 %>%
  pivot_longer(
    cols = c(starts_with("Systolic"), starts_with("Diastolic")),
    names_to = "Observation",
    values_to = "ObservationValue"
  )


# Step 2: Extract the index number from the Observation name
long_bp_0 <- long_bp_0 %>%
  mutate(
    index = gsub("[^0-9]", "", Observation),
    Observation = gsub("[0-9]", "", Observation)
  )

# Step 3: Pivot the date columns and match dates to the corresponding observation
long_bp_0 <- long_bp_0 %>%
  pivot_longer(
    cols = starts_with("Date"),
    names_to = "DateCol",
    values_to = "ObservationDate"
  ) %>%
  mutate(date_index = gsub("[^0-9]", "", DateCol)) %>%
  filter(index == date_index) %>%   # Keep only matching date-observation pairs
  select(Patient_UUID, Observation, ObservationValue, ObservationDate)


wide_bp_0 <- long_bp_0 %>%
  mutate(Observation = ifelse(Observation == "Systolic", "ObservationValue_S", "ObservationValue_D")) %>%
  pivot_wider(
    names_from = Observation,
    values_from = ObservationValue
  )

# distinct patients
distinct.wide_bp_0 <- wide_bp_0 %>%
  distinct(Patient_UUID)

unique_dob_gender <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
wide_bp_0 <- wide_bp_0 %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

#wide_bp_0 <- wide_bp_0 %>%
#  mutate(
#    ObservationDate = as.Date(ObservationDate, format = "%Y-%m-%d"),
#    DateOfBirth = as.Date(DateOfBirth, format = "%Y-%m-%d")
#  )

wide_bp_0 <- wide_bp_0 %>%
  mutate(
    AgeAtMeasurement = as.numeric(difftime(ObservationDate, DateOfBirth, units = "days")) / 365.25
  )

############################################

BP_missing_V3 <- read_csv("BloodPressure_4_JJ.csv", col_names = TRUE)

BP_missing_V3 <- BP_missing_V3 %>%
  filter(!is.na(Patient_UUID))

BP_missing_V3 <- BP_missing_V3 %>%
  mutate(across(starts_with("Date"), ~ dmy(na_if(trimws(.), ""))))


# Step 1: Pivot systolic and diastolic together with a new column for observation type and values
long_bp_1_add <- BP_missing_V3 %>%
  pivot_longer(
    cols = c(starts_with("Systolic"), starts_with("Diastolic")),
    names_to = "Observation",
    values_to = "ObservationValue"
  )


# Step 2: Extract the index number from the Observation name
long_bp_1_add <- long_bp_1_add %>%
  mutate(
    index = gsub("[^0-9]", "", Observation),
    Observation = gsub("[0-9]", "", Observation)
  )

# Step 3: Pivot the date columns and match dates to the corresponding observation
long_bp_1_add <- long_bp_1_add %>%
  pivot_longer(
    cols = starts_with("Date"),
    names_to = "DateCol",
    values_to = "ObservationDate"
  ) %>%
  mutate(date_index = gsub("[^0-9]", "", DateCol)) %>%
  filter(index == date_index) %>%   # Keep only matching date-observation pairs
  select(Patient_UUID, Observation, ObservationValue, ObservationDate)


wide_bp_1_add <- long_bp_1_add %>%
  mutate(Observation = ifelse(Observation == "Systolic", "ObservationValue_S", "ObservationValue_D")) %>%
  pivot_wider(
    names_from = Observation,
    values_from = ObservationValue
  )

# distinct patients
distinct.wide_bp_1_add <- wide_bp_1_add %>%
  distinct(Patient_UUID)

unique_dob_gender <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
wide_bp_1_add <- wide_bp_1_add %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

#wide_bp_1_add <- wide_bp_1_add %>%
#  mutate(
#    ObservationDate = as.Date(ObservationDate, format = "%Y-%m-%d"),
#    DateOfBirth = as.Date(DateOfBirth, format = "%Y-%m-%d")
#  )

wide_bp_1_add <- wide_bp_1_add %>%
  mutate(
    AgeAtMeasurement = as.numeric(difftime(ObservationDate, DateOfBirth, units = "days")) / 365.25
  )



complete_bp_1 <- rbind(bp_sd_V2, wide_bp_1)

complete_bp_2 <- rbind(complete_bp_1, wide_bp_0)

complete_bp_3 <- rbind(complete_bp_2, wide_bp_1_add)

complete_bp_3 %>%
  summarise(na_count = sum(is.na(AgeAtMeasurement)))


summary_stats_bp <- complete_bp_3 %>%
  summarise(
    min_S = min(ObservationValue_S, na.rm = TRUE),
    max_S = max(ObservationValue_S, na.rm = TRUE),
    min_D = min(ObservationValue_D, na.rm = TRUE),
    max_D = max(ObservationValue_D, na.rm = TRUE)
  )



complete_bp_3_clean <- complete_bp_3 %>%
  filter(
    ObservationValue_S >= 60,
    ObservationValue_S <= 250,
    ObservationValue_D >= 40,
    ObservationValue_D <= 140,
    ObservationValue_D <= ObservationValue_S
  )

# Rows that were removed
removed_rows <- complete_bp_3 %>%
  filter(
    ObservationValue_S < 60 |
      ObservationValue_S > 250 |
      ObservationValue_D < 40 |
      ObservationValue_D > 140 |
      ObservationValue_D > ObservationValue_S
  )

all(bp_sd_less55_for55$Patient_UUID %in% bp_sd_less55$Patient_UUID)
all(bp_sd_less55$Patient_UUID %in% bp_sd_less55_for55$Patient_UUID)

write.csv(bp_sd_less55, "bp_sd_less55", row.names = FALSE)

bp_sd <- complete_bp_3_clean
distinct.bp_sd <- bp_sd %>%
  distinct(Patient_UUID)

# Filtering Patients who have BP measurement less than 54 y.o
bp_sd_less54 <- bp_sd %>%
  filter(AgeAtMeasurement < 54)
distinct.bp_sd_less54 <- bp_sd_less54 %>%
  distinct(Patient_UUID)
distinct.bp_sd_less55 <- bp_sd_less55 %>%
  distinct(Patient_UUID)

# bp_sd_more55 <- bp_sd %>%
#   filter(AgeAtMeasurement > 55)
# distinct.bp_sd_more55 <- bp_sd_more55 %>%
#   distinct(Patient_UUID)

# Matching with the outcome data
n_outcome_LA54 <- length(intersect(bp_sd_less54$Patient_UUID, cleaned_outcome_54$Patient_UUID))

n_outcome_LA54

bp_sd_less54_unique <- bp_sd_less54 %>%
  distinct(Patient_UUID, .keep_all = TRUE)

complete_outcome_54_unique <- cleaned_outcome_54 %>%
  distinct(Patient_UUID, .keep_all = TRUE)

intersect_df_LA54 <- inner_join(
  bp_sd_less54_unique,
  complete_outcome_54_unique,
  by = "Patient_UUID"
)


# Calculating the number of outcome
intersect_df_LA54 %>%
  distinct(Patient_UUID, outcome) %>%
  mutate(outcome_group = case_when(
    is.na(outcome) ~ "NA",
    outcome == 1 ~ "1",
    outcome == 0 ~ "0"
  )) %>%
  count(outcome_group)

bp_sd_less55_inter_out_for55 <- bp_sd_less55_for55 %>%
  filter(Patient_UUID %in% intersect_df_LA55_for55$Patient_UUID)

# Filtering patients with BP >=5
patients_BP_more5_for55 <- bp_sd_less55_inter_out_for55 %>%
  add_count(Patient_UUID) %>%
  filter(n >= 5)
distinct.patients_BP_more5_for55 <- patients_BP_more5_for55 %>%
  distinct(Patient_UUID)

patients_BP_more5_outcome_for55 <- patients_BP_more5_for55 %>%
  left_join(
    complete_outcome_55 %>%
      select(Patient_UUID, outcome) %>%
      distinct(Patient_UUID, .keep_all = TRUE),
    by = "Patient_UUID"
  )


# Calculating the number of outcome
patients_BP_more5_outcome_for55 %>%
  distinct(Patient_UUID, outcome) %>%
  mutate(outcome_group = case_when(
    is.na(outcome) ~ "NA",
    outcome == 1 ~ "1",
    outcome == 0 ~ "0"
  )) %>%
  count(outcome_group)

eligible_analysis_55 <- patients_BP_more5_outcome_for55 %>%
  select (-n)

outcome_date_data <- complete_outcome_55 %>%
  select(Patient_UUID, outcome_age, outcome_date) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

eligible_analysis_55 <- eligible_analysis_55 %>%
  left_join(outcome_date_data, by = "Patient_UUID")

colSums(is.na(eligible_analysis_55[, c("outcome_date", "outcome_age")]))



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
  filter(count_under_55 >= 5, count_over_55 >= 1) %>%
  pull(Patient_UUID)

# Step 3: Filter the original data frame for only those patients
filtered_bp_sd_5_for55 <- bp_sd %>%
  filter(Patient_UUID %in% eligible_patients_55)

distinct.filtered_bp_sd_5_for55 <- filtered_bp_sd_5_for55 %>%
  distinct(Patient_UUID)


intersect_bp_outcome_55_5_for55 <- inner_join(
  cleaned_outcome_55,
  distinct.filtered_bp_sd_5_for55,
  by = "Patient_UUID"
)

distinct.intersect_bp_outcome_55_5_for55 <- intersect_bp_outcome_55_5_for55 %>%
  distinct(Patient_UUID)

intersect_bp_outcome_55_V2 <- inner_join(
  intersect_bp_outcome_55_5_for55,
  distinct.filtered_bp_sd_5_for55,
  by = "Patient_UUID"
)

# All UUIDs from both data frames
all_ids_55 <- union(
  intersect_bp_outcome_55_5_for55$Patient_UUID,
  distinct.filtered_bp_sd_5_for55$Patient_UUID
)

# UUIDs that were in the inner join
intersect_ids_55 <- intersect_bp_outcome_55_V2$Patient_UUID

# Those excluded from the join
excluded_ids_55 <- setdiff(all_ids_55, intersect_ids_55)

excluded_rows_55 <- distinct.filtered_bp_sd_5_for55 %>%
  filter(Patient_UUID %in% excluded_ids_55)

not_in_intersect_55 <- inner_join(
  excluded_rows_55,
  outcome_1,
  by = "Patient_UUID"
)

anti_not_in_intersect_55 <- anti_join(
  excluded_rows_55,
  outcome_1,
  by = "Patient_UUID"
)

bp_anti_not_in_intersect_55 <- bp_sd %>%
  filter(Patient_UUID %in% anti_not_in_intersect_55$Patient_UUID)



summary_counts <- bp_anti_not_in_intersect_55 %>%
  mutate(age_group = ifelse(AgeAtMeasurement < 55, "<55", ">=55")) %>%
  group_by(Patient_UUID, age_group) %>%
  summarise(count = n(), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = age_group, values_from = count, values_fill = 0)

summary_counts %>%
  summarise(
    min_under_55 = min(`<55`, na.rm = TRUE),
    max_under_55 = max(`<55`, na.rm = TRUE),
    min_55_and_over = min(`>=55`, na.rm = TRUE),
    max_55_and_over = max(`>=55`, na.rm = TRUE)
  )

intersect_out_55 <- inner_join(
  bp_anti_not_in_intersect_55,
  patients_BP_more5_outcome_for55,
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

distinct_eligible_analysis_55_add_final <- eligible_analysis_55_add_final

latest_obs_bp_anti_not_in_intersect_55_filtered <- latest_obs_bp_anti_not_in_intersect_55 %>%
  filter(Patient_UUID %in% eligible_analysis_55_add_final$Patient_UUID)

latest_obs_bp_anti_not_in_intersect_55 <- bp_anti_not_in_intersect_55 %>%
  group_by(Patient_UUID) %>%
  filter(ObservationDate == max(ObservationDate, na.rm = TRUE)) %>%
  select(Patient_UUID, ObservationDate, AgeAtMeasurement) %>%
  distinct()

latest_eligible_analysis_55_add_final <- eligible_analysis_55_add_final %>%
  group_by(Patient_UUID) %>%
  filter(ObservationDate == max(ObservationDate, na.rm = TRUE)) %>%
  select(Patient_UUID, ObservationDate, AgeAtMeasurement) %>%
  distinct()

latest_55_demographics <- latest_obs_bp_anti_not_in_intersect_55_filtered

# Ensure all are Date format BEFORE binding
eligible_analysis_55$ObservationDate <- as.Date(eligible_analysis_55$ObservationDate)
eligible_analysis_55$DateOfBirth <- as.Date(eligible_analysis_55$DateOfBirth)

eligible_analysis_55_add_final$ObservationDate <- as.Date(eligible_analysis_55_add_final$ObservationDate)
eligible_analysis_55_add_final$DateOfBirth <- as.Date(eligible_analysis_55_add_final$DateOfBirth)


# Now bind safely
eligible_analysis_55_final <- rbind(eligible_analysis_55, eligible_analysis_55_add_final)

common_ids <- intersect(
  eligible_analysis_55$Patient_UUID,
  eligible_analysis_55_add$Patient_UUID
)

length(common_ids)  # To see how many are common

# Optional: View the overlapping Patient_UUIDs
common_ids_df <- data.frame(Patient_UUID = common_ids)

write.csv(eligible_analysis_55_final, "eligible_analysis_55_final", row.names = FALSE)

#################################

# Find Patient_UUIDs in distinct.filtered_bp_sd but not in distinct.patients_BP_more5
patients_not_in_all_55_56_for55 <- distinct.filtered_bp_sd_5_for55 %>%
  filter(!(Patient_UUID %in% distinct.patients_BP_more5_for55$Patient_UUID))

# Filter h_outcome by Patient_UUIDs listed in patients_not_in_all_55_56
h_outcome_selected <- latest.VisitDate.no.CVD.h.56 %>%
  filter(Patient_UUID %in% patients_not_in_all_55_56$Patient_UUID)

gp_outcome_mix_selected <- latest.VisitDate.no.CVD.gp.56 %>%
  filter(Patient_UUID %in% patients_not_in_all_55_56$Patient_UUID)

outcome_1_selected <- outcome_1 %>%
  filter(Patient_UUID %in% patients_not_in_all_55_56$Patient_UUID)


########## Calculating the BPV ##############

library(dplyr)

# 1. Summary stats for Systolic (ObservationValue_S)
summary_S <- eligible_analysis_55_final %>%
  group_by(Patient_UUID) %>%
  summarise(
    mean_S = mean(ObservationValue_S, na.rm = TRUE),
    sd_S = sd(ObservationValue_S, na.rm = TRUE),
    cv_S = (sd_S / mean_S) * 100,
    .groups = "drop"
  )

# 2. Summary stats for Diastolic (ObservationValue_D)
summary_D <- eligible_analysis_55_final %>%
  group_by(Patient_UUID) %>%
  summarise(
    mean_D = mean(ObservationValue_D, na.rm = TRUE),
    sd_D = sd(ObservationValue_D, na.rm = TRUE),
    cv_D = (sd_D / mean_D) * 100,
    .groups = "drop"
  )

# 3. ARV for Systolic
arv_S <- eligible_analysis_55_final %>%
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
arv_D <- eligible_analysis_55_final %>%
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

unique_dob_gender <- data %>%
  select(Patient_UUID, DateOfBirth, Gender) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
BPV_55 <- bp_summary %>%
  left_join(unique_dob_gender, by = "Patient_UUID")



patients_with_all_na_dates <- eligible_analysis_55_final %>%
  group_by(Patient_UUID) %>%
  summarise(all_na = all(is.na(ObservationDate))) %>%
  filter(all_na) %>%
  select(Patient_UUID)

eligible_analysis_55_na_date <- eligible_analysis_55_final %>%
  filter(Patient_UUID %in% patients_with_all_na_dates$Patient_UUID)

na_date_details <- bp_sd_less55 %>%
  semi_join(patients_with_all_na_dates, by = "Patient_UUID") %>%
  select(Patient_UUID, ObservationDate, DateOfBirth, ObservationValue_S, ObservationValue_D)


eligible_analysis_55_final_updated <- eligible_analysis_55_final %>%
  left_join(
    na_date_details,
    by = c("Patient_UUID", "ObservationValue_S", "ObservationValue_D"),
    suffix = c("", "_new")
  ) %>%
  mutate(
    ObservationDate = if_else(is.na(ObservationDate), ObservationDate_new, ObservationDate),
    DateOfBirth = if_else(is.na(DateOfBirth), DateOfBirth_new, DateOfBirth)
  ) %>%
  select(-ObservationDate_new, -DateOfBirth_new)


observation_date_range_55 <- eligible_analysis_55_final_updated %>%
  group_by(Patient_UUID) %>%
  summarise(
    earliest_ObservationDate = min(ObservationDate, na.rm = TRUE),
    latest_ObservationDate = max(ObservationDate, na.rm = TRUE)
  )

# Perform the join
BPV_55 <- BPV_55 %>%
  left_join(observation_date_range_55, by = "Patient_UUID")


outcome_date_data <- complete_outcome_55 %>%
  select(Patient_UUID, outcome, outcome_date) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

latest_55_demographics <- latest_55_demographics %>%
  select (-AgeAtMeasurement)

latest_55_demographics <- latest_55_demographics %>%
  mutate (outcome = 0)

# Ensure all are Date format BEFORE binding
latest_55_demographics$outcome_date <- as.Date(latest_55_demographics$outcome_date)
outcome_date_data$outcome_date <- as.Date(outcome_date_data$outcome_date)

# Now bind safely
outcome_date_final <- rbind(latest_55_demographics, outcome_date_data)

BPV_55 <- BPV_55 %>%
  left_join(outcome_date_final, by = "Patient_UUID")

colSums(is.na(BPV_55[, c("outcome_date", "BP_obs", "outcome_obs", "age_at_first_obs", "age_at_last_obs", "outcome_age", "outcome")]))

BPV_55 <- BPV_55 %>%
  select (-outcome_date.x, -outcome_date.y, -outcome.x)

BPV_55 <- BPV_55 %>%
  rename (outcome = outcome.y)

BPV_55 <- BPV_55 %>%
  mutate(
    BP_obs = ceiling (as.numeric(difftime(latest_ObservationDate, earliest_ObservationDate, units = "days")) / 365.25),
    outcome_obs = ceiling (as.numeric(difftime(outcome_date, latest_ObservationDate ,units = "days")) / 365.25),
    age_at_first_obs = ceiling (as.numeric(difftime(earliest_ObservationDate, DateOfBirth, units = "days")) / 365.25),
    age_at_last_obs = ceiling (as.numeric(difftime(latest_ObservationDate, DateOfBirth, units = "days")) / 365.25),
    outcome_age = ceiling (as.numeric(difftime(outcome_date, DateOfBirth, units = "days")) / 365.25)
  )

BPV_55 <- BPV_55 %>%
  mutate(
    outcome_obs_days = as.numeric(difftime(outcome_date, DateOfBirth + years(55), units = "weeks")))



BPV_55 <- BPV_55 %>%
  mutate(
    year_turns_55 = year(DateOfBirth) + 55,
    jan1_turns_55 = as.Date(paste0(year_turns_55, "-01-01")),
    outcome_obs_days = as.numeric(difftime(outcome_date, jan1_turns_55, units = "days"))
  )


BPV_55_negative_outcome <- BPV_55 %>%
  filter(outcome_obs_days < 0)

BPV_55_negative_outcomeobs <- BPV_55 %>%
  filter(outcome_obs <= 0)




BPV_55 %>% filter(outcome_obs_days == 0)

write.csv(BPV_55, "BPV 55 final_V2.csv", row.names = FALSE)

# Variables to summarize
vars_to_summarize <- c("mean_S", "sd_S", "cv_S", "arv_S", "mean_D", "sd_D", "cv_D", "arv_D")

# Summary statistics
summary_stats_BPV_55 <- BPV_55 %>%
  summarise(across(all_of(vars_to_summarize),
                   list(mean = ~mean(., na.rm = TRUE),
                        sd   = ~sd(., na.rm = TRUE),
                        min  = ~min(., na.rm = TRUE),
                        max  = ~max(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# View the result
print(summary_stats_BPV_55)


# Variables to summarize
vars_to_summarize_obs <- c("BP_obs", "outcome_obs", "age_at_first_obs", "age_at_last_obs", "outcome_age")

# Summary statistics
summary_stats_obs_duration <- BPV_55 %>%
  summarise(across(all_of(vars_to_summarize_obs),
                   list(mean = ~mean(., na.rm = TRUE),
                        sd   = ~sd(., na.rm = TRUE),
                        min  = ~min(., na.rm = TRUE),
                        max  = ~max(., na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# View the result
print(summary_stats_obs_duration)


# Filter rows where BP_obs is 0
bp_obs_zero <- BPV_55 %>%
  filter(BP_obs == 0)

# View the result
print(bp_obs_zero)

# Optional: Count how many rows have BP_obs == 0
n_zero <- nrow(bp_obs_zero)
cat("Number of rows with BP_obs == 0:", n_zero, "\n")


# Filter rows where BP_obs is 0
bp_sd_zero <- BPV_55 %>%
  filter(sd_S == 0)

# View the result
print(bp_obs_zero)

# Optional: Count how many rows have BP_obs == 0
n_zero <- nrow(bp_sd_zero)
cat("Number of rows with sd == 0:", n_zero, "\n")


