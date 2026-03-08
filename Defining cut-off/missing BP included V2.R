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


# Ensure all are Date format BEFORE binding
bp_sd_V2$ObservationDate <- as.Date(bp_sd_V2$ObservationDate)
bp_sd_V2$DateOfBirth <- as.Date(bp_sd_V2$DateOfBirth)

wide_bp_1$ObservationDate <- as.Date(wide_bp_1$ObservationDate)
wide_bp_1$DateOfBirth <- as.Date(wide_bp_1$DateOfBirth)

wide_bp_0$ObservationDate <- as.Date(wide_bp_0$ObservationDate)
wide_bp_0$DateOfBirth <- as.Date(wide_bp_0$DateOfBirth)

wide_bp_1_add$ObservationDate <- as.Date(wide_bp_1_add$ObservationDate)
wide_bp_1_add$DateOfBirth <- as.Date(wide_bp_1_add$DateOfBirth)

# Now bind safely
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

# complete_bp_3_clean <- complete_bp_3_clean %>%
#   select (-DateOfBirth, -AgeAtMeasurement)
#
# unique_dob_gender <- data %>%
#   select(Patient_UUID, DateOfBirth) %>%
#   distinct(Patient_UUID, .keep_all = TRUE)
#
# # Perform the join
# complete_bp_3_clean <- complete_bp_3_clean %>%
#   left_join(unique_dob_gender, by = "Patient_UUID")
#
# complete_bp_3_clean <- complete_bp_3_clean %>%
#   mutate(
#     ObservationDate = as.Date(ObservationDate, format = "%Y-%m-%d"),
#     DateOfBirth = as.Date(DateOfBirth, format = "%Y-%m-%d")
#   )
#
# complete_bp_3_clean <- complete_bp_3_clean %>%
#   mutate(
#     AgeAtMeasurement = as.numeric(difftime(ObservationDate, DateOfBirth, units = "days")) / 365.25
#   )
#
#
# formatted_uuids <- complete_bp_3_clean %>%
#   filter(grepl("^\\d{4}-\\d{2}-\\d{2}$", ObservationDate)) %>%
#   select(Patient_UUID)
#
# formatted_uuids_incorrect <- complete_bp_2 %>%
#   filter(!grepl("^\\d{4}-\\d{2}-\\d{2}$", ObservationDate)) %>%
#   select(Patient_UUID, ObservationDate)





bp_sd <- complete_bp_3_clean
distinct.bp_sd <- bp_sd %>%
  distinct(Patient_UUID)

# Filtering Patients who have BP measurement less than 55 y.o
bp_sd_less55 <- bp_sd %>%
  filter(AgeAtMeasurement <= 55)
distinct.bp_sd_less55 <- bp_sd_less55 %>%
  distinct(Patient_UUID)

# bp_sd_more55 <- bp_sd %>%
#   filter(AgeAtMeasurement > 55)
# distinct.bp_sd_more55 <- bp_sd_more55 %>%
#   distinct(Patient_UUID)

# Matching with the outcome data
n_outcome_LA55 <- length(intersect(bp_sd_less55$Patient_UUID, cleaned_outcome_56$Patient_UUID))

n_outcome_LA55

bp_sd_less55_unique <- bp_sd_less55 %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# complete_outcome_56_unique <- cleaned_outcome_56 %>%
#   distinct(Patient_UUID, .keep_all = TRUE)

intersect_df_LA55 <- inner_join(
  bp_sd_less55_unique,
  complete_outcome_55_unique,
  by = "Patient_UUID"
)


# Calculating the number of outcome
intersect_df_LA55 %>%
  distinct(Patient_UUID, outcome) %>%
  mutate(outcome_group = case_when(
    is.na(outcome) ~ "NA",
    outcome == 1 ~ "1",
    outcome == 0 ~ "0"
  )) %>%
  count(outcome_group)

bp_sd_less55_inter_out <- bp_sd_less55 %>%
  filter(Patient_UUID %in% intersect_df_LA55$Patient_UUID)

# Filtering patients with BP >=5
patients_BP_more5 <- bp_sd_less55_inter_out %>%
  add_count(Patient_UUID) %>%
  filter(n >= 5)
distinct.patients_BP_more5 <- patients_BP_more5 %>%
  distinct(Patient_UUID)

patients_BP_more5_outcome <- patients_BP_more5 %>%
  left_join(
    complete_outcome_56 %>%
      select(Patient_UUID, outcome) %>%
      distinct(Patient_UUID, .keep_all = TRUE),
    by = "Patient_UUID"
  )


# Calculating the number of outcome
patients_BP_more5_outcome %>%
  distinct(Patient_UUID, outcome) %>%
  mutate(outcome_group = case_when(
    is.na(outcome) ~ "NA",
    outcome == 1 ~ "1",
    outcome == 0 ~ "0"
  )) %>%
  count(outcome_group)



# Filtering patients with BP >=3
# patients_BP_more3 <- bp_sd_less55_inter_out %>%
#   add_count(Patient_UUID) %>%
#   filter(n >= 3)
# distinct.patients_BP_more3 <- patients_BP_more3 %>%
#   distinct(Patient_UUID)
#
# patients_BP_more3_outcome <- patients_BP_more3 %>%
#   left_join(
#     complete_outcome_56 %>%
#       select(Patient_UUID, outcome) %>%
#       distinct(Patient_UUID, .keep_all = TRUE),
#     by = "Patient_UUID"
#   )
#
# # Calculating the number of outcome
# patients_BP_more3_outcome %>%
#   distinct(Patient_UUID, outcome) %>%
#   mutate(outcome_group = case_when(
#     is.na(outcome) ~ "NA",
#     outcome == 1 ~ "1",
#     outcome == 0 ~ "0"
#   )) %>%
#   count(outcome_group)
#################################################

# # Filtering patients with BP <=5
# patients_BP_less5 <- intersect_df_LA55 %>%
#   add_count(Patient_UUID) %>%
#   filter(n < 5)
# distinct.patients_BP_less5 <- patients_BP_less5 %>%
#   distinct(Patient_UUID)
#
# # Filtering patients with BP <3
# patients_BP_less3 <- intersect_df_LA55 %>%
#   add_count(Patient_UUID) %>%
#   filter(n < 3)
# distinct.patients_BP_less3 <- patients_BP_less3 %>%
#   distinct(Patient_UUID)
###############################################################

bp_sd_more56 <- bp_sd %>%
  filter(AgeAtMeasurement >= 56)
distinct.bp_sd_more56 <- bp_sd_more56 %>%
  distinct(Patient_UUID)


write.csv(distinct.patients_BP_less5, "distinct.patients_BP_less5", row.names = FALSE)



############# check on Patient_UUID with BP before and after 55 #############

# Step 1: For each patient, count how many measurements at AgeOfMeasurement <= 55 and >= 56
patient_age_counts <- bp_sd %>%
  group_by(Patient_UUID) %>%
  summarise(
    count_under_55 = sum(AgeAtMeasurement <= 55, na.rm = TRUE),
    count_over_56 = sum(AgeAtMeasurement >= 56, na.rm = TRUE)
  )

number_of_patients_with_5 <- patient_age_counts %>%
  filter(count_under_55 == 5) %>%
  summarise(n = n())



summary_stats_patient_age_counts <- patient_age_counts %>%
  summarise(
    min_count55 = min(count_under_55, na.rm = TRUE),
    max_count55 = max(count_under_55, na.rm = TRUE)
  )


# Step 2: Filter patients who meet both conditions
eligible_patients <- patient_age_counts %>%
  filter(count_under_55 >= 5, count_over_56 >= 1) %>%
  pull(Patient_UUID)

# Step 3: Filter the original data frame for only those patients
filtered_bp_sd_5 <- bp_sd %>%
  filter(Patient_UUID %in% eligible_patients)

distinct.filtered_bp_sd_5 <- filtered_bp_sd %>%
  distinct(Patient_UUID)

# Filtering patients with BP >=5
patients_BP_more5_fromall <- filtered_bp_sd %>%
  add_count(Patient_UUID) %>%
  filter(n >= 5)

summary_stats_bp <- patients_BP_more5_fromall %>%
  summarise(
    min_bp = min(n, na.rm = TRUE),
    max_bp = max(n, na.rm = TRUE)
  )


#########################################################

duplicates_outcome_56 <- complete_outcome_56 %>%
  group_by(Patient_UUID) %>%
  filter(n() > 1) %>%
  ungroup()



intersect_bp_outcome_55_5 <- inner_join(
  cleaned_outcome_56,
  distinct.filtered_bp_sd_5,
  by = "Patient_UUID"
)

distinct.intersect_bp_outcome_55_5 <- intersect_bp_outcome_55_5 %>%
  distinct(Patient_UUID)

intersect_bp_outcome_55_V2 <- inner_join(
  intersect_bp_outcome_55,
  distinct.patients_BP_more5,
  by = "Patient_UUID"
)

#################################

# Find Patient_UUIDs in distinct.filtered_bp_sd but not in distinct.patients_BP_more5
patients_not_in_all_55_56 <- distinct.filtered_bp_sd_5 %>%
  filter(!(Patient_UUID %in% distinct.patients_BP_more5$Patient_UUID))

# Filter h_outcome by Patient_UUIDs listed in patients_not_in_all_55_56
h_outcome_selected <- latest.VisitDate.no.CVD.h.56 %>%
  filter(Patient_UUID %in% patients_not_in_all_55_56$Patient_UUID)

gp_outcome_mix_selected <- latest.VisitDate.no.CVD.gp.56 %>%
  filter(Patient_UUID %in% patients_not_in_all_55_56$Patient_UUID)

outcome_1_selected <- outcome_1 %>%
  filter(Patient_UUID %in% patients_not_in_all_55_56$Patient_UUID)



# Find Patient_UUIDs in distinct.filtered_bp_sd but not in distinct.patients_BP_more5
patients_not_in_all_55_56 <- distinct.filtered_bp_sd_3BP %>%
  filter(!(Patient_UUID %in% distinct.patients_BP_more5$Patient_UUID))

