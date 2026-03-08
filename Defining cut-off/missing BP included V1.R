library(readr)
library(tidyr)
library(dplyr)


BP_missing <- read_csv("BP_unlinked_V2.csv", col_names = TRUE)

bp_sd_V2 <- read_csv("bp_sd_V2", col_names = TRUE)

# Example: converting DateOfBirth column 
BP_unlinked_V2 <- BP_unlinked_V2 %>%
  mutate(DateOfBirth = dmy(DateOfBirth))

# Step 1: Pivot systolic and diastolic together with a new column for observation type and values
long_bp <- BP_missing %>%
  pivot_longer(
    cols = c(starts_with("Systolic"), starts_with("Diastolic")),
    names_to = "Observation",
    values_to = "ObservationValue"
  )


# Step 2: Extract the index number from the Observation name
long_bp <- long_bp %>%
  mutate(
    index = gsub("[^0-9]", "", Observation),
    Observation = gsub("[0-9]", "", Observation)
  )

# Step 3: Pivot the date columns and match dates to the corresponding observation
long_bp <- long_bp %>%
  pivot_longer(
    cols = starts_with("Date"),
    names_to = "DateCol",
    values_to = "ObservationDate"
  ) %>%
  mutate(date_index = gsub("[^0-9]", "", DateCol)) %>%
  filter(index == date_index) %>%   # Keep only matching date-observation pairs
  select(Patient_UUID, Observation, ObservationValue, ObservationDate)


# Converting DateOfBirth column in wide_bp_upto55at2012
long_bp <- long_bp %>%
  mutate(ObservationDate = dmy(ObservationDate))


wide_bp <- long_bp %>%
  mutate(Observation = ifelse(Observation == "Systolic", "ObservationValue_S", "ObservationValue_D")) %>%
  pivot_wider(
    names_from = Observation,
    values_from = ObservationValue
  )


# Perform the join
wide_bp <- wide_bp %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

wide_bp <- wide_bp %>%
  mutate(
    ObservationDate = as.Date(ObservationDate, format = "%Y-%m-%d"),
    DateOfBirth = as.Date(DateOfBirth, format = "%Y-%m-%d")
  )


wide_bp <- wide_bp %>%
  mutate(
    AgeAtMeasurement = as.numeric(difftime(ObservationDate, DateOfBirth, units = "days")) / 365.25
  )

####### bp_sd_V2 is the BP dataset from V8#######


bp_sd_V2 <- bp_sd_V2 %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

bp_sd_V2 <- bp_sd_V2 %>%
  mutate(
    ObservationDate = as.Date(ObservationDate, format = "%Y-%m-%d"),
    DateOfBirth = as.Date(DateOfBirth, format = "%Y-%m-%d")
  )


bp_sd_V2 <- bp_sd_V2 %>%
  mutate(
    AgeAtMeasurement = as.numeric(difftime(ObservationDate, DateOfBirth, units = "days")) / 365.25
  )

wide_bp_less55 <- wide_bp %>%
  filter(AgeAtMeasurement <= 55)
distinct.wide_bp_less55 <- wide_bp_less55 %>%
  distinct(Patient_UUID)

bp_sd_V2_less55 <- bp_sd_V2 %>%
  filter(AgeAtMeasurement <= 55)
distinct.bp_sd_V2_less55 <- bp_sd_V2_less55 %>%
  distinct(Patient_UUID)


######## Matching each dataset with the outcome ###################

n_outcome_56 <- length(intersect(wide_bp_less55$Patient_UUID, outcome_1_56$Patient_UUID))

n_outcome_56_pre <- length(intersect(bp_sd_V2_less55$Patient_UUID, outcome_56_combined$Patient_UUID))

# Print results
n_outcome_56
n_outcome_56_pre


intersect_df_56 <- inner_join(
  wide_bp_less55,
  outcome_1_56,
  by = "Patient_UUID"
)

distinct.intersect_df_56 <- intersect_df_56 %>%
  distinct(Patient_UUID)

intersect_df_56_pre <- inner_join(
  bp_sd_V2_less55,
  outcome_56_combined,
  by = "Patient_UUID"
)

distinct.intersect_df_56_pre <- intersect_df_56_pre %>%
  distinct(Patient_UUID)

###### Checking duplicated Patient_UUID in both dataset ##########

intersect_both <- inner_join(
  distinct.intersect_df_56_pre,
  distinct.intersect_df_56,
  by = "Patient_UUID"
)

intersect_o1 <- inner_join(
  intersect_both,
  outcome_1_56,
  by = "Patient_UUID"
)
