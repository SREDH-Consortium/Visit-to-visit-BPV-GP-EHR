library("dplyr")
library("lubridate")
library("eeptools")
library("data.table")
library("stringr")
library("writexl")
library("xlsx")
library(survival)
library(ggplot2)


data <- `Demographics_BloodPRessure`
h_outcome <- Hospital_Admissions
gp_outcome <- BPV_CardiacEvents
med <- Medications
gp_outcome_mix <- Comorbidities
lab <- LabValues



gp_outcome_mix$VisitDate <- as.Date(gp_outcome_mix$VisitDate, format = "%d-%m-%Y")

# Remove rows with NA VisitDate and Reason
gp_outcome_mix <- gp_outcome_mix %>% filter(!is.na(VisitDate))
gp_outcome_mix <- gp_outcome_mix %>%
  filter(!is.na(Reason) & trimws(Reason) != "")

# Remove rows with NA ENCOUNTER_DATE and Reason
h_outcome <- h_outcome %>% filter(!is.na(ENCOUNTER_DATE))
h_outcome <- h_outcome %>% filter(!is.na(DIAGNOSIS_CODE))

# Convert columns to Date type (adjust the format as needed)
data$DateOfBirth <- as.Date(data$DateOfBirth, format = "%Y-%m-%d")
data$ObservationDate <- as.Date(data$ObservationDate, format = "%Y-%m-%d")

# Calculate age in years
data <- data %>%
  mutate(BPobs_duration = time_length(interval(DateOfBirth, ObservationDate), "years"))


###### filtering the outcome #########


outcome <- c("I20.0", "I21.1", "I21.3", "I21.4", "I21.9", "I25.2", "I25.8", "I25.9", "I25.10", "I25.11", "I25.12", "I46.0", "I46.9", "I48", "I48.0", "I48.2", "I48.9",
             "I50.0", "I50.9", "I61.4", "I61.5", "I63.3", "I63.5", "I63.8", "I63.9", "I64", "I66.0", "I67.2", "I70.20", "I70.21", "I70.22", "I70.23", "R57", "Y84.0",
             "Z95.1", "Z95.5")

pt.h.outcome <- h_outcome %>%
  filter(DIAGNOSIS_CODE %in% outcome)

earliest.pt.h.outcome <- pt.h.outcome %>%
  group_by(Patient_UUID) %>%
  summarize(earliest_VisitDate = min(ENCOUNTER_DATE, na.rm = TRUE), .groups = 'drop')

# distinct patients_h_outcome_1835
distinct.h.outcome <- pt.h.outcome %>%
  distinct(Patient_UUID)

# Get the earliest VisitDate for each Patient_UUID
earliest.gp.outcome <- gp_outcome %>%
  group_by(Patient_UUID) %>%
  summarize(earliest_VisitDate = min(VisitDate, na.rm = TRUE), .groups = 'drop')

outcome_1 <- rbind (earliest.pt.h.outcome, earliest.gp.outcome)

# Extract unique Patient_UUID, DateOfBirth and Gender combinations
unique_dob_gender <- data %>%
  select(Patient_UUID, DateOfBirth, Gender) %>%
  distinct(Patient_UUID, .keep_all = TRUE)


# Perform the join
outcome_1 <- outcome_1 %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

# Convert earliest_VisitDate from character to Date
outcome_1 <- outcome_1 %>%
  mutate(
    earliest_VisitDate = as.Date(earliest_VisitDate)  # Convert to Date format
  )


# Calculate outcome_age
outcome_1 <- outcome_1 %>%
  mutate(
    outcome_age = ceiling (as.numeric(difftime(earliest_VisitDate, DateOfBirth, units = "days")) / 365.25)
  )

# Remove rows with NA in the DateOfBirth column
outcome_1 <- outcome_1 %>%
  filter(!is.na(DateOfBirth))

outcome_1 <- outcome_1 %>%
  filter(Gender != "Transgender/Other")

outcome_1 <- outcome_1 %>%
  mutate(outcome = 1)

outcome_1 <- outcome_1 %>%
  rename (outcome_date = earliest_VisitDate)

# Filter for the earliest outcome_date per Patient_UUID
outcome_1 <- outcome_1 %>%
  group_by(Patient_UUID) %>%
  filter(outcome_date == min(outcome_date)) %>%
  ungroup()

outcome_1_46 <- outcome_1 %>%
  filter(outcome_age >= 46)

outcome_1_51 <- outcome_1 %>%
  filter(outcome_age >= 51)

outcome_1_56 <- outcome_1 %>%
  filter(outcome_age >= 56)

write.csv(outcome_1, "outcome_1_V2.csv", row.names = FALSE)


# distinct patients_h_outcome_1835
distinct.outcome_1 <- outcome_1 %>%
  distinct(Patient_UUID)

###### OUTCOME = 0 from GP data ########
# Filter `gp_outcome_mix` to get Patient_UUIDs not in `outcome_1`
no.CVD.gp <- gp_outcome_mix[!(gp_outcome_mix$Patient_UUID %in% outcome_1$Patient_UUID), ]

# Extract unique Patient_UUID, DateOfBirth and Gender combinations
unique_dob_gender <- data %>%
  select(Patient_UUID, DateOfBirth, Gender) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

unique_dob_gender <- unique_dob_gender %>%
  filter(Gender != "Transgender/Other")


# Perform the join
no.CVD.gp <- no.CVD.gp %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

# Calculate the outcome_age

no.CVD.gp <- no.CVD.gp %>%
  mutate(
    outcome_age = ceiling(as.numeric(difftime(VisitDate, DateOfBirth, units = "days")) / 365.25)
  )


no.CVD.gp.46 <- no.CVD.gp %>%
  filter(outcome_age >= 46)


# Assuming `outcome0.l` has columns `Patient_UUID` and `VisitDate`
latest.VisitDate.no.CVD.gp.46 <- no.CVD.gp.46 %>%
  group_by(Patient_UUID) %>%
  summarize(Latest.VisitDate = max(VisitDate, na.rm = TRUE))


no.CVD.gp.51 <- no.CVD.gp %>%
  filter(outcome_age >= 51)


# Assuming `outcome0.l` has columns `Patient_UUID` and `VisitDate`
latest.VisitDate.no.CVD.gp.51 <- no.CVD.gp.51 %>%
  group_by(Patient_UUID) %>%
  summarize(Latest.VisitDate = max(VisitDate, na.rm = TRUE))


no.CVD.gp.56 <- no.CVD.gp %>%
  filter(outcome_age >= 56)


# Assuming `outcome0.l` has columns `Patient_UUID` and `VisitDate`
latest.VisitDate.no.CVD.gp.56 <- no.CVD.gp.56 %>%
  group_by(Patient_UUID) %>%
  summarize(Latest.VisitDate = max(VisitDate, na.rm = TRUE))



###### OUTCOME = 0 from hospital data ########

# Filter `h_outcome` to get Patient_UUIDs not in `outcome_1`
no.CVD.h <- h_outcome[!(h_outcome$Patient_UUID %in% outcome_1$Patient_UUID), ]

# Extract unique Patient_UUID, DateOfBirth and Gender combinations
unique_dob_gender <- data %>%
  select(Patient_UUID, DateOfBirth, Gender) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

unique_dob_gender <- unique_dob_gender %>%
  filter(Gender != "Transgender/Other")


# Perform the join
no.CVD.h <- no.CVD.h %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

# Calculate the outcome_age

no.CVD.h <- no.CVD.h %>%
  mutate(
    ENCOUNTER_DATE = as.Date(ENCOUNTER_DATE, format = "%Y-%m-%d"),
    DateOfBirth = as.Date(DateOfBirth, format = "%Y-%m-%d"),
    outcome_age = ceiling (as.numeric(difftime(ENCOUNTER_DATE, DateOfBirth, units = "days")) / 365.25)
  )

no.CVD.h.46 <- no.CVD.h %>%
  filter(outcome_age >= 46)


# Assuming `outcome0.l` has columns `Patient_UUID` and `VisitDate`
latest.VisitDate.no.CVD.h.46 <- no.CVD.h.46 %>%
  group_by(Patient_UUID) %>%
  summarize(Latest.VisitDate = max(ENCOUNTER_DATE, na.rm = TRUE))


no.CVD.h.51 <- no.CVD.h %>%
  filter(outcome_age >= 51)


# Assuming `outcome0.l` has columns `Patient_UUID` and `VisitDate`
latest.VisitDate.no.CVD.h.51 <- no.CVD.h.51 %>%
  group_by(Patient_UUID) %>%
  summarize(Latest.VisitDate = max(ENCOUNTER_DATE, na.rm = TRUE))


no.CVD.h.56 <- no.CVD.h %>%
  filter(outcome_age >= 56)


# Assuming `outcome0.l` has columns `Patient_UUID` and `VisitDate`
latest.VisitDate.no.CVD.h.56 <- no.CVD.h.56 %>%
  group_by(Patient_UUID) %>%
  summarize(Latest.VisitDate = max(ENCOUNTER_DATE, na.rm = TRUE))


# Combine the datasets for each landmark
combined_noCVD.46 <- rbind(latest.VisitDate.no.CVD.gp.46, latest.VisitDate.no.CVD.h.46)
combined_noCVD.51 <- rbind(latest.VisitDate.no.CVD.gp.51, latest.VisitDate.no.CVD.h.51)
combined_noCVD.56 <- rbind(latest.VisitDate.no.CVD.gp.56, latest.VisitDate.no.CVD.h.56)



# Filter for the latest VisitDate per Patient_UUID for each landmark
outcome_0_46 <- combined_noCVD.46 %>%
  group_by(Patient_UUID) %>%
  filter(Latest.VisitDate == max(Latest.VisitDate)) %>%
  ungroup()

outcome_0_51 <- combined_noCVD.51 %>%
  group_by(Patient_UUID) %>%
  filter(Latest.VisitDate == max(Latest.VisitDate)) %>%
  ungroup()

outcome_0_56 <- combined_noCVD.56 %>%
  group_by(Patient_UUID) %>%
  filter(Latest.VisitDate == max(Latest.VisitDate)) %>%
  ungroup()

# Add outcome column

outcome_0_46<- outcome_0_46 %>%
  mutate(outcome = 0)

outcome_0_51<- outcome_0_51 %>%
  mutate(outcome = 0)

outcome_0_56<- outcome_0_56 %>%
  mutate(outcome = 0)

#Rename the Latest.VisitDate

outcome_0_46 <- outcome_0_46 %>%
  rename (outcome_date = Latest.VisitDate)

outcome_0_51 <- outcome_0_51 %>%
  rename (outcome_date = Latest.VisitDate)

outcome_0_56 <- outcome_0_56 %>%
  rename (outcome_date = Latest.VisitDate)

# distinct patients outcome = 0 for each landmark
distinct.outcome_0_46 <- outcome_0_46 %>%
  distinct(Patient_UUID)

distinct.outcome_0_51 <- outcome_0_51 %>%
  distinct(Patient_UUID)

distinct.outcome_0_56 <- outcome_0_56 %>%
  distinct(Patient_UUID)



# Step 1: Remove duplicate Patient_UUID and retain the one with the latest outcome_date (if tied, pick the first row)
outcome_0_46_cleaned <- outcome_0_46 %>%
  group_by(Patient_UUID) %>%
  filter(outcome_date == max(outcome_date, na.rm = TRUE)) %>%
  slice(1) %>%  # In case of ties, keep only the first occurrence
  ungroup()


outcome_0_51_cleaned <- outcome_0_51 %>%
  group_by(Patient_UUID) %>%
  filter(outcome_date == max(outcome_date, na.rm = TRUE)) %>%
  slice(1) %>%  # In case of ties, keep only the first occurrence
  ungroup()


outcome_0_56_cleaned <- outcome_0_56 %>%
  group_by(Patient_UUID) %>%
  filter(outcome_date == max(outcome_date, na.rm = TRUE)) %>%
  slice(1) %>%  # In case of ties, keep only the first occurrence
  ungroup()



# outcome_0_cleaned <- outcome_0 %>%
#   group_by(Patient_UUID) %>%
#   filter(outcome_date == max(outcome_date, na.rm = TRUE)) %>%
#   distinct(Patient_UUID, .keep_all = TRUE) %>%  # Ensures only one row per Patient_UUID
#   ungroup()
#
#
# # Step 2: Check if there are still any duplicated Patient_UUID after filtering
# duplicated_patient_uuids <- outcome_0_cleaned %>%
#   group_by(Patient_UUID) %>%
#   filter(n() > 1) %>%
#   ungroup()
#
# # If any duplicated Patient_UUIDs are still present, display them
# duplicated_patient_uuids

outcome_0_46 <- outcome_0_46_cleaned
outcome_0_51 <- outcome_0_51_cleaned
outcome_0_56 <- outcome_0_56_cleaned


# Perform the join
outcome_0_46 <- outcome_0_46 %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

# Calculate the outcome_age

outcome_0_46 <- outcome_0_46 %>%
  mutate(
    outcome_age = as.numeric(difftime(outcome_date, DateOfBirth, units = "days")) / 365.25
  )


# Perform the join
outcome_0_51 <- outcome_0_51 %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

# Calculate the outcome_age

outcome_0_51 <- outcome_0_51 %>%
  mutate(
    outcome_age = as.numeric(difftime(outcome_date, DateOfBirth, units = "days")) / 365.25
  )

# Perform the join
outcome_0_56 <- outcome_0_56 %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

# Calculate the outcome_age

outcome_0_56 <- outcome_0_56 %>%
  mutate(
    outcome_age = as.numeric(difftime(outcome_date, DateOfBirth, units = "days")) / 365.25
  )

outcome_46_combined <- rbind (outcome_1_46, outcome_0_46)
outcome_51_combined <- rbind (outcome_1_51, outcome_0_51)
outcome_56_combined <- rbind (outcome_1_56, outcome_0_56)
