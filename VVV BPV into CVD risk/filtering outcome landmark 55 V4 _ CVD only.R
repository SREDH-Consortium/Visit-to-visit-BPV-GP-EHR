library (plyr)


# If you use 'plyr' for other reasons, load it first:
# library(plyr)

# Then load 'dplyr' (or 'tidyverse'):
library(dplyr)
library(lubridate) # Often useful for dates, but not strictly needed for this problem
# ... any other packages you need

data <- `Demographics_BloodPRessure`
h_outcome <- Hospital_Admissions
gp_outcome <- BPV_CardiacEvents
gp_outcome_mix <- Comorbidities
lab <- LabValues
med <- Medications



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




gp_outcome_CVD <- gp_outcome %>%
  filter(!str_detect(tolower(Reason), "atrial fibrillation"))

# Ensure VisitDate is in Date format
gp_outcome_CVD <- gp_outcome_CVD %>%
  mutate(VisitDate = as.Date(VisitDate))

# Get full row with earliest VisitDate per patient
earliest.gp_outcome_CVD <- gp_outcome_CVD %>%
  filter(!is.na(VisitDate)) %>%
  group_by(Patient_UUID) %>%
  slice_min(VisitDate, with_ties = FALSE) %>%
  ungroup()


# Step 1: Remove duplicates in outcome_1_55 (keep one row per Patient_UUID)
outcome_1_unique <- outcome_1_55 %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Step 2: Join with h_outcome (many diagnoses per Patient_UUID are allowed)
outcome_1_553 <- h_outcome %>%
  filter(Patient_UUID %in% outcome_1_unique$Patient_UUID)

outcome <- c( "G45.8", "G45.9", "I20.0", "I20.1", "I20.8", "I20.9", "I21.1", "I21.3", "I21.4", "I21.9", "I25.2", "I25.8", "I25.9", "I25.10", "I25.11",
              "I25.12", "I46.0", "I46.9", "I50.0", "I50.9", "I61.1", "I61.4", "I61.5", "I61.9", "I63.2", "I63.3", "I63.4", "I63.5", "I63.8", "I63.9",
              "I64", "I66.0", "I66.9", "I67.2", "I70.20", "I70.21", "I70.22", "I70.23", "I73.9", "K41.9", "K42.9", "K43.2", "K43.3", "K43.9", "K44.9",
              "K50.1", "K50.9", "K51.8", "K51.9", "K64.8", "K64.9", "K75.4", "K75.8", "R57", "R93.1", "Y84.0", "Z95.1", "Z95.5")

pt.h.outcome <- outcome_1_553 %>%
  filter(DIAGNOSIS_CODE %in% outcome)

pt.h.outcome$ENCOUNTER_DATE <- as.Date(pt.h.outcome$ENCOUNTER_DATE, format = "%Y-%m-%d") # Adjust format as needed


pt.h.outcome_CVDonly <- pt.h.outcome %>%
  mutate(DIAGNOSIS_DESCRIPTION = str_trim(DIAGNOSIS_DESCRIPTION)) %>%
  filter(!str_detect(DIAGNOSIS_DESCRIPTION, regex("colitis|crohn|hemorrhoids|haemorrhoids|hernia|abortion|endoscopic", ignore_case = TRUE)))



# Ensure ENCOUNTER_DATE is in Date format
pt.h.outcome_CVDonly <- pt.h.outcome_CVDonly %>%
  mutate(ENCOUNTER_DATE = as.Date(ENCOUNTER_DATE))

# Get full row with earliest ENCOUNTER_DATE per patient
earliest.pt.h.outcome <- pt.h.outcome_CVDonly %>%
  filter(!is.na(ENCOUNTER_DATE)) %>%
  group_by(Patient_UUID) %>%
  slice_min(ENCOUNTER_DATE, with_ties = FALSE) %>%
  ungroup()


# distinct patients_h_outcome_1835
distinct.h.outcome <- pt.h.outcome_CVDonly %>%
  distinct(Patient_UUID)

distinct.gp.outcome <- gp_outcome_CVD %>%
  distinct(Patient_UUID)

earliest.gp_outcome_CVD <- earliest.gp_outcome_CVD %>%
  mutate(DIAGNOSIS_CODE = NA_character_)


h_outcome_CVD <- earliest.pt.h.outcome %>%
  select (-PERSON_ID, -ENCOUNTER_TYPE)

h_outcome_CVD <- h_outcome_CVD %>%
  dplyr::rename(
    VisitDate = ENCOUNTER_DATE ,
    Reason = DIAGNOSIS_DESCRIPTION
  )


outcome_1_cvd <- rbind (h_outcome_CVD, earliest.gp_outcome_CVD)

outcome_1_cvd <- outcome_1_cvd

write.csv (outcome_1_cvd, "outcome_1_cvd", row.names = FALSE)


# Extract unique Patient_UUID, DateOfBirth and Gender combinations
unique_dob_gender <- data %>%
  select(Patient_UUID, DateOfBirth, Gender) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Fix the data type
unique_dob_gender$DateOfBirth <- as.Date(unique_dob_gender$DateOfBirth)


# Perform the join
outcome_1_cvd <- outcome_1_cvd %>%
  left_join(unique_dob_gender, by = "Patient_UUID")

# Convert earliest_VisitDate from character to Date
outcome_1_cvd <- outcome_1_cvd %>%
  mutate(
    VisitDate = as.Date(VisitDate)  # Convert to Date format
  )


# Calculate outcome_age
outcome_1_cvd <- outcome_1_cvd %>%
  mutate(
    outcome_age = ceiling (as.numeric(difftime(VisitDate, DateOfBirth, units = "days")) / 365.25)
  )

# Remove rows with NA in the DateOfBirth column
outcome_1_cvd <- outcome_1_cvd %>%
  filter(!is.na(DateOfBirth))

outcome_1_cvd <- outcome_1_cvd %>%
  filter(Gender != "Transgender/Other")

outcome_1_cvd <- outcome_1_cvd %>%
  mutate(outcome = 1)

outcome_1_cvd <- outcome_1_cvd %>%
  dplyr::rename (outcome_date = VisitDate)

# Filter for the earliest outcome_date per Patient_UUID
outcome_1_cvd <- outcome_1_cvd %>%
  group_by(Patient_UUID) %>%
  filter(outcome_date == min(outcome_date)) %>%
  ungroup()

additional_outcome_1_cvd <- additional_outcome_1_56...FORMATTED

additional_outcome_1_cvd$VisitDate <- as.Date(additional_outcome_1_cvd$VisitDate, format = "%d/%m/%Y")
additional_outcome_1_cvd$DateOfBirth <- as.Date(additional_outcome_1_cvd$DateOfBirth, format = "%d/%m/%Y")

additional_outcome_1_cvd <- additional_outcome_1_cvd %>%
  select (- ENCOUNTER_TYPE)

additional_outcome_1_cvd <- additional_outcome_1_cvd %>%
  mutate(outcome = 1)


additional_outcome_1_cvd <- additional_outcome_1_cvd %>%
  dplyr::rename (outcome_date = VisitDate,
                 Reason = DIAGNOSIS_DESCRIPTION,
                 )

outcome_1_cvd_final <- rbind (outcome_1_cvd, additional_outcome_1_cvd)

# Identify duplicated Patient_UUIDs
duplicated_ids <- outcome_1_cvd_final %>%
  group_by(Patient_UUID) %>%
  filter(n() > 1) %>%
  pull(Patient_UUID) %>%
  unique()

# View them
duplicated_ids

# View rows that are duplicated
outcome_1_cvd_final %>%
  filter(Patient_UUID %in% duplicated_ids)

outcome_1_cvd_final <- outcome_1_cvd_final %>%
  filter(!Patient_UUID %in% duplicated_ids)

outcome_1_cvd_final <- outcome_1_cvd_final %>%
  distinct(Patient_UUID, .keep_all = TRUE)



write.csv (outcome_1_cvd_final, "outcome_1_cvd_final_S3.csv", row.names = FALSE)

# Filter for the earliest outcome_date per Patient_UUID
outcome_1_cvd_final <- outcome_1_cvd_final %>%
  group_by(Patient_UUID) %>%
  filter(outcome_date == min(outcome_date)) %>%
  ungroup()

outcome_1_cvd_55 <- outcome_1_cvd_final %>%
  filter(outcome_age >= 55)

write.csv (outcome_1_cvd_55, "outcome_1_cvd_55_V2", row.names = FALSE)

###### OUTCOME = 0 from GP data ########
# Filter `gp_outcome_mix` to get Patient_UUIDs not in `outcome_1`
no.CVD.gp <- gp_outcome_mix[!(gp_outcome_mix$Patient_UUID %in% outcome_1_cvd_final$Patient_UUID), ]

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


no.CVD.gp.55 <- no.CVD.gp %>%
  filter(outcome_age >= 55)

# distinct patients_h_outcome_1835
distinct.no.CVD.gp.55 <- no.CVD.gp.55 %>%
  distinct(Patient_UUID)



# filtering the latest visit date
latest.VisitDate.no.CVD.gp.55 <- no.CVD.gp.55 %>%
  group_by(Patient_UUID) %>%
  summarize(Latest.VisitDate = max(VisitDate, na.rm = TRUE))



latest.VisitDate.no.CVD.gp.55 <- no.CVD.gp.55 %>%
  group_by(Patient_UUID) %>%
  filter(VisitDate == max(VisitDate, na.rm = TRUE)) %>%
  slice(1) %>%  # pick the first row if multiple rows have the same latest VisitDate
  ungroup()





###### OUTCOME = 0 from hospital data ########

# Filter `h_outcome` to get Patient_UUIDs not in `outcome_1`
no.CVD.h <- h_outcome[!(h_outcome$Patient_UUID %in% outcome_1_cvd_final$Patient_UUID), ]

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


no.CVD.h.55 <- no.CVD.h %>%
  filter(outcome_age >= 55)


# obtaining the latest visit date
latest.VisitDate.no.CVD.h.55 <- no.CVD.h.55 %>%
  group_by(Patient_UUID) %>%
  summarize(Latest.VisitDate = max(ENCOUNTER_DATE, na.rm = TRUE))


latest.VisitDate.no.CVD.h.55 <- no.CVD.h.55 %>%
  group_by(Patient_UUID) %>%
  filter(ENCOUNTER_DATE == max(ENCOUNTER_DATE, na.rm = TRUE)) %>%
  slice(1) %>%
  ungroup()


latest.VisitDate.no.CVD.gp.55 <- latest.VisitDate.no.CVD.gp.55 %>%
  mutate(DIAGNOSIS_CODE = NA_character_)
latest.VisitDate.no.CVD.gp.55 <- latest.VisitDate.no.CVD.gp.55 %>%
  mutate(ENCOUNTER_TYPE = NA_character_)
latest.VisitDate.no.CVD.gp.55 <- latest.VisitDate.no.CVD.gp.55 %>%
  rename(DIAGNOSIS_DESCRIPTION = Reason)


latest.VisitDate.no.CVD.h.55 <- latest.VisitDate.no.CVD.h.55 %>%
  select(-PERSON_ID)
latest.VisitDate.no.CVD.h.55 <- latest.VisitDate.no.CVD.h.55 %>%
  rename(VisitDate = ENCOUNTER_DATE)

# Combine the datasets for each landmark
combined_noCVD.55 <- rbind(latest.VisitDate.no.CVD.gp.55, latest.VisitDate.no.CVD.h.55)

# List of Patient_UUIDs I want
selected_uuids <- c(
  "A3660919-7F22-41A9-93B5-F3DBE3FDF808", "C7A96325-8408-4B71-9918-4AE71654C6A8",
  "711F493D-0019-4B70-B108-2725FDA95A15", "01DF5EB7-91E4-495B-B80E-23E21B899C95",
  "5C29D647-06C4-428F-B996-804EB57BC44F", "657144C2-D4E2-4B72-AB5B-9D79EC40A496",
  "67601A62-978F-4BDA-A46B-FB3BEE7AC4EE", "893F3629-7B0B-4830-B3E3-4355E5D71465",
  "8DDAC135-6EA9-4DA0-A09C-8EF11A8C5E6B", "AF318549-A3D5-4736-AC76-63A70F5ADA31",
  "B7879935-90BD-4688-9C3B-5E44BF17D5F0", "1FECA810-30B1-47B8-9821-70A7263B4581",
  "24EE78AF-74EB-4218-BF73-0FB49758D996", "3E7E73B8-4B59-4FFE-9B19-B92E1A77FA1D",
  "829370B6-6AF2-4E1A-AD2B-29570F7BB7A8", "893BA3E4-52C4-4BF4-B0B2-880DB704C13D",
  "B5BE6F7D-557A-47DF-97D4-95E050EAB4CD", "C88CC036-2557-45FD-BE23-983C6EB951D0",
  "FCA97205-3C33-4F0E-8B3D-C3E820398F2C", "FF98993D-DABD-4D27-9991-F38B08AA8687",
  "06F09A12-3B66-4D96-8225-E2CB8A94606F", "6096876D-5CE0-4E5A-A458-15964482A75C",
  "F54106BE-11A8-44A8-AE8A-7B51D1E9367D", "3F3ACAC0-5011-46BD-ACF4-6E40AF8614A1"
)


# Filter the dataset
selected_combined_noCVD_55 <- combined_noCVD.55 %>%
  filter(Patient_UUID %in% selected_uuids)

selected_combined_noCVD_55 <- selected_combined_noCVD_55 %>%
  filter(!( (is.na(DIAGNOSIS_DESCRIPTION) | DIAGNOSIS_DESCRIPTION == "") &
              (is.na(DIAGNOSIS_CODE) | DIAGNOSIS_CODE == "") ))

# write.csv(selected_combined_noCVD_56, "additional_outcome_1_56.csv", row.names = FALSE)


# Filter rows where at least one of DIAGNOSIS_DESCRIPTION or DIAGNOSIS_CODE is not empty or NA
empty_diag_combined_noCVD_55 <- combined_noCVD.55 %>%
  filter(is.na(DIAGNOSIS_DESCRIPTION) | DIAGNOSIS_DESCRIPTION == "",
         is.na(DIAGNOSIS_CODE) | DIAGNOSIS_CODE == "")


clean_combined_noCVD_55 <- combined_noCVD.55 %>%
  filter(!( (is.na(DIAGNOSIS_DESCRIPTION) | DIAGNOSIS_DESCRIPTION == "") &
              (is.na(DIAGNOSIS_CODE) | DIAGNOSIS_CODE == "") ))

final_clean_combined_noCVD_55 <- clean_combined_noCVD_55 %>%
  group_by(Patient_UUID) %>%              # Group by each patient
  slice_max(order_by = VisitDate, n = 1, with_ties = FALSE) %>%  # Keep only the latest VisitDate
  ungroup()


write.csv(clean_combined_noCVD_55, "clean_combined_noCVD_55.csv", row.names = FALSE)
write.csv(final_clean_combined_noCVD_55, "final_clean_combined_noCVD_55.csv", row.names = FALSE)

outcome_0_55 <- final_clean_combined_noCVD_55
outcome_0_55 <- outcome_0_55 %>%
  rename (outcome_date = VisitDate)

outcome_0_55 <- outcome_0_55 %>%
  select(-DateOfBirth, -ENCOUNTER_TYPE)

unique_dob <- data %>%
  select(Patient_UUID, DateOfBirth) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

# Perform the join
outcome_0_55 <- outcome_0_55 %>%
  left_join(unique_dob, by = "Patient_UUID")


outcome_0_55$outcome <- 0

outcome_0_55 <- outcome_0_55 %>%
  rename (Reason = DIAGNOSIS_DESCRIPTION)


common_outcome_55_df <- data.frame(Patient_UUID = intersect(outcome_1_cvd_55$Patient_UUID, outcome_0_55$Patient_UUID))



complete_outcome_55 <- rbind (outcome_1_cvd_55, outcome_0_55)


cleaned_outcome_55 <- complete_outcome_55 %>%
  group_by(Patient_UUID) %>%
  arrange(outcome_date) %>%  # Ensure date order before selection
  slice(
    if (all(outcome == 1)) {
      which.min(outcome_date)  # pick earliest if all 1
    } else if (any(outcome == 1)) {
      which(outcome == 1)[1]   # pick first outcome == 1 if mixed
    } else {
      which.max(outcome_date)  # pick latest if all 0
    }
  ) %>%
  ungroup()

all(cleaned_outcome_55$Patient_UUID %in% complete_outcome_55$Patient_UUID)
all(complete_outcome_55$Patient_UUID %in% cleaned_outcome_55$Patient_UUID)


write.csv(cleaned_outcome_55, "cleaned_final_outcome_55_CVD.csv", row.names = FALSE)

additional_outcome_1_56_formated <- additional_outcome_1_56 %>%
  select(-DIAGNOSIS_DESCRIPTION, -DIAGNOSIS_CODE, -ENCOUNTER_TYPE)

additional_outcome_1_56_formated <- additional_outcome_1_56_formated %>%
  rename (outcome_date = VisitDate)

additional_outcome_1_56_formated$outcome <- 1

additional_outcome_1_56_formated <- additional_outcome_1_56_formated %>%
  mutate(
    DateOfBirth = dmy(DateOfBirth),
    outcome_date = dmy(outcome_date),
    outcome_age = ceiling(as.numeric(difftime(outcome_date, DateOfBirth, units = "days")) / 365.25)
  )


############################
selected_combined_noCVD_56_duplicate <- selected_combined_noCVD_56[!duplicated(selected_combined_noCVD_56$Patient_UUID), ]

selected_uuids_dup <- c(
  "A3660919-7F22-41A9-93B5-F3DBE3FDF808", "C7A96325-8408-4B71-9918-4AE71654C6A8",
  "711F493D-0019-4B70-B108-2725FDA95A15", "01DF5EB7-91E4-495B-B80E-23E21B899C95",
  "5C29D647-06C4-428F-B996-804EB57BC44F", "657144C2-D4E2-4B72-AB5B-9D79EC40A496",
  "67601A62-978F-4BDA-A46B-FB3BEE7AC4EE", "893F3629-7B0B-4830-B3E3-4355E5D71465",
  "8DDAC135-6EA9-4DA0-A09C-8EF11A8C5E6B", "AF318549-A3D5-4736-AC76-63A70F5ADA31",
  "B7879935-90BD-4688-9C3B-5E44BF17D5F0", "1FECA810-30B1-47B8-9821-70A7263B4581",
  "24EE78AF-74EB-4218-BF73-0FB49758D996", "3E7E73B8-4B59-4FFE-9B19-B92E1A77FA1D",
  "829370B6-6AF2-4E1A-AD2B-29570F7BB7A8", "893BA3E4-52C4-4BF4-B0B2-880DB704C13D",
  "B5BE6F7D-557A-47DF-97D4-95E050EAB4CD", "C88CC036-2557-45FD-BE23-983C6EB951D0",
  "FCA97205-3C33-4F0E-8B3D-C3E820398F2C", "FF98993D-DABD-4D27-9991-F38B08AA8687",
  "06F09A12-3B66-4D96-8225-E2CB8A94606F", "6096876D-5CE0-4E5A-A458-15964482A75C",
  "F54106BE-11A8-44A8-AE8A-7B51D1E9367D", "3F3ACAC0-5011-46BD-ACF4-6E40AF8614A1"
)


# Find duplicated rows based on Patient_UUID
duplicated_rows_noCVD <- combined_noCVD.56 %>%
  group_by(Patient_UUID) %>%
  filter(n() > 1)  # Keep only those with more than one row for the same Patient_UUID


###############################################################################################

# BPV.55.outcome_0 <- BPV.55.final_V2 %>%
#   filter(outcome == 0) %>%
#   select(Patient_UUID, outcome_date, DateOfBirth, Gender, outcome_age, outcome)
#
# BPV.55.outcome_0 <- BPV.55.outcome_0 %>%
#   mutate(DIAGNOSIS_CODE = NA_character_,
#          Reason = NA_character_)
#
# final_outcome_CVD <- rbind (outcome_1_cvd_55, BPV.55.outcome_0)
#
# write.csv (final_outcome_CVD, "final_outcome_CVD_55", row.names = FALSE)

bp_sd_less55_CVD <- bp_sd_less55_for55

distinct.bp_sd_less55 <- bp_sd_less55_CVD %>%
  distinct (Patient_UUID)

bp_sd_less55_CVD <- bp_sd_less55_CVD %>%
  mutate(Patient_UUID = as.character(Patient_UUID))

bp_counts <- bp_sd_less55_CVD %>%
  group_by(Patient_UUID) %>%
  tally(name = "bp_pair_count")

bp_counts_5 <- bp_counts %>%
  filter(bp_pair_count >= 4)

intersected_55_CVD <- bp_counts_5 %>%
  inner_join(final_outcome_CVD_55, by = "Patient_UUID")

intersected_55_CVD <- intersected_55_CVD %>%
  mutate(
    # 1. The date the person turns 55
    date_turns_55 = as.Date(DateOfBirth) %m+% years(55),
    # 2. The end of the month in which they turn 55
    end_of_birth_month = ceiling_date(date_turns_55, "month") - days(1),
    # 3. Number of days from end_of_birth_month to the outcome_date
    time_ms = ceiling(interval(date_turns_55, outcome_date) / months(1))
  )

intersected_55_CVD_final <- intersected_55_CVD %>%
  filter(time_ms > 0)

table(intersected_55_CVD_final$outcome)

write.csv (intersected_55_CVD_final, "cvd_55_no_BP_V2", row.names = FALSE)

