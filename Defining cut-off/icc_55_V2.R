library("dplyr")
library("lubridate")
library("eeptools")
library("data.table")
library("stringr")
library("writexl")
library("xlsx")
library(survival)
library(ggplot2)
library(purrr)
library(tidyr)
library(irr)
library(psych)
library(tibble)

install.packages("Matrix", type = "source")

remove.packages("Matrix")
install.packages("Matrix", type = "source")

install.packages("https://cran.r-project.org/src/contrib/Archive/Matrix/Matrix_1.5-4.tar.gz",
                 repos = NULL, type = "source")

packageVersion("Matrix")


# Step 1: Filter eligible_analisis_55_final to only include Patient_UUIDs in BPV_55_med_com_RF_lab_timems_final
filtered_BPV.55_3022 <- eligible_analysis_55_final %>%
  filter(Patient_UUID %in% BPV.55_3022$Patient_UUID)

distinct_filtered_eligible_analysis_55_final <- filtered_BPV.55_3022 %>%
  distinct (Patient_UUID)

filtered_eligible_analysis_55_final %>%
  summarise(
    min_S  = min(ObservationValue_S, na.rm = TRUE),
    max_S  = max(ObservationValue_S, na.rm = TRUE),
    mean_S = mean(ObservationValue_S, na.rm = TRUE),
    sd_S   = sd(ObservationValue_S, na.rm = TRUE),

    min_D  = min(ObservationValue_D, na.rm = TRUE),
    max_D  = max(ObservationValue_D, na.rm = TRUE),
    mean_D = mean(ObservationValue_D, na.rm = TRUE),
    sd_D   = sd(ObservationValue_D, na.rm = TRUE)
  )


# Step 1: Count the number of non-NA S and D observations per patient
patient_counts <- filtered_BPV.55_3022 %>%
  group_by(Patient_UUID) %>%
  summarise(
    count_S = sum(!is.na(ObservationValue_S)),
    count_D = sum(!is.na(ObservationValue_D)),
    .groups = "drop"
  )

# Step 2: Summarise stats for number of measurements
patient_counts %>%
  summarise(
    min_S  = min(count_S),
    max_S  = max(count_S),
    mean_S = mean(count_S),
    sd_S   = sd(count_S),

    min_D  = min(count_D),
    max_D  = max(count_D),
    mean_D = mean(count_D),
    sd_D   = sd(count_D)
  )


# Step 1: Count systolic and diastolic measurements per patient
patient_counts <- filtered_BPV.55_3022 %>%
  group_by(Patient_UUID) %>%
  summarise(
    count_S = sum(!is.na(ObservationValue_S)),
    count_D = sum(!is.na(ObservationValue_D)),
    .groups = "drop"
  )

# Step 2: Filter only those with both S and D counts >= 3
filtered_counts <- patient_counts %>%
  filter(count_S >= 3, count_D >= 3)


# Step 3: Get cumulative patient counts from 5 to 20
cumulative_summary <- tibble(n = 3:20) %>%
  rowwise() %>%
  mutate(
    num_patients_S = sum(filtered_counts$count_S >= n),
    num_patients_D = sum(filtered_counts$count_D >= n)
  )

# View the table
print(cumulative_summary)

write.csv (cumulative_summary, "cumulative_summary_icc", row.names = FALSE)


# Assume filtered_eligible_analysis_55_final has these columns:
# Patient_UUID, ObservationDate, ObservationValue_S

# Step 1: Keep only systolic values and sort by date
systolic_data <- filtered_BPV.55_3022 %>%
  filter(!is.na(ObservationValue_S)) %>%
  arrange(Patient_UUID, ObservationDate)

# Step 2: For each n (from 3 to 12), calculate sd of earliest n readings per patient
sd_list <- map(3:12, function(n) {
  systolic_data %>%
    group_by(Patient_UUID) %>%
    slice_head(n = n) %>%
    summarise(sd_S = sd(ObservationValue_S), .groups = "drop") %>%
    rename(!!paste0("sd_S_", n) := sd_S)
})

# Step 3: Combine into a wide table (one row per patient)
sd_table <- reduce(sd_list, full_join, by = "Patient_UUID")

write.csv (sd_table, "sd_table_icc", row.names = FALSE)

mean_sd_table <- sd_table %>%
  summarise(across(starts_with("sd_S_"), list(mean = ~mean(.x, na.rm = TRUE),
                                              sd = ~sd(.x, na.rm = TRUE)))) %>%
  pivot_longer(everything(),
               names_to = c("n", ".value"),
               names_pattern = "sd_S_(\\d+)_(mean|sd)") %>%
  mutate(n = as.integer(n)) %>%
  arrange(n)



###############adding r and p value ##########

sd_table <- sd_table_icc

results_icc <- map_dfr(3:11, function(n) {
  col_n <- paste0("sd_S_", n)

  df_pair <- sd_table %>%
    select(sd_n = all_of(col_n), sd_12 = sd_S_12) %>%
    drop_na()

  # Pearson correlation
  cor_test <- cor.test(df_pair$sd_n, df_pair$sd_12, method = "pearson")

  # Mean difference
  delta <- df_pair$sd_n - df_pair$sd_12
  delta_mean <- mean(delta)
  delta_sd <- sd(delta)

  # ICC (from irr)
  icc_out <- irr::icc(df_pair, model = "twoway", type = "consistency", unit = "single")

  tibble(
    n = n,
    ICC = icc_out$value,
    r = cor_test$estimate,
    p_value = cor_test$p.value,
    delta_mean = delta_mean,
    delta_sd = delta_sd
  )
})

print(results_icc)

write.csv (results, "icc_results.csv", row.names = FALSE)
