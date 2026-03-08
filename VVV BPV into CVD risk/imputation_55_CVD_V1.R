
BPV_55_CVD_5BP <- BPV_55_CVD_5BP_med_com_RF_lab


latest_bp <- eligible_analysis_CVD_55_S3_5_BP_final %>%
  group_by(Patient_UUID) %>%
  filter(ObservationDate == max(ObservationDate, na.rm = TRUE)) %>%
  slice_tail(n = 1) %>%  # In case of multiple rows with same max date, keep the last one
  ungroup() %>%
  select(Patient_UUID, ObservationValue_S, ObservationValue_D)


latest_bp_CVD_5BP <- latest_bp %>%
  filter(Patient_UUID %in% BPV_55_CVD_5BP$Patient_UUID)

BPV_55_CVD_5BP <- BPV_55_CVD_5BP %>%
  left_join(latest_bp_CVD_5BP, by = "Patient_UUID")
  


BPV_55_CVD_5BP <- BPV_55_CVD_5BP %>%
  mutate(
    egfr = egfr %>%
      as.character() %>%
      str_trim() %>%
      str_replace_all("^>", "") %>%        # remove leading ">"
      str_replace_all(">", "") %>%         # remove any remaining ">"
      str_replace_all("[^0-9.]", "") %>%   # keep only numbers and dots
      na_if("") %>%
      as.numeric()
  )

BPV_55_CVD_5BP <- BPV_55_CVD_5BP %>%
  mutate(
    FH = case_when(
      FH == 1 ~ "Yes",
      FH == 0 ~ "No",
      TRUE ~ NA_character_
    ),
    FH = as.factor(FH)
  )

BPV_55_CVD_5BP <- BPV_55_CVD_5BP %>%
  mutate(
  bmi_c = bmi %>%
  as.character() %>%
  str_trim() %>%
  tolower() %>%
  str_replace_all("kg", "") %>%
  str_replace_all("[^0-9.]", "") %>%
  na_if("") %>%
  as.numeric(),
  bmi_c = ifelse(bmi_c == 0 | bmi_c < 10 | bmi_c > 80, NA, bmi_c),
  tr_bmi = log (bmi_c)
  )

BPV_55_CVD_5BP <- BPV_55_CVD_5BP %>%
  mutate(
    tr_fglucose = log (Value_fglucose),
    tr_bmi = log (bmi_c),
    tr_egfr = log (egfr)
  )

BPV_55_CVD_5BP <- BPV_55_CVD_5BP %>%
  mutate(
    tr_HDL_value = log (HDL_Value)
    )

library(ggplot2)
library(e1071)  # for skewness

# Calculate skewness

BPV_55_CVD_5BP$Value_fglucose = as.numeric(BPV_55_CVD_5BP$Value_fglucose)

glucose_skew <- skewness(BPV_55_CVD_5BP$Value_fglucose, na.rm = TRUE)
print(glucose_skew)

# Plot histogram + density
ggplot(BPV_55_CVD_5BP, aes(x = Value_fglucose)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(
    title = paste0("Distribution of Fasting Glucose (Skewness = ", round(glucose_skew, 2), ")"),
    x = "Fasting Glucose (mmol/L)",
    y = "Density"
  ) +
  theme_minimal()

########### BMI ################

BPV_55_CVD_5BP$bmi = as.numeric(BPV_55_CVD_5BP$bmi)

bmi_skew <- skewness(BPV_55_CVD_5BP$bmi, na.rm = TRUE)
print(bmi_skew)

# Plot histogram + density
ggplot(BPV_55_CVD_5BP, aes(x = bmi)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(
    title = paste0("Distribution of BMI (Skewness = ", round(bmi_skew, 2), ")"),
    x = "BMI",
    y = "Density"
  ) +
  theme_minimal()

########### Value_TCHDL ###########

BPV_55_CVD_5BP$Value_TCHDL = as.numeric(BPV_55_CVD_5BP$Value_TCHDL)

TCHDL_skew <- skewness(BPV_55_CVD_5BP$Value_TCHDL, na.rm = TRUE)
print(TCHDL_skew)

# Plot histogram + density
ggplot(BPV_55_CVD_5BP, aes(x = Value_TCHDL)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(
    title = paste0("Distribution of TCHDL ratio (Skewness = ", round(TCHDL_skew, 2), ")"),
    x = "TCHDL ratio (mmol/L)",
    y = "Density"
  ) +
  theme_minimal()


########### TC_Value ###########

BPV_55_CVD_5BP$TC_Value = as.numeric(BPV_55_CVD_5BP$TC_Value)

TC_skew <- skewness(BPV_55_CVD_5BP$TC_Value, na.rm = TRUE)
print(TC_skew)

# Plot histogram + density
ggplot(BPV_55_CVD_5BP, aes(x = TC_Value)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(
    title = paste0("Distribution of TC ratio (Skewness = ", round(TC_skew, 2), ")"),
    x = "TC value (mmol/L)",
    y = "Density"
  ) +
  theme_minimal()


########### HDL_Value ###########

BPV_55_CVD_5BP$HDL_Value = as.numeric(BPV_55_CVD_5BP$HDL_Value)

HDL_skew <- skewness(BPV_55_CVD_5BP$HDL_Value, na.rm = TRUE)
print(HDL_skew)

# Plot histogram + density
ggplot(BPV_55_CVD_5BP, aes(x = HDL_Value)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(
    title = paste0("Distribution of HDL ratio (Skewness = ", round(HDL_skew, 2), ")"),
    x = "HDL value (mmol/L)",
    y = "Density"
  ) +
  theme_minimal()

############ egfr ###################

BPV_55_CVD_5BP$egfr = as.numeric(BPV_55_CVD_5BP$egfr)

egfr_skew <- skewness(BPV_55_CVD_5BP$egfr, na.rm = TRUE)
print(egfr_skew)

# Plot histogram + density
ggplot(BPV_55_CVD_5BP, aes(x = egfr)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "steelblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(
    title = paste0("Distribution of BMI (Skewness = ", round(egfr_skew, 2), ")"),
    x = "egfr",
    y = "Density"
  ) +
  theme_minimal()

################ imputation ###############
impute_data_CVD <- BPV_55_CVD_5BP %>%
  select (Patient_UUID, mean_S, mean_D, sd_S, sd_D, cv_S, cv_D, Gender, outcome, time_ms, com_ht, com_dm, 
          com_lip, med_ht, med_dm, med_lipid, med_coag, tr_fglucose, tr_bmi, tr_egfr, egfr, Value_TCHDL, 
          TC_Value, HDL_Value, tr_HDL_value, FH, SmokingStatus, ObservationValue_S, ObservationValue_D, age)

# Run MICE setup and imputation
init <- mice(impute_data_CVD, maxit = 0)
methods <- init$method
methods["Value_TCHDL"] <- "norm"
methods["TC_Value"] <- "norm"
methods["tr_HDL_value"] <- "norm"
methods["tr_fglucose"] <- "norm"
methods["egfr"] <- "pmm"
methods["tr_bmi"] <- "norm"
methods["SmokingStatus"] <- "polyreg"
methods ["FH"] <- "polyreg"



# Check final methods
print(methods)

imputed <- mice(impute_data_CVD, m = 20, method = methods, seed = 123, maxit = 100)

plot (imputed)
densityplot(imputed)
bwplot (imputed)

saveRDS(imputed, "imputed_55_CVD_complete.rds")

############# using rf ################

imputed_rf <- mice(impute_data_CVD, m = 20, method = "rf", seed = 123, maxit = 100)

plot (imputed_rf)
densityplot(imputed_rf)
bwplot (imputed_rf)

saveRDS(imputed_rf, "imputed_55_CVD_rf.rds")

library(mice)
library(dplyr)


# 1. Combine all imputations into one long dataset
combined_data <- complete(imputed, action = "long", include = FALSE)

# 2. Split into train/test for each imputation
set.seed(123)  # for reproducibility
splits <- combined_data %>%
  group_by(.imp) %>%
  mutate(split = sample(c("train", "test"), 
                        size = n(), 
                        replace = TRUE, 
                        prob = c(0.7, 0.3))) %>%
  ungroup()

# 3. Create train and test datasets separately
train_data <- splits %>% filter(split == "train") %>% select(-split)
test_data  <- splits %>% filter(split == "test") %>% select(-split)

