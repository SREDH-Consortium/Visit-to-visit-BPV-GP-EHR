library(CVrisk)
library(mice)
library(dplyr)

# Take the first imputed dataset for inspection
data_check <- complete(imputed, 1)

# Find rows with invalid TC (<=0 or NA)
invalid_chol <- data_check %>%
  filter(is.na(TC_Value) | TC_Value <= 0) %>%
  select(Patient_UUID, TC_Value, HDL_Value, age, ObservationValue_S, med_ht, SmokingStatus, com_dm)

# View them
print(invalid_chol)

sapply(1:imputed$m, function(i) class(complete(imputed, i)$TC_Value))



# ---- Step 1: Calculate ASCVD risk per imputation ----
imputed_list <- lapply(1:imputed$m, function(i) {
  data <- complete(imputed, i)
  
  # Fix invalid cholesterol by replacing with median of valid values
  median_tc <- median(data$TC_Value[data$TC_Value > 0], na.rm = TRUE)
  data$TC_Value <- ifelse(is.na(data$TC_Value) | data$TC_Value <= 0, median_tc, data$TC_Value)
  
  # Ensure binary numeric coding for categorical vars
  data <- data %>%
    mutate(
      SmokingStatus = ifelse(SmokingStatus %in% c("Yes", "1"), 1, 0),
      med_ht = ifelse(med_ht %in% c("Yes", "1"), 1, 0),
      com_dm = ifelse(com_dm %in% c("Yes", "1"), 1, 0)
    )
  
  # Calculate risk
  data$ascvd_10y <- ascvd_10y_frs(
    gender    = data$Gender,
    age       = data$age,
    hdl       = data$HDL_Value,
    totchol   = data$TC_Value,
    sbp       = data$ObservationValue_S,
    bp_med    = data$med_ht,
    smoker    = data$SmokingStatus,
    diabetes  = data$com_dm
  )
  
  data %>% select(Patient_UUID, ascvd_10y)
})

# ---- Step 2: Merge risks across imputations ----
risk_all <- Reduce(function(x, y) full_join(x, y, by = "Patient_UUID"), imputed_list)
colnames(risk_all)[-1] <- paste0("risk_imp", 1:imputed$m)

# ---- Step 3: Pool risk per patient (Rubin: mean across imputations) ----
risk_all <- risk_all %>%
  rowwise() %>%
  mutate(
    pooled_risk = mean(c_across(starts_with("risk_imp")), na.rm = TRUE)
  ) %>%
  ungroup()

# ---- Step 4: Add percentage and classify into risk groups ----
risk_all <- risk_all %>%
  mutate(
    risk_percent = pooled_risk,
    risk_group = case_when(
      risk_percent < 10 ~ "Low (<10%)",
      risk_percent >= 10 & risk_percent < 20 ~ "Intermediate (10%-20%)",
      risk_percent >= 20 ~ "High (≥20%)"
    )
  )

# ---- Step 5: Final dataset ----
final_ascvd <- risk_all %>%
  select(Patient_UUID, pooled_risk, risk_percent, risk_group)

# View result
head(final_ascvd)

write.csv (final_ascvd, "final_ascvd.csv", row.names = FALSE)

