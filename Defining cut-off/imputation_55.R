library (mice)




# Step 1: Prepare data — convert "Not Recorded" to NA
impute_data <- BPV_55 %>%
  mutate(
    SmokingStatus = if_else(SmokingStatus %in% c("Not Recorded", "Missing"), NA_character_, SmokingStatus)
  ) %>%
  select(ValueHDL, outcome_age, Gender, mean_S, mean_D, com_ht, com_dm, com_lip, SmokingStatus) %>%
  mutate(
    SmokingStatus = as.factor(SmokingStatus),
    Gender = as.factor(Gender)
  )


# Step 2: Initial run to configure method
init <- mice(impute_data, maxit = 0)

# Step 3: Update method to skip imputation for Gender
methods <- init$method
methods["Gender"] <- ""  # Skip imputation for Gender


# Step 4: Run imputation
imputed <- mice(impute_data, m = 5, method = methods, seed = 123)

# Step 5: Fit a model using all imputed datasets (optional)
# Linear regression of ValueHDL
fit <- with(imputed, lm(ValueHDL ~ outcome_age + Gender + mean_S + mean_D + com_ht + com_dm + com_lip + SmokingStatus))

# Step 6: Pool the results
pooled_results <- pool(fit)
summary(pooled_results)

# Step 7: Extract completed dataset (use first imputation for reintegration)
completed_data <- complete(imputed, 1)

# Step 8: Replace the imputed columns in the original dataset
# Remove original columns to avoid duplication
original_data <- BPV_55 %>%
  select(-ValueHDL, -SmokingStatus)

# Step 9: Combine with imputed columns
final_data <- bind_cols(original_data, completed_data %>% select(ValueHDL, SmokingStatus))


sum(is.na(impute_data$ValueHDL))  # should be > 0
imputed$method

methods["ValueHDL"] <- "pmm"

imputed <- mice(impute_data, m = 5, method = methods, seed = 123)
completed_data <- complete(imputed, 1)

pred <- make.predictorMatrix(impute_data)
pred

str(impute_data$ValueHDL)

impute_data$ValueHDL <- as.numeric(impute_data$ValueHDL)

str(impute_data$ValueHDL)
# Should say:  num [1:3243] NA NA NA 3.7 4.1 ...

impute_data <- BPV_55_med_com_RF_lab %>%
  mutate(
    SmokingStatus = na_if(SmokingStatus, "Not Recorded"),
    SmokingStatus = as.factor(SmokingStatus),
    Gender = as.factor(Gender),
    ValueHDL = as.numeric(ValueHDL)  # <-- convert here
  ) %>%
  select(ValueHDL, outcome_age, Gender, mean_S, mean_D, com_ht, com_dm, com_lip, SmokingStatus)

init <- mice(impute_data, maxit = 0)
methods <- init$method
methods["ValueHDL"] <- "pmm"
methods["SmokingStatus"] <- "polyreg"
methods["Gender"] <- ""

imputed <- mice(impute_data, m = 5, method = methods, seed = 123)


################# Checking MAR or MNAR #####################



