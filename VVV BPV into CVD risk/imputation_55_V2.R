library (mice)
library (dplyr)
library (stringr)


BPV_55_3022 <- BPV_55_3022 %>%
  mutate(
    egfr_num = case_when(
      grepl("<", egfr) ~ as.numeric(gsub("[^0-9.]", "", egfr)),  # minimum
      grepl(">", egfr) ~ as.numeric(gsub("[^0-9.]", "", egfr)),  # maximum
      TRUE ~ as.numeric(egfr)
    )
  )


impute_data <- BPV_55_3022 %>%
  mutate(
    # --- Clean SmokingStatus ---
    SmokingStatus = na_if(SmokingStatus, "Not Recorded"),
    SmokingStatus = na_if(SmokingStatus, "Missing"),
    SmokingStatus = as.factor(SmokingStatus),

    # --- Ensure other key variables are correct types ---
    Gender = as.factor(Gender),
    ValueHDL = as.numeric(ValueHDL),
    Value_fglucose = as.numeric(Value_fglucose),
    tr_fglucose = log (Value_fglucose),
    egfr_num = as.numeric(egfr_num),

    # --- Clean bmi_cleaned ---
    bmi_cleaned = bmi %>%
      as.character() %>%
      str_trim() %>%
      tolower() %>%
      str_replace_all("kg", "") %>%
      str_replace_all("[^0-9.]", "") %>%
      na_if("") %>%
      as.numeric(),
    bmi_cleaned = ifelse(bmi_cleaned == 0 | bmi_cleaned < 15 | bmi_cleaned > 45, NA, bmi_cleaned),
    tr_bmi = log (bmi_cleaned)
  ) %>%
  # *** IMPORTANT: Include 'outcome' in your select statement ***
  select(Patient_UUID, ValueHDL, tr_fglucose, egfr_num, tr_bmi, SmokingStatus, outcome, Gender,
         mean_S, mean_D, com_ht, com_dm, com_lip, med_ht, med_dm, med_lipid, med_coag, time_ms,
         sd_S, sd_D, sd_S_std, sd_D_std, cv_S, cv_D, cv_S_std, cv_D_std, arv_S, arv_D, arv_S_std, arv_D_std)


# Run MICE setup and imputation
init <- mice(impute_data, maxit = 0)
methods <- init$method
methods["ValueHDL"] <- "norm"
methods["tr_fglucose"] <- "norm"
methods["egfr_num"] <- "pmm"
methods["tr_bmi"] <- "norm"
methods["SmokingStatus"] <- "polyreg"

# Check final methods
print(methods)

imputed <- mice(impute_data, m = 20, method = methods, seed = 123, maxit = 100)

saveRDS(imputed, "imputed_S2_V2.rds")

plot (imputed)
densityplot(imputed)
bwplot (imputed)

?mice


#mice# models sd_S_std
# Step 6: Fit models to each dataset

#### 1.Unadjusted #####

fitted_models_sd_S_std_1 <- with(imputed,coxph(Surv(time_ms, outcome) ~ sd_S_std))
fitted_models_sd_S_std_1

# check proportional hazard (score test)
model_check_sd_S_std_1 <- cox.zph(fitted_models_sd_S_std_1$analyses[[2]])
model_check_sd_S_std_1

plot(model_check_sd_S_std_1)

# Step 7: Combine result from imputed dataset
est_sd_S_std_1 <- pool(fitted_models_sd_S_std_1)
summary(est_sd_S_std_1)

# models sd_D_std
# Step 6: Fit models to each dataset

fitted_models_sd_D_std_1 <- with(imputed,coxph(Surv(time_ms, outcome) ~ sd_D_std))
fitted_models_sd_D_std_1

# check proportional hazard (score test)
model_check_sd_D_std_1 <- cox.zph(fitted_models_sd_D_std_1$analyses[[2]])
model_check_sd_D_std_1

plot(model_check_sd_D_std_1)

# Step 7: Combine result from imputed dataset
est_sd_D_std_1 <- pool(fitted_models_sd_D_std_1)
summary(est_sd_D_std_1)

# models cv_S_std
# Step 6: Fit models to each dataset

fitted_models_cv_S_std_1 <- with(imputed,coxph(Surv(time_ms, outcome) ~ cv_S_std))
fitted_models_cv_S_std_1

# check proportional hazard (score test)
model_check_cv_S_std_1 <- cox.zph(fitted_models_cv_S_std_1$analyses[[2]])
model_check_cv_S_std_1

plot(model_check_cv_S_std_1)

# Step 7: Combine result from imputed dataset
est_cv_S_std_1 <- pool(fitted_models_cv_S_std_1)
summary(est_cv_S_std_1)

# models cv_S_std
# Step 6: Fit models to each dataset

fitted_models_cv_S_std_1 <- with(imputed,coxph(Surv(time_ms, outcome) ~ cv_S_std))
fitted_models_cv_S_std_1

# check proportional hazard (score test)
model_check_cv_S_std_1 <- cox.zph(fitted_models_cv_S_std_1$analyses[[2]])
model_check_cv_S_std_1

plot(model_check_cv_S_std_1)

# Step 7: Combine result from imputed dataset
est_cv_S_std <- pool(fitted_models_cv_S_std_1)
summary(est_cv_S_std_1)

# models cv_D_std
# Step 6: Fit models to each dataset

fitted_models_cv_D_std_1 <- with(imputed,coxph(Surv(time_ms, outcome) ~ cv_D_std))
fitted_models_cv_D_std_1

# check proportional hazard (score test)
model_check_cv_D_std_1 <- cox.zph(fitted_models_cv_D_std_1$analyses[[2]])
model_check_cv_D_std_1

plot(model_check_cv_D_std_1)

# Step 7: Combine result from imputed dataset
est_cv_D_std_1 <- pool(fitted_models_cv_D_std_1)
summary(est_cv_D_std_1)

############# Adjusted for MEAN AND GENDER #######################

# models sd_S_std, mean, and gender
# Step 6: Fit models to each dataset

fitted_models_sd_S_2 <- with(imputed,coxph(Surv(time_ms, outcome) ~ sd_S_std + Gender + mean_S))
fitted_models_sd_S_2

# check proportional hazard (score test)
model_check_sd_S_2 <- cox.zph(fitted_models_sd_S_2$analyses[[2]])
model_check_sd_S_2

plot(model_check_sd_2)

# Step 7: Combine result from imputed dataset
est_sd_S_std_2 <- pool(fitted_models_sd_S_2)
summary(est_sd_S_std_2)

# models sd_D_std
# Step 6: Fit models to each dataset

fitted_models_sd_D_std_2 <- with(imputed,coxph(Surv(time_ms, outcome) ~ sd_D_std + Gender + mean_D))
fitted_models_sd_D_std_2

# check proportional hazard (score test)
model_check_sd_D_std_2 <- cox.zph(fitted_models_sd_D_std_2$analyses[[2]])
model_check_sd_D_std_2

plot(model_check_sd_D_std_2)

# Step 7: Combine result from imputed dataset
est_sd_D_std_2 <- pool(fitted_models_sd_D_std_2)
summary(est_sd_D_std_2)

# models cv_S_std
# Step 6: Fit models to each dataset

fitted_models_cv_S_std_2 <- with(imputed,coxph(Surv(time_ms, outcome) ~ cv_S_std + Gender + mean_S))
fitted_models_cv_S_std_2

# check proportional hazard (score test)
model_check_cv_S_std_2 <- cox.zph(fitted_models_cv_S_std_2$analyses[[2]])
model_check_cv_S_std_2

plot(model_check_cv_S_std_2)

# Step 7: Combine result from imputed dataset
est_cv_S_std_2 <- pool(fitted_models_cv_S_std_2)
summary(est_cv_S_std_2)

# models cv_D_std
# Step 6: Fit models to each dataset

fitted_models_cv_D_std_2 <- with(imputed,coxph(Surv(time_ms, outcome) ~ cv_D_std + Gender + mean_D))
fitted_models_cv_D_std_2

# check proportional hazard (score test)
model_check_cv_D_std_2 <- cox.zph(fitted_models_cv_D_std_2$analyses[[2]])
model_check_cv_D_std_2

plot(model_check_cv_D_std_2)

# Step 7: Combine result from imputed dataset
est_cv_D_std_2 <- pool(fitted_models_cv_D_std_2)
summary(est_cv_D_std_2)

imputed_55 <-readRDS("imputed_55.rds")




