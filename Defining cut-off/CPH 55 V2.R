#################### # CPH analysis using SD adjusted for medication and ht #################

#################### Systolic ###########################

# Step 1: Standardize the SD SBP variable
BPV_55$sd_S_std <- scale(BPV_55$sd_S)

# Step 2: Fit the Cox proportional hazards model
sd_S_std_gender_med_ht <- coxph(Surv(outcome_obs_days, outcome) ~ sd_S_std + Gender + med_ht + med_dm + med_lipid + med_coag + ht, data = BPV_55_med_com)

# Step 3: Display the results
summary(sd_S_std_gender_med_ht)

# Extract hazard ratio and confidence interval
hr_sd_S_std <- exp(coef(sd_S_std_unadj)["sd_S_std"])
ci_sd_S_std <- exp(confint(sd_S_std_unadj)["sd_S_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in CV SBP:", hr_sd_S_std, "\n")
cat("95% Confidence Interval:", ci_sd_S_std[1], "-", ci_sd_S_std[2], "\n")

################# Diastolic ###################

# Step 1: Standardize the SD DBP variable
BPV_55$sd_D_std <- scale(BPV_55$sd_D)

# Step 2: Fit the Cox proportional hazards model
sd_D_std_gender_med_ht <- coxph(Surv(outcome_obs, outcome) ~ sd_D_std + Gender + med_ht + med_dm + med_lipid + med_coag + ht, data = BPV_55_med_com)

# Step 3: Display the results
summary(sd_D_std_gender_med_ht)

# Extract hazard ratio and confidence interval
hr_sd_D_std <- exp(coef(sd_D_std_unadj)["sd_D_std"])
ci_sd_D_std <- exp(confint(sd_D_std_unadj)["sd_D_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in SD SBP:", hr_sd_D_std, "\n")
cat("95% Confidence Interval:", ci_sd_D_std[1], "-", ci_sd_D_std[2], "\n")

############### each mmHg increase #####################

# Step 1: Standardize the CV SBP variable
# BPV_55$cv_S_std <- scale(BPV_55$cv_S)

# Step 2: Fit the Cox proportional hazards model
sd_S_unadj <- coxph(Surv(outcome_obs, outcome) ~ sd_S, data = BPV_55)

# Step 3: Display the results
summary(sd_S_unadj)

# Extract hazard ratio and confidence interval
hr_sd_S <- exp(coef(sd_S_unadj)["sd_S"])
ci_sd_S <- exp(confint(sd_S_unadj)["sd_S", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in CV SBP:", hr_sd_S, "\n")
cat("95% Confidence Interval:", ci_sd_S[1], "-", ci_sd_S[2], "\n")

################# Diastolic ###################

# Step 1: Standardize the CV DBP variable
# BPV_55$cv_D_std <- scale(BPV_55$cv_D)

# Step 2: Fit the Cox proportional hazards model
sd_D_unadj <- coxph(Surv(outcome_obs, outcome) ~ sd_D, data = BPV_55)

# Step 3: Display the results
summary(sd_D_unadj)

# Extract hazard ratio and confidence interval
hr_sd_D <- exp(coef(sd_D_unadj)["sd_D"])
ci_sd_D <- exp(confint(sd_D_unadj)["sd_D", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in sd DBP:", hr_sd_D, "\n")
cat("95% Confidence Interval:", ci_sd_D[1], "-", ci_sd_D[2], "\n")
