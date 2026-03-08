library (survival)
library (ggplot2)
library(writexl)
library (car)

############# Kaplan Meier ##########

# fix time to be days from age 55 to evedata:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABIAAAASCAYAAABWzo5XAAAAbElEQVR4Xs2RQQrAMAgEfZgf7W9LAguybljJpR3wEse5JOL3ZObDb4x1loDhHbBOFU6i2Ddnw2KNiXcdAXygJlwE8OFVBHDgKrLgSInN4WMe9iXiqIVsTMjH7z/GhNTEibOxQswcYIWYOR/zAjBJfiXh3jZ6AAAAAElFTkSuQmCCnt


write_xlsx(BPV_55_cph, "BPV_55_cph.xlsx")


km_fit <- survfit(Surv(time_ms, outcome) ~ 1, data=BPV_55_med_com_RF_lab_timems_final)
plot(km_fit)

# Check the class of outcome in your main data
class(BPV_55_cph$outcome)

# If it's a factor or character, check its unique values
if (is.factor(BPV_55_cph$outcome) || is.character(BPV_55_cph$outcome)) {
  levels(BPV_55_cph$outcome) # If it's a factor
  unique(BPV_55_cph$outcome) # If it's a character or factor
}

# Before defining your base_formula and running any coxph models:

# Option 1: Convert from factor/character to numeric
# This is generally the safest way if you know your levels are "0" and "1"
BPV_55_cph$outcome <- as.numeric(as.character(BPV_55_cph$outcome))

# Verify the conversion
class(BPV_55_cph$outcome)
unique(BPV_55_cph$outcome) # Should now show 0 and 1 (numeric)

# Make sure there are no NAs introduced by coercion if there were unexpected values
sum(is.na(BPV_55_cph$outcome))


########### CPH analysis using mean ###############

# Step 1: Standardize the mean_S variable
BPV_55_cph$mean_S_std <- scale(BPV_55_cph$mean_S)


# Step 2: Fit the Cox proportional hazards model
mean_S_std_unadj <- coxph(Surv(time_ms, outcome) ~ mean_S_std, data = BPV_55)

# Step 3: Display the results
summary(mean_S_std_unadj)


# Extract hazard ratio and confidence interval
hr <- exp(coef(mean_S_std_unadj)["mean_S_std"])
ci <- exp(confint(mean_S_std_unadj)["mean_S_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in mean SBP:", hr, "\n")
cat("95% Confidence Interval:", ci[1], "-", ci[2], "\n")

########### Diastolic ###############

# Step 1: Standardize the mean_S variable
BPV_55$mean_D_std <- scale(BPV_55$mean_D)

# Step 2: Fit the Cox proportional hazards model
mean_D_std_unadj <- coxph(Surv(time_ms, outcome) ~ mean_D_std, data = BPV_55_cph)

# Step 3: Display the results
summary(mean_D_std_unadj)


# Extract hazard ratio and confidence interval
hr_meanD <- exp(coef(mean_D_std_unadj)["mean_D_std"])
ci_meanD <- exp(confint(mean_D_std_unadj)["mean_D_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in mean D SBP:", hr_meanD, "\n")
cat("95% Confidence Interval:", ci_meanD[1], "-", ci_meanD[2], "\n")


#################### # CPH analysis using SD #################

#################### Systolic ###########################

# Step 1: Standardize the SD SBP variable
BPV_55$sd_S_std <- scale(BPV_55$sd_S)

# Step 2: Fit the Cox proportional hazards model
sd_S_std_unadj <- coxph(Surv(time_ms, outcome) ~ sd_S_std, data = BPV_55_cph)

# Step 3: Display the results
summary(sd_S_std_unadj)

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
sd_D_std_unadj <- coxph(Surv(time_ms, outcome) ~ sd_D_std, data = BPV_55_cph)

# Step 3: Display the results
summary(sd_D_std_unadj)

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
sd_S_unadj <- coxph(Surv(time_ms, outcome) ~ sd_S, data = BPV_55_cph)

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
sd_D_unadj <- coxph(Surv(time_ms, outcome) ~ sd_D, data = BPV_55_cph)

# Step 3: Display the results
summary(sd_D_unadj)

# Extract hazard ratio and confidence interval
hr_sd_D <- exp(coef(sd_D_unadj)["sd_D"])
ci_sd_D <- exp(confint(sd_D_unadj)["sd_D", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in sd DBP:", hr_sd_D, "\n")
cat("95% Confidence Interval:", ci_sd_D[1], "-", ci_sd_D[2], "\n")



#################### # CPH analysis using CV #################

#################### Systolic ###########################

# Step 1: Standardize the CV SBP variable
BPV_55$cv_S_std <- scale(BPV_55$cv_S)

# Step 2: Fit the Cox proportional hazards model
cv_S_std_unadj <- coxph(Surv(time_ms, outcome) ~ cv_S_std, data = BPV_55_cph)

# Step 3: Display the results
summary(cv_S_std_unadj)

# Extract hazard ratio and confidence interval
hr_cv_S_std <- exp(coef(cv_S_std_unadj)["cv_S_std"])
ci_cv_S_std <- exp(confint(cv_S_std_unadj)["cv_S_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in CV SBP:", hr_cv_S_std, "\n")
cat("95% Confidence Interval:", ci_cv_S_std[1], "-", ci_cv_S_std[2], "\n")

################# Diastolic ###################

# Step 1: Standardize the CV DBP variable
BPV_55$cv_D_std <- scale(BPV_55$cv_D)

# Step 2: Fit the Cox proportional hazards model
cv_D_std_unadj <- coxph(Surv(time_ms, outcome) ~ cv_D_std, data = BPV_55_cph)

# Step 3: Display the results
summary(cv_D_std_unadj)

# Extract hazard ratio and confidence interval
hr_cv_D_std <- exp(coef(cv_D_std_unadj)["cv_D_std"])
ci_cv_D_std <- exp(confint(cv_D_std_unadj)["cv_D_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in cv DBP:", hr_cv_D_std, "\n")
cat("95% Confidence Interval:", ci_cv_D_std[1], "-", ci_cv_D_std[2], "\n")

############### CV each increase #####################

# Step 1: Standardize the CV SBP variable
# BPV_55$cv_S_std <- scale(BPV_55$cv_S)

# Step 2: Fit the Cox proportional hazards model
cv_S_unadj <- coxph(Surv(time_ms, outcome) ~ cv_S, data = BPV_55_cph)

# Step 3: Display the results
summary(cv_S_unadj)

# Extract hazard ratio and confidence interval
hr_cv_S <- exp(coef(cv_S_unadj)["cv_S"])
ci_cv_S <- exp(confint(cv_S_unadj)["cv_S", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in CV SBP:", hr_cv_S, "\n")
cat("95% Confidence Interval:", ci_cv_S[1], "-", ci_cv_S[2], "\n")

################# Diastolic ###################

# Step 1: Standardize the CV DBP variable
# BPV_55$cv_D_std <- scale(BPV_55$cv_D)

# Step 2: Fit the Cox proportional hazards model
cv_D_unadj <- coxph(Surv(time_ms, outcome) ~ cv_D, data = BPV_55_cph)

# Step 3: Display the results
summary(cv_D_unadj)

# Extract hazard ratio and confidence interval
hr_cv_D <- exp(coef(cv_D_unadj)["cv_D"])
ci_cv_D <- exp(confint(cv_D_unadj)["cv_D", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in cv DBP:", hr_cv_D, "\n")
cat("95% Confidence Interval:", ci_cv_D[1], "-", ci_cv_D[2], "\n")


#################### # CPH analysis using ARV #################

#################### Systolic ###########################

# Step 1: Standardize the arv SBP variable
BPV_55$arv_S_std <- scale(BPV_55$arv_S)

# Step 2: Fit the Cox proportional hazards model
arv_S_std_unadj <- coxph(Surv(time_ms, outcome) ~ arv_S_std, data = BPV_55_cph)

# Step 3: Display the results
summary(arv_S_std_unadj)

# Extract hazard ratio and confidence interval
hr_arv_S_std <- exp(coef(arv_S_std_unadj)["arv_S_std"])
ci_arv_S_std <- exp(confint(arv_S_std_unadj)["arv_S_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-arv increase in CV SBP:", hr_arv_S_std, "\n")
cat("95% Confidence Interval:", ci_arv_S_std[1], "-", ci_arv_S_std[2], "\n")

################# Diastolic ###################

# Step 1: Standardize the arv DBP variable
BPV_55$arv_D_std <- scale(BPV_55$arv_D)

# Step 2: Fit the Cox proportional hazards model
arv_D_std_unadj <- coxph(Surv(time_ms, outcome) ~ arv_D_std, data = BPV_55_cph)

# Step 3: Display the results
summary(arv_D_std_unadj)

# Extract hazard ratio and confidence interval
hr_arv_D_std <- exp(coef(arv_D_std_unadj)["arv_D_std"])
ci_arv_D_std <- exp(confint(arv_D_std_unadj)["arv_D_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-arv increase in arv SBP:", hr_arv_D_std, "\n")
cat("95% Confidence Interval:", ci_arv_D_std[1], "-", ci_arv_D_std[2], "\n")

############### each arv increase #####################

# Step 1: Standardize the CV SBP variable
# BPV_55$cv_S_std <- scale(BPV_55$cv_S)

# Step 2: Fit the Cox proportional hazards model
arv_S_unadj <- coxph(Surv(outcome_obs, outcome) ~ arv_S, data = BPV_55)

# Step 3: Display the results
summary(arv_S_unadj)

# Extract hazard ratio and confidence interval
hr_arv_S <- exp(coef(arv_S_unadj)["arv_S"])
ci_arv_S <- exp(confint(arv_S_unadj)["arv_S", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-arv increase in ARV SBP:", hr_arv_S, "\n")
cat("95% Confidence Interval:", ci_arv_S[1], "-", ci_arv_S[2], "\n")

################# Diastolic ###################

# Step 1: Standardize the CV DBP variable
#BPV_55$cv_D_std <- scale(BPV_55$cv_D)

# Step 2: Fit the Cox proportional hazards model
arv_D_unadj <- coxph(Surv(time_ms, outcome) ~ arv_D, data = BPV_55_cph)

# Step 3: Display the results
summary(arv_D_unadj)

# Extract hazard ratio and confidence interval
hr_arv_D <- exp(coef(arv_D_unadj)["arv_D"])
ci_arv_D <- exp(confint(arv_D_unadj)["arv_D", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-arv increase in ARV DBP:", hr_arv_D, "\n")
cat("95% Confidence Interval:", ci_arv_D[1], "-", ci_arv_D[2], "\n")

#############################

#################### # CPH analysis using SD adjusted for gender #################

#################### Systolic ###########################

# Step 1: Standardize the SD SBP variable
BPV_55$sd_S_std <- scale(BPV_55$sd_S)

# Step 2: Fit the Cox proportional hazards model
sd_S_std_gender <- coxph(Surv(time_ms, outcome) ~ sd_S_std + Gender, data = BPV_55_cph)

# Step 3: Display the results
summary(sd_S_std_gender)

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
sd_D_std_gender <- coxph(Surv(time_ms, outcome) ~ sd_D_std + Gender, data = BPV_55_cph)

# Step 3: Display the results
summary(sd_D_std_gender)

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
sd_S_unadj <- coxph(Surv(time_ms, outcome) ~ sd_S, data = BPV_55)

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

#################### # CPH analysis using SD adjusted for medication #################

#################### Systolic ###########################

# Step 1: Standardize the SD SBP variable
BPV_55$sd_S_std <- scale(BPV_55$sd_S)

# Step 2: Fit the Cox proportional hazards model
sd_S_std_gender_med <- coxph(Surv(outcome_obs_days, outcome) ~ sd_S_std + Gender + med_ht + med_dm + med_lipid + med_coag, data = BPV_55_med)

# Step 3: Display the results
summary(sd_S_std_gender_med)

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
sd_D_std_gender <- coxph(Surv(outcome_obs, outcome) ~ sd_D_std + Gender, data = BPV_55)

# Step 3: Display the results
summary(sd_D_std_gender)

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




#################### # CPH analysis using SD adjusted for comorbidities #################

#################### Systolic ###########################

# Step 2: Fit the Cox proportional hazards model
sd_S_std_gender_com <- coxph(Surv(time_ms, outcome) ~ sd_S_std + Gender + com_ht + com_dm + com_lip,
                             data = BPV_55_cph)

# Step 3: Display the results
summary(sd_S_std_gender_com)

# Extract hazard ratio and confidence interval
hr_sd_S_std_gender_com <- exp(coef(sd_S_std_gender_com)["sd_S_std"])
ci_sd_S_std_gender_com <- exp(confint(sd_S_std_gender_com)["sd_S_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in CV SBP:", hr_sd_S_std_gender_com, "\n")
cat("95% Confidence Interval:", ci_sd_S_std_gender_com[1], "-", ci_sd_S_std_gender_com[2], "\n")

################# Diastolic ###################

# Step 2: Fit the Cox proportional hazards model
sd_D_std_gender_com <- coxph(Surv(time_ms, outcome) ~ sd_D_std + Gender + com_ht + com_dm + com_lip,
                             data = BPV_55_cph)

# Step 3: Display the results
summary(sd_D_std_gender_com)

# Extract hazard ratio and confidence interval
hr_sd_D_std_gender_com <- exp(coef(sd_D_std_gender_com)["sd_D_std"])
ci_sd_D_std_gender_com <- exp(confint(sd_D_std_gender_com)["sd_D_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in SD SBP:", hr_sd_D_std_gender_com, "\n")
cat("95% Confidence Interval:", ci_sd_D_std_gender_com[1], "-", ci_sd_D_std_gender_com[2], "\n")

############### each mmHg increase #####################

# Step 2: Fit the Cox proportional hazards model
sd_S_gender_com <- coxph(Surv(time_ms, outcome) ~ sd_S + Gender + com_ht + com_dm + com_lip,
                             data = BPV_55_cph)

# Step 3: Display the results
summary(sd_S_gender_com)

# Extract hazard ratio and confidence interval
hr_sd_S_gender_com <- exp(coef(sd_S_gender_com)["sd_S"])
ci_sd_S_gender_com <- exp(confint(sd_S_gender_com)["sd_S", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in SD SBP:", hr_sd_S_gender_com, "\n")
cat("95% Confidence Interval:", ci_sd_S_gender_com[1], "-", ci_sd_S_gender_com[2], "\n")

################# Diastolic ###################

# Step 2: Fit the Cox proportional hazards model
sd_D_gender_com <- coxph(Surv(time_ms, outcome) ~ sd_D + Gender + com_ht + com_dm + com_lip,
                         data = BPV_55_cph)

# Step 3: Display the results
summary(sd_D_gender_com)

# Extract hazard ratio and confidence interval
hr_sd_D_gender_com <- exp(coef(sd_D_gender_com)["sd_D"])
ci_sd_D_gender_com <- exp(confint(sd_D_gender_com)["sd_D", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in SD SBP:", hr_sd_D_gender_com, "\n")
cat("95% Confidence Interval:", ci_sd_S_gender_com[1], "-", ci_sd_D_gender_com[2], "\n")


#################### # CPH analysis using SD adjusted for comorbidities, lab #################

#################### Systolic ###########################

# Step 2: Fit the Cox proportional hazards model
sd_S_std_gender_com_lab <- coxph(Surv(time_ms, outcome) ~ sd_S_std + Gender + com_ht + com_dm + com_lip +
                                       ValueHDL + egfr + tr_fglucose + tr_bmi, data = BPV_55_cph)

# Step 3: Display the results
summary(sd_S_std_gender_com_lab)

# Extract hazard ratio and confidence interval
hr_sd_S_std_gender_com_lab <- exp(coef(sd_S_std_gender_com_lab)["sd_S_std"])
ci_sd_S_std_gender_com_lab <- exp(confint(sd_S_std_gender_com_lab)["sd_S_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in CV SBP:", hr_sd_S_std_gender_com_lab, "\n")
cat("95% Confidence Interval:", ci_sd_S_std_gender_com_lab[1], "-", ci_sd_S_std_gender_com_lab[2], "\n")

################# Diastolic ###################

# Step 2: Fit the Cox proportional hazards model
sd_D_std_gender_com_lab <- coxph(Surv(time_ms, outcome) ~ sd_D_std + Gender + com_ht + com_dm + com_lip +
                               ValueHDL + egfr + tr_fglucose + tr_bmi, data = BPV_55_cph)

# Step 3: Display the results
summary(sd_D_std_gender_com_lab)

# Extract hazard ratio and confidence interval
hr_sd_D_std_gender_com_lab <- exp(coef(sd_D_std_gender_com_lab)["sd_D_std"])
ci_sd_D_std_gender_com_lab <- exp(confint(sd_D_std_gender_com_lab)["sd_D_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in SD DBP:", hr_sd_D_std_gender_com_lab, "\n")
cat("95% Confidence Interval:", ci_sd_D_std_gender_com_lab[1], "-", ci_sd_D_std_gender_com_lab[2], "\n")

############### each mmHg increase #####################

# Step 2: Fit the Cox proportional hazards model
sd_S_gender_com_lab <- coxph(Surv(time_ms, outcome) ~ sd_S + Gender + com_ht + com_dm + com_lip +
                             ValueHDL + egfr + tr_fglucose + tr_bmi, data = BPV_55_cph)

# Step 3: Display the results
summary(sd_S_gender_com_lab)

# Extract hazard ratio and confidence interval
hr_sd_S_gender_com_lab <- exp(coef(sd_S_gender_com_lab)["sd_S"])
ci_sd_S_gender_com_lab <- exp(confint(sd_S_gender_com_lab)["sd_S", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in SD SBP:", hr_sd_S_gender_com_lab, "\n")
cat("95% Confidence Interval:", ci_sd_S_gender_com_lab[1], "-", ci_sd_S_gender_com_lab[2], "\n")

################# Diastolic ###################

# Step 2: Fit the Cox proportional hazards model
sd_D_gender_com <- coxph(Surv(time_ms, outcome) ~ sd_D + Gender + com_ht + com_dm + com_lip,
                         data = BPV_55_cph)

# Step 3: Display the results
summary(sd_D_gender_com)

# Extract hazard ratio and confidence interval
hr_sd_D_gender_com <- exp(coef(sd_D_gender_com)["sd_D"])
ci_sd_D_gender_com <- exp(confint(sd_D_gender_com)["sd_D", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in SD SBP:", hr_sd_D_gender_com, "\n")
cat("95% Confidence Interval:", ci_sd_S_gender_com[1], "-", ci_sd_D_gender_com[2], "\n")




#################### # CPH analysis using SD adjusted for comorbidities, lab, medications #################

#################### Systolic ###########################

# Step 2: Fit the Cox proportional hazards model
sd_S_std_gender_com_lab_med <- coxph(Surv(time_ms, outcome) ~ sd_S_std + Gender + com_ht + com_dm + com_lip +
                            ValueHDL + egfr + tr_fglucose + tr_bmi + med_ht + med_dm + med_lipid + med_coag,
                            data = BPV_55_cph)

# Step 3: Display the results
summary(sd_S_std_gender_com_lab_med)

# Extract hazard ratio and confidence interval
hr_sd_S_std_gender_com_lab_med <- exp(coef(sd_S_std_gender_com_lab_med)["sd_S_std"])
ci_sd_S_std_gender_com_lab_med <- exp(confint(sd_S_std_gender_com_lab_med)["sd_S_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in CV SBP:", hr_sd_S_std_gender_com_lab_med, "\n")
cat("95% Confidence Interval:", ci_sd_S_std_gender_com_lab_med[1], "-", ci_sd_S_std_gender_com_lab_med[2], "\n")

################# Diastolic ###################

# Step 2: Fit the Cox proportional hazards model
sd_D_std_gender_com <- coxph(Surv(time_ms, outcome) ~ sd_D_std + Gender + com_ht + com_dm + com_lip,
                             data = BPV_55_cph)

# Step 3: Display the results
summary(sd_D_std_gender_com)

# Extract hazard ratio and confidence interval
hr_sd_D_std_gender_com <- exp(coef(sd_D_std_gender_com)["sd_D_std"])
ci_sd_D_std_gender_com <- exp(confint(sd_D_std_gender_com)["sd_D_std", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in SD SBP:", hr_sd_D_std_gender_com, "\n")
cat("95% Confidence Interval:", ci_sd_D_std_gender_com[1], "-", ci_sd_D_std_gender_com[2], "\n")

############### each mmHg increase #####################

# Step 2: Fit the Cox proportional hazards model
sd_S_gender_com <- coxph(Surv(time_ms, outcome) ~ sd_S + Gender + com_ht + com_dm + com_lip,
                         data = BPV_55_cph)

# Step 3: Display the results
summary(sd_S_gender_com)

# Extract hazard ratio and confidence interval
hr_sd_S_gender_com <- exp(coef(sd_S_gender_com)["sd_S"])
ci_sd_S_gender_com <- exp(confint(sd_S_gender_com)["sd_S", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in SD SBP:", hr_sd_S_gender_com, "\n")
cat("95% Confidence Interval:", ci_sd_S_gender_com[1], "-", ci_sd_S_gender_com[2], "\n")

################# Diastolic ###################

# Step 2: Fit the Cox proportional hazards model
sd_D_gender_com <- coxph(Surv(time_ms, outcome) ~ sd_D + Gender + com_ht + com_dm + com_lip,
                         data = BPV_55_cph)

# Step 3: Display the results
summary(sd_D_gender_com)

# Extract hazard ratio and confidence interval
hr_sd_D_gender_com <- exp(coef(sd_D_gender_com)["sd_D"])
ci_sd_D_gender_com <- exp(confint(sd_D_gender_com)["sd_D", ])

# Display the hazard ratio and confidence interval
cat("Hazard Ratio (HR) for 1-SD increase in SD SBP:", hr_sd_D_gender_com, "\n")
cat("95% Confidence Interval:", ci_sd_S_gender_com[1], "-", ci_sd_D_gender_com[2], "\n")


##### Check for multicollinearity

model <- glm(outcome ~ sd_S + sd_D + cv_S + cv_D + Gender + com_ht + com_dm + com_lip +
               ValueHDL + egfr + tr_fglucose + tr_bmi + med_ht + med_dm + med_lipid + med_coag,
             data = BPV_55_cph, family = "binomial")
vif(model)

cor(BPV_55_cph %>% select(com_ht, com_dm, com_lip))

###########Test for interaction################

test_interaction <- function(data, formula_main, formula_interaction, model_name = "") {
  cat("\n============================\n")
  cat("Testing:", model_name, "\n")
  cat("============================\n")

  # Fit main and interaction models
  model_main <- glm(formula_main, data = data, family = "binomial")
  model_inter <- glm(formula_interaction, data = data, family = "binomial")

  # Likelihood ratio test
  lrt <- anova(model_main, model_inter, test = "LRT")

  # Print model summaries (optional)
  cat("\nSummary of interaction model:\n")
  print(summary(model_inter))

  # Print p-value from likelihood ratio test
  p_val <- lrt$`Pr(>Chi)`[2]
  cat("\nLikelihood Ratio Test p-value for interaction:", round(p_val, 4), "\n")

  if (!is.na(p_val) && p_val < 0.05) {
    cat("✅ Interaction term significantly improves the model.\n")
  } else {
    cat("❌ No significant improvement from interaction.\n")
  }
}


# 1. cv_S * Gender
test_interaction(
  data = train_data,
  formula_main = outcome ~ cv_S + Gender,
  formula_interaction = outcome ~ cv_S * Gender,
  model_name = "cv_S * Gender"
)

# 2. sd_D * com_ht
test_interaction(
  data = train_data,
  formula_main = outcome ~ sd_D + com_ht,
  formula_interaction = outcome ~ sd_D * com_ht,
  model_name = "sd_D * com_ht"
)

# 3. sd_S * com_dm
test_interaction(
  data = train_data,
  formula_main = outcome ~ sd_S + com_dm,
  formula_interaction = outcome ~ sd_S * com_dm,
  model_name = "sd_S * com_dm"
)

# 4. cv_D * com_lip
test_interaction(
  data = train_data,
  formula_main = outcome ~ cv_D + com_lip,
  formula_interaction = outcome ~ cv_D * com_lip,
  model_name = "cv_D * com_lip"
)


############Sub Group Analysis###############

# Your base model formula
base_formula <- Surv(time_ms, outcome) ~ sd_S_std + Gender + com_ht + com_dm + com_lip +
  ValueHDL + egfr + tr_fglucose + tr_bmi + med_ht + med_dm + med_lipid + med_coag

BPV_55_cph <- BPV_55_cph %>%
  mutate(egfr_group = ifelse(egfr < 90, "<90", "≥90"))


# Create subsets for each subgroup
bpv_dm <- BPV_55_cph %>% filter(com_dm == "1") # Or "DM" if you relabeled
bpv_no_dm <- BPV_55_cph %>% filter(com_dm == "0") # Or "No DM"

bpv_ht <- BPV_55_cph %>% filter(com_ht == "1") # Or "HT"
bpv_no_ht <- BPV_55_cph %>% filter(com_ht == "0") # Or "No HT"

bpv_egfr_lt90 <- BPV_55_cph %>% filter(egfr_group == "<90")
bpv_egfr_ge90 <- BPV_55_cph %>% filter(egfr_group == ">=90")


cat("\n--- Subgroup Analysis: DM vs Non-DM ---\n")
cat("Model for Non-DM:\n")
coxph_no_dm <- coxph(base_formula, data = bpv_no_dm)
summary(coxph_no_dm)

cat("\nModel for DM:\n")
coxph_dm <- coxph(base_formula, data = bpv_dm)
summary(coxph_dm)

cat("\n--- Subgroup Analysis: HT vs Non-HT ---\n")
cat("Model for Non-HT:\n")
coxph_no_ht <- coxph(base_formula, data = bpv_no_ht)
summary(coxph_no_ht)

cat("\nModel for HT:\n")
coxph_ht <- coxph(base_formula, data = bpv_ht)
summary(coxph_ht)

cat("\n--- Subgroup Analysis: EGFR <90 vs >=90 ---\n")
cat("Model for EGFR >=90:\n")
coxph_egfr_ge90 <- coxph(base_formula, data = bpv_egfr_ge90)
summary(coxph_egfr_ge90)

cat("\nModel for EGFR <90:\n")
coxph_egfr_lt90 <- coxph(base_formula, data = bpv_egfr_lt90)
summary(coxph_egfr_lt90)
