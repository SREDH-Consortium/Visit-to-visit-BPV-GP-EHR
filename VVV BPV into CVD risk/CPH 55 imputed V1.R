library (survival)
library (ggplot2)
library(writexl)
library (car)
library (tidyverse)
library (mice)

imputed <- read_rds("imputed_55_SD_CV_ARV.rds", refhook = NULL)

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
model_check_sd_D_std_1 <- cox.zph(fitted_models_sd_D_std$analyses[[2]])
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


# models arv_S_std
# Step 6: Fit models to each dataset

fitted_models_arv_S_std_1 <- with(imputed,coxph(Surv(time_ms, outcome) ~ arv_S_std))
fitted_models_arv_S_std_1

# check proportional hazard (score test)
model_check_arv_S_std_1 <- cox.zph(fitted_models_arv_S_std_1$analyses[[2]])
model_check_arv_S_std_1

plot(model_check_arv_S_std_1)

# Step 7: Combine result from imputed dataset
est_arv_S_std_1 <- pool(fitted_models_arv_S_std_1)
summary(est_arv_S_std_1)


# models arv_D_std
# Step 6: Fit models to each dataset

fitted_models_arv_D_std_1 <- with(imputed,coxph(Surv(time_ms, outcome) ~ arv_D_std))
fitted_models_arv_D_std_1

# check proportional hazard (score test)
model_check_arv_D_std_1 <- cox.zph(fitted_models_arv_D_std_1$analyses[[2]])
model_check_arv_D_std_1

plot(model_check_arv_D_std_1)

# Step 7: Combine result from imputed dataset
est_arv_D_std_1 <- pool(fitted_models_arv_D_std_1)
summary(est_arv_D_std_1)

############# Adjusted for MEAN AND GENDER #######################

# models sd_S_std, mean, and gender
# Step 6: Fit models to each dataset

fitted_models_sd_S_std_2 <- with(imputed,coxph(Surv(time_ms, outcome) ~ sd_S_std + Gender + mean_S))
fitted_models_sd_S_std_2

# check proportional hazard (score test)
model_check_sd_S_std_2 <- cox.zph(fitted_models_sd_S_std_2$analyses[[2]])
model_check_sd_S_std_2

plot(model_check_sd_S_std_2)

# Step 7: Combine result from imputed dataset
est_sd_S_std_2 <- pool(fitted_models_sd_S_std_2)
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

# models arv_S_std
# Step 6: Fit models to each dataset

fitted_models_arv_S_std_2 <- with(imputed,coxph(Surv(time_ms, outcome) ~ arv_S_std + Gender + mean_S))
fitted_models_arv_S_std_2

# check proportional hazard (score test)
model_check_arv_S_std_2 <- cox.zph(fitted_models_arv_S_std_2$analyses[[2]])
model_check_arv_S_std_2

plot(model_check_arv_S_std_2)

# Step 7: Combine result from imputed dataset
est_arv_S_std_2 <- pool(fitted_models_arv_S_std_2)
summary(est_arv_S_std_2)

# models arv_D_std
# Step 6: Fit models to each dataset

fitted_models_arv_D_std_2 <- with(imputed,coxph(Surv(time_ms, outcome) ~ arv_D_std + Gender + mean_D))
fitted_models_arv_D_std_2

# check proportional hazard (score test)
model_check_arv_D_std_2 <- cox.zph(fitted_models_arv_D_std_2$analyses[[2]])
model_check_arv_D_std_2

plot(model_check_arv_D_std_2)

# Step 7: Combine result from imputed dataset
est_arv_D_std_2 <- pool(fitted_models_arv_D_std_2)
summary(est_arv_D_std_2)



############# Adjusted for MEAN, GENDER, COMORBIDITIES #######################

# models sd_S_std, mean, gender, comorbidities
# Step 6: Fit models to each dataset

fitted_models_sd_S_std_3 <- with(imputed,coxph(Surv(time_ms, outcome) ~ sd_S_std + Gender + mean_S + com_ht + com_dm + com_lip
                                               ))
fitted_models_sd_S_std_3

# check proportional hazard (score test)
model_check_sd_S_std_3 <- cox.zph(fitted_models_sd_S_std_3$analyses[[2]])
model_check_sd_S_std_3

plot(model_check_sd_S_std_3)

# Step 7: Combine result from imputed dataset
est_sd_S_std_3 <- pool(fitted_models_sd_S_std_3)
summary(est_sd_S_std_3)

# models sd_D_std
# Step 6: Fit models to each dataset

fitted_models_sd_D_std_3 <- with(imputed,coxph(Surv(time_ms, outcome) ~ sd_D_std + Gender + mean_D + com_ht + com_dm + com_lip))
fitted_models_sd_D_std_3

# check proportional hazard (score test)
model_check_sd_D_std_3 <- cox.zph(fitted_models_sd_D_std_3$analyses[[2]])
model_check_sd_D_std_3

plot(model_check_sd_D_std_3)

# Step 7: Combine result from imputed dataset
est_sd_D_std_3 <- pool(fitted_models_sd_D_std_3)
summary(est_sd_D_std_3)

# models cv_S_std
# Step 6: Fit models to each dataset

fitted_models_cv_S_std_3 <- with(imputed,coxph(Surv(time_ms, outcome) ~ cv_S_std + Gender + mean_S + com_ht + com_dm + com_lip))
fitted_models_cv_S_std_3

# check proportional hazard (score test)
model_check_cv_S_std_3 <- cox.zph(fitted_models_cv_S_std_3$analyses[[2]])
model_check_cv_S_std_3

plot(model_check_cv_S_std_3)

# Step 7: Combine result from imputed dataset
est_cv_S_std_3 <- pool(fitted_models_cv_S_std_3)
summary(est_cv_S_std_3)

# models cv_D_std
# Step 6: Fit models to each dataset

fitted_models_cv_D_std_3 <- with(imputed,coxph(Surv(time_ms, outcome) ~ cv_D_std + Gender + mean_D + com_ht + com_dm + com_lip))
fitted_models_cv_D_std_3

# check proportional hazard (score test)
model_check_cv_D_std_3 <- cox.zph(fitted_models_cv_D_std_3$analyses[[2]])
model_check_cv_D_std_3

plot(model_check_cv_D_std_3)

# Step 7: Combine result from imputed dataset
est_cv_D_std_3 <- pool(fitted_models_cv_D_std_3)
summary(est_cv_D_std_3)

# models arv_S_std
# Step 6: Fit models to each dataset

fitted_models_arv_S_std_3 <- with(imputed,coxph(Surv(time_ms, outcome) ~ arv_S_std + Gender + mean_S + com_ht + com_dm + com_lip))
fitted_models_arv_S_std_3

# check proportional hazard (score test)
model_check_arv_S_std_3 <- cox.zph(fitted_models_arv_S_std_3$analyses[[2]])
model_check_arv_S_std_3

plot(model_check_arv_S_std_3)

# Step 7: Combine result from imputed dataset
est_arv_S_std_3 <- pool(fitted_models_arv_S_std_3)
summary(est_arv_S_std_3)

# models arv_D_std
# Step 6: Fit models to each dataset

fitted_models_arv_D_std_3 <- with(imputed,coxph(Surv(time_ms, outcome) ~ arv_D_std + Gender + mean_D + com_ht + com_dm + com_lip))
fitted_models_arv_D_std_3

# check proportional hazard (score test)
model_check_arv_D_std_3 <- cox.zph(fitted_models_arv_D_std_3$analyses[[2]])
model_check_arv_D_std_3

plot(model_check_arv_D_std_3)

# Step 7: Combine result from imputed dataset
est_arv_D_std_3 <- pool(fitted_models_arv_D_std_3)
summary(est_arv_D_std_3)

############# Adjusted for MEAN, GENDER, COMORBIDITIES, LAB #######################

# models sd_S_std, mean, gender, comorbidities, lab
# Step 6: Fit models to each dataset

fitted_models_sd_S_std_4 <- with(imputed,coxph(Surv(time_ms, outcome) ~ sd_S_std + Gender + mean_S + com_ht + com_dm + com_lip +
                                           ValueHDL + egfr_num + tr_fglucose + tr_bmi))
fitted_models_sd_S_std_4

# check proportional hazard (score test)
model_check_sd_S_std_4 <- cox.zph(fitted_models_sd_S_std_4$analyses[[2]])
model_check_sd_S_std_4

plot(model_check_sd_S_std_4)

# Step 7: Combine result from imputed dataset
est_sd_S_std_4 <- pool(fitted_models_sd_S_std_4)
summary(est_sd_S_std_4)

# models sd_D_std
# Step 6: Fit models to each dataset

fitted_models_sd_D_std_4 <- with(imputed,coxph(Surv(time_ms, outcome) ~ sd_D_std + Gender + mean_D + com_ht + com_dm + com_lip +
                                                 ValueHDL + egfr_num + tr_fglucose + tr_bmi))
fitted_models_sd_D_std_4

# check proportional hazard (score test)
model_check_sd_D_std_4 <- cox.zph(fitted_models_sd_D_std_4$analyses[[2]])
model_check_sd_D_std_4

plot(model_check_sd_D_std_4)

# Step 7: Combine result from imputed dataset
est_sd_D_std_4 <- pool(fitted_models_sd_D_std_4)
summary(est_sd_D_std_4)

# models cv_S_std
# Step 6: Fit models to each dataset

fitted_models_cv_S_std_4 <- with(imputed,coxph(Surv(time_ms, outcome) ~ cv_S_std + Gender + mean_S + com_ht + com_dm + com_lip +
                                                 ValueHDL + egfr_num + tr_fglucose + tr_bmi))
fitted_models_cv_S_std_4

# check proportional hazard (score test)
model_check_cv_S_std_4 <- cox.zph(fitted_models_cv_S_std_4$analyses[[2]])
model_check_cv_S_std_4

plot(model_check_cv_S_std_4)

# Step 7: Combine result from imputed dataset
est_cv_S_std_4 <- pool(fitted_models_cv_S_std_4)
summary(est_cv_S_std_4)

# models cv_D_std
# Step 6: Fit models to each dataset

fitted_models_cv_D_std_4 <- with(imputed,coxph(Surv(time_ms, outcome) ~ cv_D_std + Gender + mean_D + com_ht + com_dm + com_lip +
                                                 ValueHDL + egfr_num + tr_fglucose + tr_bmi))
fitted_models_cv_D_std_4

# check proportional hazard (score test)
model_check_cv_D_std_4 <- cox.zph(fitted_models_cv_D_std_4$analyses[[2]])
model_check_cv_D_std_4

plot(model_check_cv_D_std_4)

# Step 7: Combine result from imputed dataset
est_cv_D_std_4 <- pool(fitted_models_cv_D_std_4)
summary(est_cv_D_std_4)


# models arv_S_std
# Step 6: Fit models to each dataset

fitted_models_arv_S_std_4 <- with(imputed,coxph(Surv(time_ms, outcome) ~ arv_S_std + Gender + mean_S + com_ht + com_dm + com_lip +
                                                 ValueHDL + egfr_num + tr_fglucose + tr_bmi))
fitted_models_arv_S_std_4

# check proportional hazard (score test)
model_check_arv_S_std_4 <- cox.zph(fitted_models_arv_S_std_4$analyses[[2]])
model_check_arv_S_std_4

plot(model_check_arv_S_std_4)

# Step 7: Combine result from imputed dataset
est_arv_S_std_4 <- pool(fitted_models_arv_S_std_4)
summary(est_arv_S_std_4)

# models arv_D_std
# Step 6: Fit models to each dataset

fitted_models_arv_D_std_4 <- with(imputed,coxph(Surv(time_ms, outcome) ~ arv_D_std + Gender + mean_D + com_ht + com_dm + com_lip +
                                                 ValueHDL + egfr_num + tr_fglucose + tr_bmi))
fitted_models_arv_D_std_4

# check proportional hazard (score test)
model_check_arv_D_std_4 <- cox.zph(fitted_models_arv_D_std_4$analyses[[2]])
model_check_arv_D_std_4

plot(model_check_arv_D_std_4)

# Step 7: Combine result from imputed dataset
est_arv_D_std_4 <- pool(fitted_models_arv_D_std_4)
summary(est_arv_D_std_4)


############# Adjusted for MEAN, GENDER, COMORBIDITIES, LAB, MEDS #######################

# models sd_S_std, mean, gender, comorbidities, lab
# Step 6: Fit models to each dataset

fitted_models_sd_S_std_5 <- with(imputed,coxph(Surv(time_ms, outcome) ~ sd_S_std + Gender + mean_S + com_ht + com_dm + com_lip +
                                           ValueHDL + egfr_num + tr_fglucose + tr_bmi + med_ht + med_dm + med_lipid + med_coag))
fitted_models_sd_S_std_5

# check proportional hazard (score test)
model_check_sd_S_std_5 <- cox.zph(fitted_models_sd_S_std_5$analyses[[10]])
model_check_sd_S_std_5

plot(model_check_sd_S_std_5)

# Step 7: Combine result from imputed dataset
est_sd_S_std_5 <- pool(fitted_models_sd_S_std_5)
summary(est_sd_S_std_5)

# models sd_D_std
# Step 6: Fit models to each dataset

fitted_models_sd_D_std_5 <- with(imputed,coxph(Surv(time_ms, outcome) ~ sd_D_std + Gender + mean_D + com_ht + com_dm + com_lip +
                                                 ValueHDL + egfr_num + tr_fglucose + tr_bmi + med_ht + med_dm + med_lipid + med_coag))
fitted_models_sd_D_std_5

# check proportional hazard (score test)
model_check_sd_D_std_5 <- cox.zph(fitted_models_sd_D_std_5$analyses[[2]])
model_check_sd_D_std_5

plot(model_check_sd_D_std_5)

# Step 7: Combine result from imputed dataset
est_sd_D_std_5 <- pool(fitted_models_sd_D_std_5)
summary(est_sd_D_std_5)

# models cv_S_std
# Step 6: Fit models to each dataset

fitted_models_cv_S_std_5 <- with(imputed,coxph(Surv(time_ms, outcome) ~ cv_S_std + Gender + mean_S + com_ht + com_dm + com_lip +
                                                 ValueHDL + egfr_num + tr_fglucose + tr_bmi + med_ht + med_dm + med_lipid + med_coag))
fitted_models_cv_S_std_5

# check proportional hazard (score test)
model_check_cv_S_std_5 <- cox.zph(fitted_models_cv_S_std_5$analyses[[2]])
model_check_cv_S_std_5

plot(model_check_cv_S_std_5)

# Step 7: Combine result from imputed dataset
est_cv_S_std_5 <- pool(fitted_models_cv_S_std_5)
summary(est_cv_S_std_5)

# models cv_D_std
# Step 6: Fit models to each dataset

fitted_models_cv_D_std_5 <- with(imputed,coxph(Surv(time_ms, outcome) ~ cv_D_std + Gender + mean_D + com_ht + com_dm + com_lip +
                                                 ValueHDL + egfr_num + tr_fglucose + tr_bmi + med_ht + med_dm + med_lipid + med_coag))
fitted_models_cv_D_std_5

# check proportional hazard (score test)d
model_check_cv_D_std_5 <- cox.zph(fitted_models_cv_D_std_5$analyses[[2]])
model_check_cv_D_std_5

plot(model_check_cv_D_std_5)

# Step 7: Combine result from imputed dataset
est_cv_D_std_5 <- pool(fitted_models_cv_D_std_5)
summary(est_cv_D_std_5)



# models arv_S_std
# Step 6: Fit models to each dataset

fitted_models_arv_S_std_5 <- with(imputed,coxph(Surv(time_ms, outcome) ~ arv_S_std + Gender + mean_S + com_ht + com_dm + com_lip +
                                                 ValueHDL + egfr_num + tr_fglucose + tr_bmi + med_ht + med_dm + med_lipid + med_coag))
fitted_models_arv_S_std_5

# check proportional hazard (score test)
model_check_arv_S_std_5 <- cox.zph(fitted_models_arv_S_std_5$analyses[[2]])
model_check_arv_S_std_5

plot(model_check_arv_S_std_5)

# Step 7: Combine result from imputed dataset
est_arv_S_std_5 <- pool(fitted_models_arv_S_std_5)
summary(est_arv_S_std_5)

# models arv_D_std
# Step 6: Fit models to each dataset

fitted_models_arv_D_std_5 <- with(imputed,coxph(Surv(time_ms, outcome) ~ arv_D_std + Gender + mean_D + com_ht + com_dm + com_lip +
                                                 ValueHDL + egfr_num + tr_fglucose + tr_bmi + med_ht + med_dm + med_lipid + med_coag))
fitted_models_arv_D_std_5

# check proportional hazard (score test)d
model_check_arv_D_std_5 <- cox.zph(fitted_models_arv_D_std_5$analyses[[2]])
model_check_arv_D_std_5

plot(model_check_arv_D_std_5)

# Step 7: Combine result from imputed dataset
est_arv_D_std_5 <- pool(fitted_models_arv_D_std_5)
summary(est_arv_D_std_5)




