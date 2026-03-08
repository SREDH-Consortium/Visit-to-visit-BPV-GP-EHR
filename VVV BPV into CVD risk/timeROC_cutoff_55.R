library(survival)
library(timeROC)
library(dplyr)
library(ggsurvfit)

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(sd_S_cat = ifelse(sd_S >= 19, 1, 0))


cox_sd_S <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ sd_S_cat))

# Get linear predictor (risk score)
lp <- predict(cox_sd_S, type = "lp")

# Time points at which to evaluate ROC
times <- quantile(BPV.55_3022$time_ms[BPV.55_3022$outcome == 1], probs = c(0.252, 0.525, 0.65))

tiff("roc_obj_sd_S.tiff", width = 6, height = 5, units = "in", res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_sd_S <- timeROC(T = BPV.55_3022$time_ms,
                   delta = BPV.55_3022$outcome,
                   marker = lp,
                   cause = 1,
                   times = times,
                   iid = TRUE)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Build legend labels
legend_labels <- paste0("t = ", times_months, " months")

# Plot with custom title
plot(roc_obj_sd_S, time = times[1], col = "red", title = FALSE)
plot(roc_obj_sd_S, time = times[2], add = TRUE, col = "blue", title = FALSE)
plot(roc_obj_sd_S, time = times[3], add = TRUE, col = "green", title = FALSE)

# Add title separately
title("Time - Dependent ROC for Systolic Blood Pressure Variability\n(Standard Deviation, 19 mmHg Cut - Off)",
      cex.main = 0.9)


# Add smaller legend with no border
legend("bottomright",
       legend = legend_labels,
       col = c("red", "blue", "green"),
       lty = 1,
       cex = 0.8,       # shrink text size
       bty = "n")       # remove box border


dev.off()

# AUCs at each time point
roc_obj_sd_S$AUC

# Extract AUCs and SEs
auc <- roc_obj_sd_S$AUC
se <- roc_obj_sd_S$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Combine into a data frame
auc_ci <- data.frame(
  time = times,
  AUC = auc,
  lower_CI = lower,
  upper_CI = upper
)

print(auc_ci)


######## sd_D ########

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(sd_D_cat = ifelse(sd_D >= 11, 1, 0))


cox_sd_D <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ sd_D_cat))

# Get linear predictor (risk score)
lp <- predict(cox_sd_D, type = "lp")

# Time points at which to evaluate ROC
times <- quantile(BPV.55_3022$time_ms[BPV.55_3022$outcome == 1], probs = c(0.252, 0.525, 0.65))

tiff("roc_obj_sd_D.tiff", width = 6, height = 5, units = "in", res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_sd_D <- timeROC(T = BPV.55_3022$time_ms,
                        delta = BPV.55_3022$outcome,
                        marker = lp,
                        cause = 1,
                        times = times,
                        iid = TRUE)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Extract AUCs and SEs
auc <- roc_obj_sd_D$AUC
se <- roc_obj_sd_D$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Build legend labels with AUC and CI
legend_labels <- paste0(
  "t = ", times_months, " months, ",
  "AUC = ", sprintf("%.3f", auc),
  " (", sprintf("%.3f", lower), "–", sprintf("%.3f", upper), ")"
)

# Plot ROC curves at each time
plot(roc_obj_sd_D, time = times[1], col = "red", title = FALSE)
plot(roc_obj_sd_D, time = times[2], add = TRUE, col = "blue", title = FALSE)
plot(roc_obj_sd_D, time = times[3], add = TRUE, col = "green", title = FALSE)

# Add title separately
title("Time - Dependent ROC for Diastolic Blood Pressure Variability\n(Standard Deviation, 11 mmHg Cut - Off)",
      cex.main = 0.9)

# Add smaller legend with no border
legend("bottomright",
       legend = legend_labels,
       col = c("red", "blue", "green"),
       lty = 1,
       cex = 0.8,       # shrink text size
       bty = "n")       # remove box border


dev.off()

# AUCs at each time point
roc_obj_sd_D$AUC

# Extract AUCs and SEs
auc <- roc_obj_sd_D$AUC
se <- roc_obj_sd_D$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Combine into a data frame
auc_ci <- data.frame(
  time = times,
  AUC = auc,
  lower_CI = lower,
  upper_CI = upper
)

print(auc_ci)

######## cv_S ########

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(cv_S_cat = ifelse(cv_S >= 14, 1, 0))


cox_cv_S <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ cv_S_cat))

# Get linear predictor (risk score)
lp <- predict(cox_cv_S, type = "lp")

tiff("roc_obj_cv_S.tiff", width = 6, height = 5, units = "in", res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_cv_S <- timeROC(T = BPV.55_3022$time_ms,
                        delta = BPV.55_3022$outcome,
                        marker = lp,
                        cause = 1,
                        times = times,
                        iid = TRUE)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Extract AUCs and SEs
auc <- roc_obj_cv_S$AUC
se <- roc_obj_cv_S$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Build legend labels with AUC and CI
legend_labels <- paste0(
  "t = ", times_months, " months, ",
  "AUC = ", sprintf("%.3f", auc),
  " (", sprintf("%.3f", lower), "–", sprintf("%.3f", upper), ")"
)

# Plot ROC curves at each time
plot(roc_obj_cv_S, time = times[1], col = "red", title = FALSE)
plot(roc_obj_cv_S, time = times[2], add = TRUE, col = "blue", title = FALSE)
plot(roc_obj_cv_S, time = times[3], add = TRUE, col = "green", title = FALSE)

# Add title separately
title("Time - Dependent ROC for Systolic Blood Pressure Variability\n(Coefficient of Variation, 14% Cut - Off)",
      cex.main = 0.9)

# Add smaller legend with no border
legend("bottomright",
       legend = legend_labels,
       col = c("red", "blue", "green"),
       lty = 1,
       cex = 0.8,       # shrink text size
       bty = "n")       # remove box border


dev.off()

# AUCs at each time point
roc_obj_cv_S$AUC

# Extract AUCs and SEs
auc <- roc_obj_cv_S$AUC
se <- roc_obj_cv_S$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Combine into a data frame
auc_ci <- data.frame(
  time = times,
  AUC = auc,
  lower_CI = lower,
  upper_CI = upper
)

print(auc_ci)



######## cv_D ########

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(cv_D_cat = ifelse(cv_D >= 12, 1, 0))


cox_cv_D <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ cv_D_cat))

# Get linear predictor (risk score)
lp <- predict(cox_cv_D, type = "lp")

tiff("roc_obj_cv_D.tiff", width = 6, height = 5, units = "in", res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_cv_D <- timeROC(T = BPV.55_3022$time_ms,
                        delta = BPV.55_3022$outcome,
                        marker = lp,
                        cause = 1,
                        times = times,
                        iid = TRUE)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Extract AUCs and SEs
auc <- roc_obj_cv_D$AUC
se <- roc_obj_cv_D$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Build legend labels with AUC and CI
legend_labels <- paste0(
  "t = ", times_months, " months, ",
  "AUC = ", sprintf("%.3f", auc),
  " (", sprintf("%.3f", lower), "–", sprintf("%.3f", upper), ")"
)

# Plot ROC curves at each time
plot(roc_obj_cv_D, time = times[1], col = "red", title = FALSE)
plot(roc_obj_cv_D, time = times[2], add = TRUE, col = "blue", title = FALSE)
plot(roc_obj_cv_D, time = times[3], add = TRUE, col = "green", title = FALSE)

# Add title separately
title("Time - Dependent ROC for Diastolic Blood Pressure Variability\n(Coefficient of Variation, 12% Cut - Off)",
      cex.main = 0.9)

# Add smaller legend with no border
legend("bottomright",
       legend = legend_labels,
       col = c("red", "blue", "green"),
       lty = 1,
       cex = 0.8,       # shrink text size
       bty = "n")       # remove box border


dev.off()

# AUCs at each time point
roc_obj_cv_D$AUC

# Extract AUCs and SEs
auc <- roc_obj_cv_D$AUC
se <- roc_obj_cv_D$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Combine into a data frame
auc_ci <- data.frame(
  time = times,
  AUC = auc,
  lower_CI = lower,
  upper_CI = upper
)

print(auc_ci)

######## arv_S ########

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(arv_S_cat = ifelse(arv_S >= 15, 1, 0))


cox_arv_S <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ arv_S_cat))

# Get linear predictor (risk score)
lp <- predict(cox_arv_S, type = "lp")

tiff("roc_obj_arv_S.tiff", width = 6, height = 5, units = "in", res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_arv_S <- timeROC(T = BPV.55_3022$time_ms,
                        delta = BPV.55_3022$outcome,
                        marker = lp,
                        cause = 1,
                        times = times,
                        iid = TRUE)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Extract AUCs and SEs
auc <- roc_obj_arv_S$AUC
se <- roc_obj_arv_S$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Build legend labels with AUC and CI
legend_labels <- paste0(
  "t = ", times_months, " months, ",
  "AUC = ", sprintf("%.3f", auc),
  " (", sprintf("%.3f", lower), "–", sprintf("%.3f", upper), ")"
)

# Plot ROC curves at each time
plot(roc_obj_arv_S, time = times[1], col = "red", title = FALSE)
plot(roc_obj_arv_S, time = times[2], add = TRUE, col = "blue", title = FALSE)
plot(roc_obj_arv_S, time = times[3], add = TRUE, col = "green", title = FALSE)

# Add title separately
title("Time - Dependent ROC for Systolic Blood Pressure Variability\n(Average Real Variability, 15 mmHg Cut - Off)",
      cex.main = 0.9)

# Add smaller legend with no border
legend("bottomright",
       legend = legend_labels,
       col = c("red", "blue", "green"),
       lty = 1,
       cex = 0.8,       # shrink text size
       bty = "n")       # remove box border


dev.off()

# AUCs at each time point
roc_obj_arv_S$AUC

# Extract AUCs and SEs
auc <- roc_obj_arv_S$AUC
se <- roc_obj_arv_S$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Combine into a data frame
auc_ci <- data.frame(
  time = times,
  AUC = auc,
  lower_CI = lower,
  upper_CI = upper
)

print(auc_ci)



######## arv_D ########

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(arv_D_cat = ifelse(arv_D >= 11, 1, 0))


cox_arv_D <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ arv_D_cat))

# Get linear predictor (risk score)
lp <- predict(cox_arv_D, type = "lp")

tiff("roc_obj_arv_D.tiff", width = 6, height = 5, units = "in", res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_arv_D <- timeROC(T = BPV.55_3022$time_ms,
                        delta = BPV.55_3022$outcome,
                        marker = lp,
                        cause = 1,
                        times = times,
                        iid = TRUE)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Extract AUCs and SEs
auc <- roc_obj_arv_D$AUC
se <- roc_obj_arv_D$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Build legend labels with AUC and CI
legend_labels <- paste0(
  "t = ", times_months, " months, ",
  "AUC = ", sprintf("%.3f", auc),
  " (", sprintf("%.3f", lower), "–", sprintf("%.3f", upper), ")"
)

# Plot ROC curves at each time
plot(roc_obj_arv_D, time = times[1], col = "red", title = FALSE)
plot(roc_obj_arv_D, time = times[2], add = TRUE, col = "blue", title = FALSE)
plot(roc_obj_arv_D, time = times[3], add = TRUE, col = "green", title = FALSE)

# Add title separately
title("Time - Dependent ROC for Diastolic Blood Pressure Variability\n(Average Real Variability, 11 mmHg Cut - Off)",
      cex.main = 0.9)

# Add smaller legend with no border
legend("bottomright",
       legend = legend_labels,
       col = c("red", "blue", "green"),
       lty = 1,
       cex = 0.8,       # shrink text size
       bty = "n")       # remove box border


dev.off()

# AUCs at each time point
roc_obj_arv_D$AUC

# Extract AUCs and SEs
auc <- roc_obj_arv_D$AUC
se <- roc_obj_arv_D$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Combine into a data frame
auc_ci <- data.frame(
  time = times,
  AUC = auc,
  lower_CI = lower,
  upper_CI = upper
)

print(auc_ci)


# Se, Sp, PPV and NPV computation for sd_S at threshold 19 mmHg
res.SeSpPPVNPV.sd_S <- SeSpPPVNPV(cutpoint=19,
                                  T=BPV.55_3022$time_ms,
                                  delta=BPV.55_3022$outcome,marker=BPV.55_3022$sd_S,
                                  cause=1,weighting="marginal",
                                  times= quantile(BPV.55_3022$time_ms,probs=seq(0.252, 0.525, 0.65)),
                                  iid=TRUE)

# 1, 2, and 5 years in months
times = c(12, 24, 60)

res.SeSpPPVNPV.sd_S <- SeSpPPVNPV(
  cutpoint = 19,
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = BPV.55_3022$sd_S,
  cause = 1,
  weighting = "marginal",
  times = c(12, 24, 60),
  iid = TRUE
)

res.SeSpPPVNPV.sd_S


# Se, Sp, PPV and NPV computation for sd_D at threshold 11 mmHg
res.SeSpPPVNPV.sd_D <- SeSpPPVNPV(cutpoint=11,
                                  T=BPV.55_3022$time_ms,
                                  delta=BPV.55_3022$outcome,marker=BPV.55_3022$sd_D,
                                  cause=1,weighting="marginal",
                                  times= quantile(BPV.55_3022$time_ms,probs=seq(0.252, 0.525, 0.65)),
                                  iid=TRUE)
res.SeSpPPVNPV.sd_D

# 1, 2, and 5 years in months
times = c(12, 24, 60)

res.SeSpPPVNPV.sd_D <- SeSpPPVNPV(
  cutpoint = 11,
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = BPV.55_3022$sd_D,
  cause = 1,
  weighting = "marginal",
  times = c(12, 24, 60),
  iid = TRUE
)

res.SeSpPPVNPV.sd_D


# Se, Sp, PPV and NPV computation for cv_S at threshold 14 %
res.SeSpPPVNPV.cv_S <- SeSpPPVNPV(cutpoint=14,
                                  T=BPV.55_3022$time_ms,
                                  delta=BPV.55_3022$outcome,marker=BPV.55_3022$cv_S,
                                  cause=1,weighting="marginal",
                                  times= quantile(BPV.55_3022$time_ms,probs=seq(0.252, 0.525, 0.65)),
                                  iid=TRUE)
res.SeSpPPVNPV.cv_S

# 1, 2, and 5 years in months
times = c(12, 24, 60)

res.SeSpPPVNPV.cv_S <- SeSpPPVNPV(
  cutpoint = 14,
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = BPV.55_3022$cv_S,
  cause = 1,
  weighting = "marginal",
  times = c(12, 24, 60),
  iid = TRUE
)

res.SeSpPPVNPV.cv_S


# Se, Sp, PPV and NPV computation for cv_D at threshold 12 %
res.SeSpPPVNPV.cv_D <- SeSpPPVNPV(cutpoint=12,
                                  T=BPV.55_3022$time_ms,
                                  delta=BPV.55_3022$outcome,marker=BPV.55_3022$cv_D,
                                  cause=1,weighting="marginal",
                                  times= quantile(BPV.55_3022$time_ms,probs=seq(0.252, 0.525, 0.65)),
                                  iid=TRUE)
res.SeSpPPVNPV.cv_D

# 1, 2, and 5 years in months
times = c(12, 24, 60)

res.SeSpPPVNPV.cv_D <- SeSpPPVNPV(
  cutpoint = 12,
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = BPV.55_3022$cv_D,
  cause = 1,
  weighting = "marginal",
  times = c(12, 24, 60),
  iid = TRUE
)

res.SeSpPPVNPV.cv_D


# Se, Sp, PPV and NPV computation for arv_S at threshold 15 %
res.SeSpPPVNPV.arv_S <- SeSpPPVNPV(cutpoint=15,
                                  T=BPV.55_3022$time_ms,
                                  delta=BPV.55_3022$outcome,marker=BPV.55_3022$arv_S,
                                  cause=1,weighting="marginal",
                                  times= quantile(BPV.55_3022$time_ms,probs=seq(0.252, 0.525, 0.65)),
                                  iid=TRUE)
res.SeSpPPVNPV.arv_S

# 1, 2, and 5 years in months
times = c(12, 24, 60)

res.SeSpPPVNPV.arv_S <- SeSpPPVNPV(
  cutpoint = 15,
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = BPV.55_3022$arv_S,
  cause = 1,
  weighting = "marginal",
  times = c(12, 24, 60),
  iid = TRUE
)

res.SeSpPPVNPV.arv_S



# Se, Sp, PPV and NPV computation for arv_D at threshold 11 %
res.SeSpPPVNPV.arv_D <- SeSpPPVNPV(cutpoint=11,
                                  T=BPV.55_3022$time_ms,
                                  delta=BPV.55_3022$outcome,marker=BPV.55_3022$arv_D,
                                  cause=1,weighting="marginal",
                                  times= quantile(BPV.55_3022$time_ms,probs=seq(0.252, 0.525, 0.65)),
                                  iid=TRUE)
res.SeSpPPVNPV.arv_D

res.SeSpPPVNPV.arv_D <- SeSpPPVNPV(
  cutpoint = 11,
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = BPV.55_3022$arv_D,
  cause = 1,
  weighting = "marginal",
  times = c(12, 24, 60),
  iid = TRUE
)

res.SeSpPPVNPV.arv_D


km_fit <- survfit(Surv(time_ms, outcome) ~ 1, data=BPV.55_3022)
plot(km_fit)

# Survival function for cvd data by sex
cvd.fit.sd_S <- survfit(Surv(time_ms, outcome) ~ sd_S_cat,
                   data = BPV.55_3022,
                   conf.type = "log-log")


# Kaplan-Meier survival curve for men and women
ggsurvfit(cvd.fit.sd_S) +
  scale_y_continuous(limits = c(0,1)) +
  labs( x = "Time", y = "Survival probability") +
  scale_colour_discrete(labels = c("SD < 19", "SD >=19"))
