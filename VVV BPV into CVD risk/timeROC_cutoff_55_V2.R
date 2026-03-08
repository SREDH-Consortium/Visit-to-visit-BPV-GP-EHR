library(survival)
library(timeROC)
library(dplyr)
library(ggsurvfit)


####### sd_S ##########

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(sd_S_cat = ifelse(sd_S >= 19, 1, 0))

cox_sd_S <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ sd_S_cat))

# Get linear predictor (risk score)
lp <- predict(cox_sd_S, type = "lp")

# Time points at which to evaluate ROC
times <- quantile(BPV.55_3022$time_ms[BPV.55_3022$outcome == 1],
                  probs = c(0.252, 0.525, 0.65))

tiff("roc_obj_sd_S_2.tiff", width = 6, height = 5, units = "in",
     res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_sd_S <- timeROC(
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = lp,
  cause = 1,
  times = times,
  iid = TRUE
)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Extract AUCs and SEs
auc <- roc_obj_sd_S$AUC
se <- roc_obj_sd_S$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Round everything to 2 decimals
auc_round   <- round(auc, 2)
lower_round <- round(lower, 2)
upper_round <- round(upper, 2)

# Build legend labels with AUC and CI
legend_labels <- paste0(
  "t = ", times_months, " months, ",
  "AUC = ", auc_round,
  " (", lower_round, "–", upper_round, ")"
)

# Plot ROC curves at each time
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
       cex = 0.8,
       bty = "n")

dev.off()

# Final AUC summary table (2 decimals)
auc_ci <- data.frame(
  time = times,
  AUC = auc_round,
  lower_CI = lower_round,
  upper_CI = upper_round
)

print(auc_ci)



####### sd_D #########

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(sd_D_cat = ifelse(sd_D >= 11, 1, 0))

cox_sd_D <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ sd_D_cat))

# Get linear predictor (risk score)
lp <- predict(cox_sd_D, type = "lp")

# Time points at which to evaluate ROC
times <- quantile(BPV.55_3022$time_ms[BPV.55_3022$outcome == 1],
                  probs = c(0.252, 0.525, 0.65))

tiff("roc_obj_sd_D_2.tiff", width = 6, height = 5, units = "in",
     res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_sd_D <- timeROC(
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = lp,
  cause = 1,
  times = times,
  iid = TRUE
)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Extract AUCs and SEs
auc <- roc_obj_sd_D$AUC
se <- roc_obj_sd_D$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Format everything to always show 2 decimals
auc_fmt   <- sprintf("%.2f", auc)
lower_fmt <- sprintf("%.2f", lower)
upper_fmt <- sprintf("%.2f", upper)

# Build legend labels with AUC and CI
legend_labels <- paste0(
  "t = ", times_months, " months, ",
  "AUC = ", auc_fmt,
  " (", lower_fmt, "–", upper_fmt, ")"
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
       cex = 0.8,
       bty = "n")

dev.off()

# Final AUC summary table (force 2 decimals in text)
auc_ci <- data.frame(
  time = times,
  AUC = auc_fmt,
  lower_CI = lower_fmt,
  upper_CI = upper_fmt
)

print(auc_ci)


####### cv_S #########

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(cv_S_cat = ifelse(cv_S >= 14, 1, 0))

cox_cv_S <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ cv_S_cat))

# Get linear predictor (risk score)
lp <- predict(cox_cv_S, type = "lp")

# Time points at which to evaluate ROC
times <- quantile(BPV.55_3022$time_ms[BPV.55_3022$outcome == 1],
                  probs = c(0.252, 0.525, 0.65))

tiff("roc_obj_cv_S_2.tiff", width = 6, height = 5, units = "in",
     res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_cv_S <- timeROC(
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = lp,
  cause = 1,
  times = times,
  iid = TRUE
)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Extract AUCs and SEs
auc <- roc_obj_cv_S$AUC
se <- roc_obj_cv_S$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Format everything to always show 2 decimals
auc_fmt   <- sprintf("%.2f", auc)
lower_fmt <- sprintf("%.2f", lower)
upper_fmt <- sprintf("%.2f", upper)

# Build legend labels with AUC and CI
legend_labels <- paste0(
  "t = ", times_months, " months, ",
  "AUC = ", auc_fmt,
  " (", lower_fmt, "–", upper_fmt, ")"
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
       cex = 0.8,
       bty = "n")

dev.off()

# Final AUC summary table (force 2 decimals in text)
auc_ci <- data.frame(
  time = times,
  AUC = auc_fmt,
  lower_CI = lower_fmt,
  upper_CI = upper_fmt
)

print(auc_ci)


####### cv_D #########

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(cv_D_cat = ifelse(cv_D >= 12, 1, 0))

cox_cv_D <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ cv_D_cat))

# Get linear predictor (risk score)
lp <- predict(cox_cv_D, type = "lp")

# Time points at which to evaluate ROC
times <- quantile(BPV.55_3022$time_ms[BPV.55_3022$outcome == 1],
                  probs = c(0.252, 0.525, 0.65))

tiff("roc_obj_cv_D_2.tiff", width = 6, height = 5, units = "in",
     res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_cv_D <- timeROC(
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = lp,
  cause = 1,
  times = times,
  iid = TRUE
)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Extract AUCs and SEs
auc <- roc_obj_cv_D$AUC
se <- roc_obj_cv_D$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Format everything to always show 2 decimals
auc_fmt   <- sprintf("%.2f", auc)
lower_fmt <- sprintf("%.2f", lower)
upper_fmt <- sprintf("%.2f", upper)

# Build legend labels with AUC and CI
legend_labels <- paste0(
  "t = ", times_months, " months, ",
  "AUC = ", auc_fmt,
  " (", lower_fmt, "–", upper_fmt, ")"
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
       cex = 0.8,
       bty = "n")

dev.off()

# Final AUC summary table (force 2 decimals in text)
auc_ci <- data.frame(
  time = times,
  AUC = auc_fmt,
  lower_CI = lower_fmt,
  upper_CI = upper_fmt
)

print(auc_ci)


####### arv_S #########

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(arv_S_cat = ifelse(arv_S >= 15, 1, 0))

cox_arv_S <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ arv_S_cat))

# Get linear predictor (risk score)
lp <- predict(cox_arv_S, type = "lp")

# Time points at which to evaluate ROC
times <- quantile(BPV.55_3022$time_ms[BPV.55_3022$outcome == 1],
                  probs = c(0.252, 0.525, 0.65))

tiff("roc_obj_arv_S_2.tiff", width = 6, height = 5, units = "in",
     res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_arv_S <- timeROC(
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = lp,
  cause = 1,
  times = times,
  iid = TRUE
)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Extract AUCs and SEs
auc <- roc_obj_arv_S$AUC
se <- roc_obj_arv_S$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Format everything to always show 2 decimals
auc_fmt   <- sprintf("%.2f", auc)
lower_fmt <- sprintf("%.2f", lower)
upper_fmt <- sprintf("%.2f", upper)

# Build legend labels with AUC and CI
legend_labels <- paste0(
  "t = ", times_months, " months, ",
  "AUC = ", auc_fmt,
  " (", lower_fmt, "–", upper_fmt, ")"
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
       cex = 0.8,
       bty = "n")

dev.off()

# Final AUC summary table (force 2 decimals in text)
auc_ci <- data.frame(
  time = times,
  AUC = auc_fmt,
  lower_CI = lower_fmt,
  upper_CI = upper_fmt
)

print(auc_ci)


####### arv_D #########

BPV.55_3022 <- BPV.55_3022 %>%
  mutate(arv_D_cat = ifelse(arv_D >= 11, 1, 0))

cox_arv_D <- with(BPV.55_3022, coxph(Surv(time_ms, outcome) ~ arv_D_cat))

# Get linear predictor (risk score)
lp <- predict(cox_arv_D, type = "lp")

# Time points at which to evaluate ROC
times <- quantile(BPV.55_3022$time_ms[BPV.55_3022$outcome == 1],
                  probs = c(0.252, 0.525, 0.65))

tiff("roc_obj_arv_D_2.tiff", width = 6, height = 5, units = "in",
     res = 600, compression = "lzw")
par(mar = c(5, 5, 4, 2))  # adjust margins

# Compute time-dependent ROC and AUC
roc_obj_arv_D <- timeROC(
  T = BPV.55_3022$time_ms,
  delta = BPV.55_3022$outcome,
  marker = lp,
  cause = 1,
  times = times,
  iid = TRUE
)

# Round times (already in months) to whole months
times_months <- round(times, 0)

# Extract AUCs and SEs
auc <- roc_obj_arv_D$AUC
se <- roc_obj_arv_D$inference$vect_sd_1

# Compute 95% CI
lower <- auc - 1.96 * se
upper <- auc + 1.96 * se

# Format everything to always show 2 decimals
auc_fmt   <- sprintf("%.2f", auc)
lower_fmt <- sprintf("%.2f", lower)
upper_fmt <- sprintf("%.2f", upper)

# Build legend labels with AUC and CI
legend_labels <- paste0(
  "t = ", times_months, " months, ",
  "AUC = ", auc_fmt,
  " (", lower_fmt, "–", upper_fmt, ")"
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
       cex = 0.8,
       bty = "n")

dev.off()

# Final AUC summary table (force 2 decimals in text)
auc_ci <- data.frame(
  time = times,
  AUC = auc_fmt,
  lower_CI = lower_fmt,
  upper_CI = upper_fmt
)

print(auc_ci)


         