rm(list=ls())

install.packages("glmnet")
install.packages("Matrix")
install.packages("mice")
install.packages("survcomp")

install.packages("https://cran.r-project.org/src/contrib/Archive/Matrix/Matrix_1.5-4.tar.gz",
                 repos = NULL, type = "source")
install.packages("lme4", type = "source")

install.packages("BiocManager")
BiocManager::install("survcomp")



library(survival)
library(pec)
library(survcomp)
library(timeROC)
library(dplyr)
library(mice)
library (riskRegression)

setwd("C:/Users/nicks/OneDrive - UNSW/Documents/Consults/Mifetika Lukitasari")

imputed = readRDS("imputed_55_CVD_complete.rds")

# --- STEP 1: Train/Test split ---
train_fraction <- 0.7
n <- nrow(complete(imputed, 1))
train_index <- sample(1:n, size = floor(train_fraction * n))

imputed_list <- lapply(1:imputed$m, function(i) {
  data <- complete(imputed, i)
  data$dataset <- ifelse(1:nrow(data) %in% train_index, "train", "test")
  data
})

# --- STEP 2: Fit Cox models on train data ---
cox_models <- lapply(imputed_list, function(df) {
  train_data <- df %>% filter(dataset == "train")
  coxph(Surv(time_ms, outcome) ~ Gender + SmokingStatus + tr_fglucose + tr_bmi + Value_TCHDL +
          egfr + FH + ObservationValue_S, data = train_data, x = TRUE, y = TRUE)
})

# Helper: function to get metrics for a dataset
compute_metrics <- function(model, data, times_auc = c(12, 24, 36, 48, 60)) {
  lp <- predict(model, newdata = data, type = "lp")

  # Harrell's C-index
  harrell_c <- concordance.index(lp, surv.time = data$time_ms, surv.event = data$outcome)$c.index

  # Time-varying Brier score + Integrated Brier Score
  pec_result <- pec(
    object = list("Cox" = model),
    formula = Surv(time_ms, outcome) ~ 1,
    data = data,
    times = times_auc,
    exact = TRUE,
    cens.model = "cox"
  )

  time_brier <- pec_result$AppErr$Cox  # vector of Brier scores at each time
  int_brier <- crps(pec_result)  # Integrated Brier Score

  # Calibration slope
  cal_slope <- coef(coxph(Surv(time_ms, outcome) ~ lp, data = data))

  # Time-varying AUC
  tv_auc <- timeROC(
    T = data$time_ms,
    delta = data$outcome,
    marker = lp,
    cause = 1,
    times = times_auc,
    iid = TRUE
  )$AUC

  list(
    Harrell_C = harrell_c,
    Brier = int_brier,
    Brier_Times = time_brier,
    CalSlope = cal_slope,
    TV_AUC = tv_auc
  )
}


# --- STEP 3: Compute metrics for train and test sets ---
train_metrics <- lapply(1:length(imputed_list), function(i) {
  df <- imputed_list[[i]] %>% filter(dataset == "train")
  compute_metrics(cox_models[[i]], df)
})

test_metrics <- lapply(1:length(imputed_list), function(i) {
  df <- imputed_list[[i]] %>% filter(dataset == "test")
  compute_metrics(cox_models[[i]], df)
})

# --- STEP 4: Pooling function ---
pool_metric <- function(values) {
  m <- length(values)
  qbar <- mean(values)
  ubar <- mean((values - qbar)^2) # between-imputation variance
  t <- ubar + (1 + 1/m) * var(values)
  se <- sqrt(t)
  list(mean = qbar, se = se, ci = c(qbar - 1.96*se, qbar + 1.96*se))
}

# Helper: pool all scalar metrics
pool_all <- function(metric_list, field) sapply(metric_list, function(x) x[[field]])

# --- STEP 5: Pool metrics ---
# Scalars
pooled_train <- list(
  Harrell_C = pool_metric(pool_all(train_metrics, "Harrell_C")),
  Brier = pool_metric(pool_all(train_metrics, "Brier")),
  CalSlope = pool_metric(pool_all(train_metrics, "CalSlope"))
)

pooled_test <- list(
  Harrell_C = pool_metric(pool_all(test_metrics, "Harrell_C")),
  Brier = pool_metric(pool_all(test_metrics, "Brier")),
  CalSlope = pool_metric(pool_all(test_metrics, "CalSlope"))
)

# Time-varying AUC pooling (each time point separately)
pool_tv_auc <- function(metric_list) {
  auc_mat <- do.call(rbind, lapply(metric_list, function(x) x$TV_AUC))
  lapply(1:ncol(auc_mat), function(j) pool_metric(auc_mat[, j]))
}

pooled_train$TV_AUC <- pool_tv_auc(train_metrics)
pooled_test$TV_AUC <- pool_tv_auc(test_metrics)

# Time-varying Brier score pooling (each time point separately)
pool_tvb <- function(metric_list) {
  tvb_mat <- do.call(rbind, lapply(metric_list, function(x) x$Brier_Times))
  lapply(1:ncol(tvb_mat), function(j) pool_metric(tvb_mat[, j]))
}

pooled_train$tvb <- pool_tvb(train_metrics)
pooled_test$tvb <- pool_tvb(test_metrics)

# --- STEP 6: Final results ---
results <- list(
  Train = pooled_train,
  Test = pooled_test
)

results

saveRDS(results, file = "results.rds")


# --- Extract pooled TV_AUC values 12,24,36,48,60 ---

# Manually define the times you pooled for
time_points_auc <- c(12, 24, 36, 48, 60)

# Build dataframe of pooled TV_AUC
tvauc_df <- data.frame(
  time  = time_points_auc,
  mean  = sapply(pooled_test$TV_AUC, function(x) x$mean),
  lower = sapply(pooled_test$TV_AUC, function(x) x$ci[1]),
  upper = sapply(pooled_test$TV_AUC, function(x) x$ci[2])
)

print(tvauc_df)

# Plot
library(ggplot2)
ggplot(tvauc_df, aes(x = time, y = mean)) +
  geom_line(color = "darkred", linewidth = 1) +
  geom_point(size = 3, color = "darkred") +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 1, color = "red4") +
  labs(
    title = "Time-varying AUC (Test set)",
    x = "Time (months)",
    y = "AUC"
  ) +
  theme_minimal(base_size = 14)
