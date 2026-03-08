library(survival)
library(pec)
library(survcomp)
library(riskRegression)
library(timeROC)
library(dplyr)
library (mice)

install.packages("parallelly")


if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

BiocManager::install("survcomp")

imputed <- readRDS ("imputed_55_CVD.rds")

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
compute_metrics <- function(model, data, times_auc = c(12,24,36)) {
  # Linear predictor
  lp <- predict(model, newdata = data, type = "lp")

  # 1. Harrell's C-index
  harrell_c <- concordance.index(lp, surv.time = data$time_ms, surv.event = data$outcome)$c.index

  # 2. Brier score
  brier <- pec(
    object = list("Cox" = model),
    formula = Surv(time_ms, outcome) ~ 1,
    data = data,
    times = max(times_auc),
    exact = FALSE
  )$AppErr$Cox

  # 3. Calibration slope
  cal_slope <- coef(coxph(Surv(time_ms, outcome) ~ lp, data = data))

  # 4. Wolbers' C-index
  wolbers_c <- Score(
    object = list("Cox" = model),
    formula = Surv(time_ms, outcome) ~ 1,
    data = data,
    summary = "cindex",
    cens.model = "cox"
  )$AppCindex$Cox

  # 5. Time-varying AUC
  tv_auc <- timeROC(
    T = data$time_ms,
    delta = data$outcome,
    marker = lp,
    cause = 1,
    times = times_auc,
    iid = TRUE
  )$AUC

  list(Harrell_C = harrell_c, Brier = brier, CalSlope = cal_slope, Wolbers_C = wolbers_c, TV_AUC = tv_auc)
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
  CalSlope = pool_metric(pool_all(train_metrics, "CalSlope")),
  Wolbers_C = pool_metric(pool_all(train_metrics, "Wolbers_C"))
)

pooled_test <- list(
  Harrell_C = pool_metric(pool_all(test_metrics, "Harrell_C")),
  Brier = pool_metric(pool_all(test_metrics, "Brier")),
  CalSlope = pool_metric(pool_all(test_metrics, "CalSlope")),
  Wolbers_C = pool_metric(pool_all(test_metrics, "Wolbers_C"))
)

# Time-varying AUC pooling (each time point separately)
pool_tv_auc <- function(metric_list) {
  auc_mat <- do.call(rbind, lapply(metric_list, function(x) x$TV_AUC))
  lapply(1:ncol(auc_mat), function(j) pool_metric(auc_mat[, j]))
}

pooled_train$TV_AUC <- pool_tv_auc(train_metrics)
pooled_test$TV_AUC <- pool_tv_auc(test_metrics)

# --- STEP 6: Final results ---
results <- list(
  Train = pooled_train,
  Test = pooled_test
)

results
