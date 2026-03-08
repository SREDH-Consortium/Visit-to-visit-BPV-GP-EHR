library(survival)
library(survcomp)
library(pec)
library(timeROC)
library(dplyr)
library(ggplot2)


library(mice)

# 1. Convert the mids object to "long" format
# include = TRUE is crucial: it keeps the original data (with NAs) as .imp == 0
long_data <- complete(imputed, action = "long", include = TRUE)

# 2. Create the back-transformed variables
# We use exp() because it is the inverse of log()
long_data$fglucose <- exp(long_data$tr_fglucose)
long_data$bmi      <- exp(long_data$tr_bmi)

# 3. Convert back to a mice (mids) object
# .imp and .id are columns created by complete() that as.mids needs
imputed_with_orig <- as.mids(long_data)

# Now 'imputed_with_orig' is ready for analysis with pool(), using the new variables

saveRDS(imputed_with_orig, "imputed_S3_V1.rds")

imputed = readRDS("imputed_S3_V1.rds")

# Configuration
K_folds <- 10
times_auc_eval <- c(12, 24, 36, 48, 60)

# --- STEP 1: Generate Folds ---
# We generate folds once so the split is consistent across all imputations
n <- nrow(complete(imputed_with_orig, 1))
set.seed(123) # Ensure reproducibility
# Create a vector of fold IDs (1 to 10) assigned randomly to rows
fold_ids <- sample(rep(1:K_folds, length.out = n))

# --- STEP 2: Helper Functions ---

# 2a. Metric computation (Same as your original, slightly cleaned)
compute_metrics <- function(model, data, times_auc) {
  # Predict linear predictor
  lp <- predict(model, newdata = data, type = "lp")

  # 1. Harrell's C-index
  harrell_c <- concordance.index(lp, surv.time = data$time_ms, surv.event = data$outcome)$c.index

  # 2. Time-varying Brier score + Integrated Brier Score
  # Note: We use tryCatch as PEC/AUC can fail if a specific fold has no events at specific times
  pec_result <- tryCatch({
    pec(
      object = list("Cox" = model),
      formula = Surv(time_ms, outcome) ~ 1,
      data = data,
      times = times_auc,
      exact = FALSE, # Set to FALSE for specific times output matching
      cens.model = "cox",
      verbose = FALSE
    )
  }, error = function(e) return(NULL))

  if(!is.null(pec_result)){
    time_brier <- pec_result$AppErr$Cox # Vector at specific times
    int_brier <- crps(pec_result)[1]    # Integrated Brier Score
  } else {
    time_brier <- rep(NA, length(times_auc))
    int_brier <- NA
  }

  # 3. Calibration slope
  cal_fit <- tryCatch(coxph(Surv(time_ms, outcome) ~ lp, data = data), error=function(e) NULL)
  cal_slope <- if(!is.null(cal_fit)) coef(cal_fit)[1] else NA

  # 4. Time-varying AUC
  tv_auc_obj <- tryCatch({
    timeROC(
      T = data$time_ms,
      delta = data$outcome,
      marker = lp,
      cause = 1,
      times = times_auc,
      iid = FALSE # Turn off IID for speed in CV, we just need estimates
    )
  }, error = function(e) return(NULL))

  tv_auc <- if(!is.null(tv_auc_obj)) tv_auc_obj$AUC else rep(NA, length(times_auc))

  list(
    Harrell_C = harrell_c,
    Brier = int_brier,
    Brier_Times = time_brier,
    CalSlope = cal_slope,
    TV_AUC = tv_auc
  )
}

# 2b. Function to average metrics across K folds for a single imputation
average_fold_metrics <- function(fold_list) {
  # Extract scalars
  h_c <- mean(sapply(fold_list, function(x) x$Harrell_C), na.rm = TRUE)
  brier <- mean(sapply(fold_list, function(x) x$Brier), na.rm = TRUE)
  slope <- mean(sapply(fold_list, function(x) x$CalSlope), na.rm = TRUE)

  # Extract vectors (bind rows then take column means)
  tv_auc_mat <- do.call(rbind, lapply(fold_list, function(x) x$TV_AUC))
  tv_auc <- colMeans(tv_auc_mat, na.rm = TRUE)

  brier_t_mat <- do.call(rbind, lapply(fold_list, function(x) x$Brier_Times))
  brier_t <- colMeans(brier_t_mat, na.rm = TRUE)

  list(
    Harrell_C = h_c,
    Brier = brier,
    Brier_Times = brier_t,
    CalSlope = slope,
    TV_AUC = tv_auc
  )
}

# --- STEP 3: Main CV Loop over Imputations and Folds ---

# This list will store the averaged CV performance for each imputation
imputation_results <- list()

print("Starting Cross-Validation across imputations...")

for (m_idx in 1:imputed$m) {
  # Retrieve the m-th imputed dataset
  current_data <- complete(imputed_with_orig, m_idx)
  current_data$fold_id <- fold_ids

  # Store results for the K folds within this imputation
  folds_results <- list()

  for (k in 1:K_folds) {
    # Define Train/Test based on Folds
    train_data <- current_data %>% filter(fold_id != k)
    test_data  <- current_data %>% filter(fold_id == k)

    # Fit Model on Train
    fit <- coxph(Surv(time_ms, outcome) ~ Gender + SmokingStatus + fglucose + bmi + Value_TCHDL +
                   egfr + FH + com_ht + com_dm + med_ht + med_lipid + med_dm + ObservationValue_S,
                   data = train_data, x = TRUE, y = TRUE)

    # Evaluate on Test (Validation)
    folds_results[[k]] <- compute_metrics(fit, test_data, times_auc_eval)
  }

  # Average the K folds to get the CV estimate for this imputation
  imputation_results[[m_idx]] <- average_fold_metrics(folds_results)
}

# --- STEP 4: Pooling (Rubin's Rules) ---
# We reuse your existing pool_metric function.
# Now "imputation_results" acts like your old "test_metrics",
# but each entry is already a CV average.

pool_metric <- function(values) {
  values <- values[!is.na(values)] # Handle potential NAs
  m <- length(values)
  qbar <- mean(values)
  ubar <- mean((values - qbar)^2)
  t <- ubar + (1 + 1/m) * var(values)
  se <- sqrt(t)
  list(mean = qbar, se = se, ci = c(qbar - 1.96*se, qbar + 1.96*se))
}

pool_all <- function(metric_list, field) sapply(metric_list, function(x) x[[field]])

# Pool Scalars
pooled_cv_results <- list(
  Harrell_C = pool_metric(pool_all(imputation_results, "Harrell_C")),
  Brier = pool_metric(pool_all(imputation_results, "Brier")),
  CalSlope = pool_metric(pool_all(imputation_results, "CalSlope"))
)

# Pool Time-varying vectors (AUC)
pool_tv_vector <- function(metric_list, field) {
  mat <- do.call(rbind, lapply(metric_list, function(x) x[[field]]))
  lapply(1:ncol(mat), function(j) pool_metric(mat[, j]))
}

pooled_cv_results$TV_AUC <- pool_tv_vector(imputation_results, "TV_AUC")
pooled_cv_results$tvb <- pool_tv_vector(imputation_results, "Brier_Times") # Renamed to match your plot logic

# --- STEP 5: Results & Plotting ---

print(pooled_cv_results$Harrell_C)

# --- Prepare TV_AUC for Plotting ---
tvauc_df <- data.frame(
  time  = times_auc_eval,
  mean  = sapply(pooled_cv_results$TV_AUC, function(x) x$mean),
  lower = sapply(pooled_cv_results$TV_AUC, function(x) x$ci[1]),
  upper = sapply(pooled_cv_results$TV_AUC, function(x) x$ci[2])
)

print(tvauc_df)


# --- Prepare TV_Brier for Dataframe ---
tvb_df = matrix(nrow = length(times_auc_eval), ncol = 4)
tvb_df[,1] = times_auc_eval

# Note: The loop index here depends on how `pec` outputs times.
# Assuming input times resulted in matching output columns 1:5
for (i in 1:length(times_auc_eval)) {
  tvb_df[i,2] = pooled_cv_results$tvb[[i]]$mean
  tvb_df[i,3] = pooled_cv_results$tvb[[i]]$ci[1]
  tvb_df[i,4] = pooled_cv_results$tvb[[i]]$ci[2]
}

tvb_df = data.frame(tvb_df)
colnames(tvb_df) = c('time','mean','lower','upper')

print(tvb_df)
