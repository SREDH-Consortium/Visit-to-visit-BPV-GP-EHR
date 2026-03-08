library(mice)
library(survival)
library(dplyr)
library(survcomp)
library(pec)
library(timeROC)


imputed_with_orig <- readRDS("imputed_S3_V1.rds")

K_folds <- 10
times_auc_eval <- c(12, 24, 36, 48, 60)

gender_levels <- c("Male", "Female")


compute_metrics <- function(model, data, times_auc) {

  lp <- predict(model, newdata = data, type = "lp")

  # Harrell C
  harrell_c <- concordance.index(
    lp,
    surv.time  = data$time_ms,
    surv.event = data$outcome
  )$c.index

  # Brier Score
  pec_result <- tryCatch({
    pec(
      object  = list("Cox" = model),
      formula = Surv(time_ms, outcome) ~ 1,
      data    = data,
      times   = times_auc,
      exact   = FALSE,
      cens.model = "cox",
      verbose = FALSE
    )
  }, error = function(e) NULL)

  if (!is.null(pec_result)) {
    time_brier <- pec_result$AppErr$Cox
    int_brier  <- crps(pec_result)[1]
  } else {
    time_brier <- rep(NA, length(times_auc))
    int_brier  <- NA
  }

  # Calibration slope
  cal_fit <- tryCatch(
    coxph(Surv(time_ms, outcome) ~ lp, data = data),
    error = function(e) NULL
  )
  cal_slope <- if (!is.null(cal_fit)) coef(cal_fit)[1] else NA

  # Time-dependent AUC
  tv_auc_obj <- tryCatch({
    timeROC(
      T      = data$time_ms,
      delta  = data$outcome,
      marker = lp,
      cause  = 1,
      times  = times_auc,
      iid    = FALSE
    )
  }, error = function(e) NULL)

  tv_auc <- if (!is.null(tv_auc_obj)) tv_auc_obj$AUC else rep(NA, length(times_auc))

  list(
    Harrell_C   = harrell_c,
    Brier       = int_brier,
    Brier_Times = time_brier,
    CalSlope    = cal_slope,
    TV_AUC      = tv_auc
  )
}


average_fold_metrics <- function(fold_list) {

  h_c   <- mean(sapply(fold_list, function(x) x$Harrell_C), na.rm = TRUE)
  brier <- mean(sapply(fold_list, function(x) x$Brier), na.rm = TRUE)
  slope <- mean(sapply(fold_list, function(x) x$CalSlope), na.rm = TRUE)

  tv_auc_mat   <- do.call(rbind, lapply(fold_list, function(x) x$TV_AUC))
  tv_auc_mean  <- colMeans(tv_auc_mat, na.rm = TRUE)

  brier_t_mat  <- do.call(rbind, lapply(fold_list, function(x) x$Brier_Times))
  brier_t_mean <- colMeans(brier_t_mat, na.rm = TRUE)

  list(
    Harrell_C   = h_c,
    Brier       = brier,
    Brier_Times = brier_t_mean,
    CalSlope    = slope,
    TV_AUC      = tv_auc_mean
  )
}


pool_metric <- function(values) {
  values <- values[!is.na(values)]
  m <- length(values)
  qbar <- mean(values)
  ubar <- mean((values - qbar)^2)
  t <- ubar + (1 + 1/m) * var(values)
  se <- sqrt(t)
  list(mean = qbar, se = se,
       ci = c(qbar - 1.96*se, qbar + 1.96*se))
}

pool_all <- function(metric_list, field) {
  sapply(metric_list, function(x) x[[field]])
}

pool_tv_vector <- function(metric_list, field) {
  mat <- do.call(rbind, lapply(metric_list, function(x) x[[field]]))
  lapply(1:ncol(mat), function(j) pool_metric(mat[, j]))
}


gender_results <- list()

for (g in gender_levels) {

  cat("Running analysis for Gender:", g, "\n")

  # Generate folds within gender (from first imputation)
  data_g1 <- complete(imputed_with_orig, 1) %>%
    filter(Gender == g)

  n_g <- nrow(data_g1)

  set.seed(123)
  fold_ids <- sample(rep(1:K_folds, length.out = n_g))

  imputation_results <- list()

  for (m_idx in 1:imputed_with_orig$m) {

    current_data <- complete(imputed_with_orig, m_idx) %>%
      filter(Gender == g)

    current_data$fold_id <- fold_ids

    folds_results <- list()

    for (k in 1:K_folds) {

      train_data <- current_data %>% filter(fold_id != k)
      test_data  <- current_data %>% filter(fold_id == k)

      # 🔵 RIDGE-PENALISED COX
      fit <- coxph(
        Surv(time_ms, outcome) ~
          ridge(SmokingStatus, theta = 1) +
          ridge(fglucose, theta = 1) +
          ridge(bmi, theta = 1) +
          ridge(Value_TCHDL, theta = 1) +
          ridge(egfr, theta = 1) +
          ridge(FH, theta = 1) +
          ridge(com_ht, theta = 1) +
          ridge(com_dm, theta = 1) +
          ridge(med_ht, theta = 1) +
          ridge(med_dm, theta = 1) +
          ridge(med_lipid, theta = 1) +
          ridge(ObservationValue_S, theta = 1) +
          ridge(ObservationValue_D, theta = 1) +
          ridge(cv_S_cat, theta = 1) +
          ridge(cv_D_cat, theta = 1),
        data = train_data,
        x = TRUE,
        y = TRUE
      )

      folds_results[[k]] <- compute_metrics(
        fit, test_data, times_auc_eval
      )
    }

    imputation_results[[m_idx]] <- average_fold_metrics(folds_results)
  }

  pooled_cv_results <- list(
    Harrell_C = pool_metric(pool_all(imputation_results, "Harrell_C")),
    Brier     = pool_metric(pool_all(imputation_results, "Brier")),
    CalSlope  = pool_metric(pool_all(imputation_results, "CalSlope")),
    TV_AUC    = pool_tv_vector(imputation_results, "TV_AUC"),
    tvb       = pool_tv_vector(imputation_results, "Brier_Times")
  )

  gender_results[[g]] <- pooled_cv_results
}

##### Obtaining the results ######
######### MALE #########

gender_results$Male$Harrell_C
gender_results$Male$Brier
gender_results$Male$CalSlope

tvauc_male <- data.frame(
  time  = times_auc_eval,
  mean  = sapply(gender_results$Male$TV_AUC, function(x) x$mean),
  lower = sapply(gender_results$Male$TV_AUC, function(x) x$ci[1]),
  upper = sapply(gender_results$Male$TV_AUC, function(x) x$ci[2])
)

tvauc_male

tvb_male <- data.frame(
  time  = times_auc_eval,
  mean  = sapply(gender_results$Male$tvb[-1], function(x) x$mean),
  lower = sapply(gender_results$Male$tvb[-1], function(x) x$ci[1]),
  upper = sapply(gender_results$Male$tvb[-1], function(x) x$ci[2])
)

tvb_male

########## FEMALE #########

gender_results$Female$Harrell_C
gender_results$Female$Brier
gender_results$Female$CalSlope


tvauc_female <- data.frame(
  time  = times_auc_eval,
  mean  = sapply(gender_results$Female$TV_AUC, function(x) x$mean),
  lower = sapply(gender_results$Female$TV_AUC, function(x) x$ci[1]),
  upper = sapply(gender_results$Female$TV_AUC, function(x) x$ci[2])
)

tvauc_female

tvb_female <- data.frame(
  time  = times_auc_eval,
  mean  = sapply(gender_results$Female$tvb[-1], function(x) x$mean),
  lower = sapply(gender_results$Female$tvb[-1], function(x) x$ci[1]),
  upper = sapply(gender_results$Female$tvb[-1], function(x) x$ci[2])
)

