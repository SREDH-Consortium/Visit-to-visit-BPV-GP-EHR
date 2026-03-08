library(survival)
library(mice)
library(dplyr)
library(pec)
library(timeROC)
library(rms)

# --- Keep your utility functions exactly as they are ---
# (compute_metrics, average_fold_metrics, pool_metric, pool_all, pool_tv_vector)
# [I'm assuming these are already defined in your environment]

# --- Define the Analysis Function ---
# This replaces the loop and handles the specific subsetting for each gender
run_gender_analysis <- function(imputed_data, gender_label, K_folds = 10, times_auc_eval = c(12, 24, 36, 48, 60)) {
  
  cat("\nStarting analysis for Gender:", gender_label, "\n")
  
  # 1. Generate folds within the gender group using the first imputation
  data_first <- complete(imputed_data, 1) %>% filter(Gender == gender_label)
  n_g <- nrow(data_first)
  
  set.seed(123)
  fold_ids <- sample(rep(1:K_folds, length.out = n_g))
  
  imputation_results <- list()
  
  # 2. Loop over Imputations
  for (m_idx in 1:imputed_data$m) {
    cat("  Processing Imputation:", m_idx, "\r")
    
    current_data <- complete(imputed_data, m_idx) %>%
      filter(Gender == gender_label)
    
    current_data$fold_id <- fold_ids
    folds_results <- list()
    
    # 3. Loop over Folds (Cross-Validation)
    for (k in 1:K_folds) {
      train_data <- current_data %>% filter(fold_id != k)
      test_data  <- current_data %>% filter(fold_id == k)
      
      # Fit Ridge-Penalised Cox
      fit <- coxph(
        Surv(time_ms, outcome) ~ 
          ridge(SmokingStatus, theta = 1) + ridge(fglucose, theta = 1) + 
          ridge(bmi, theta = 1) + ridge(Value_TCHDL, theta = 1) + 
          ridge(egfr, theta = 1) + ridge(FH, theta = 1) + 
          ridge(com_ht, theta = 1) + ridge(com_dm, theta = 1) + 
          ridge(med_ht, theta = 1) + ridge(med_dm, theta = 1) + 
          ridge(med_lipid, theta = 1) + ridge(sd_S_cat, theta = 1),
        data = train_data, x = TRUE, y = TRUE
      )
      
      folds_results[[k]] <- compute_metrics(fit, test_data, times_auc_eval)
    }
    
    imputation_results[[m_idx]] <- average_fold_metrics(folds_results)
  }
  
  # 4. Pooling across imputations using Rubin's Rules logic
  pooled_cv_results <- list(
    Harrell_C = pool_metric(pool_all(imputation_results, "Harrell_C")),
    Brier     = pool_metric(pool_all(imputation_results, "Brier")),
    CalSlope  = pool_metric(pool_all(imputation_results, "CalSlope")),
    TV_AUC    = pool_tv_vector(imputation_results, "TV_AUC"),
    tvb       = pool_tv_vector(imputation_results, "Brier_Times")
  )
  
  return(pooled_cv_results)
}

# --- Execute Analysis for each dataset separately ---

# Run for Males
results_male <- run_gender_analysis(imputed_with_orig, "Male")

# Run for Females
results_female <- run_gender_analysis(imputed_with_orig, "Female")


# --- Process and Print Results for MALE ---
cat("\n--- MALE RESULTS ---\n")
print(results_male$Harrell_C)
print(results_male$Brier)
print(results_male$CalSlope)

tvauc_male <- data.frame(
  time  = times_auc_eval,
  mean  = sapply(results_male$TV_AUC, function(x) x$mean),
  lower = sapply(results_male$TV_AUC, function(x) x$ci[1]),
  upper = sapply(results_male$TV_AUC, function(x) x$ci[2])
)
print(tvauc_male)

tvb_male <- data.frame(
  time  = times_auc_eval,
  mean  = sapply(results_male$tvb[-1], function(x) x$mean),
  lower = sapply(results_male$tvb[-1], function(x) x$ci[1]),
  upper = sapply(results_male$tvb[-1], function(x) x$ci[2])
)
print(tvb_male)


# --- Process and Print Results for FEMALE ---
cat("\n--- FEMALE RESULTS ---\n")
print(results_female$Harrell_C)
print(results_female$Brier)
print(results_female$CalSlope)

tvauc_female <- data.frame(
  time  = times_auc_eval,
  mean  = sapply(results_female$TV_AUC, function(x) x$mean),
  lower = sapply(results_female$TV_AUC, function(x) x$ci[1]),
  upper = sapply(results_female$TV_AUC, function(x) x$ci[2])
)
print(tvauc_female)

tvb_female <- data.frame(
  time  = times_auc_eval,
  mean  = sapply(results_female$tvb[-1], function(x) x$mean),
  lower = sapply(results_female$tvb[-1], function(x) x$ci[1]),
  upper = sapply(results_female$tvb[-1], function(x) x$ci[2])
)
print(tvb_female)