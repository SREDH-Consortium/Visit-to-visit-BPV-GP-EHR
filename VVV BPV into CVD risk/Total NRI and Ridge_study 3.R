run_total_analysis_detailed <- function(imputed_data, K_folds = 10, times_auc_eval = c(12, 24, 36, 48, 60)) {
  
  cat("\n>>> Running Total Population Analysis (Detailed components) <<<\n")
  
  data_first <- complete(imputed_data, 1)
  n_total <- nrow(data_first)
  set.seed(123)
  fold_ids <- sample(rep(1:K_folds, length.out = n_total))
  
  # Storage for pooling across imputations
  imp_nri_total <- c()
  imp_nri_event <- c()
  imp_nri_nonevent <- c()
  
  for (m_idx in 1:imputed_data$m) {
    cat("  Processing Imputation:", m_idx, "\n")
    
    current_data <- complete(imputed_data, m_idx) %>%
      mutate(
        Gender_num        = as.numeric(as.factor(Gender)) - 1,
        SmokingStatus_num = as.numeric(as.factor(SmokingStatus)) - 1,
        FH_num            = as.numeric(as.factor(FH)) - 1,
        com_ht_num        = as.numeric(as.factor(com_ht)) - 1,
        com_dm_num        = as.numeric(as.factor(com_dm)) - 1,
        med_ht_num        = as.numeric(as.factor(med_ht)) - 1,
        med_dm_num        = as.numeric(as.factor(med_dm)) - 1,
        med_lipid_num     = as.numeric(as.factor(med_lipid)) - 1
      )
    
    current_data$fold_id <- fold_ids
    
    # Storage for fold averages
    fold_total <- c(); fold_event <- c(); fold_nonevent <- c()
    
    for (k in 1:K_folds) {
      train_data <- current_data %>% filter(fold_id != k)
      test_data  <- current_data %>% filter(fold_id == k)
      
      fit_std <- coxph(Surv(time_ms, outcome) ~ ridge(ObservationValue_S, theta=1) + ridge(ObservationValue_D, theta=1) + ridge(Gender_num, theta=1) + 
                         ridge(SmokingStatus_num, theta=1) + ridge(fglucose, theta=1) + ridge(bmi, theta=1) + 
                         ridge(Value_TCHDL, theta=1) + ridge(egfr, theta=1) + ridge(FH_num, theta=1) + 
                         ridge(com_ht_num, theta=1) + ridge(com_dm_num, theta=1) + ridge(med_ht_num, theta=1) + 
                         ridge(med_dm_num, theta=1) + ridge(med_lipid_num, theta=1),
                       data = train_data, x = TRUE, y = TRUE)
      
      fit_new <- coxph(Surv(time_ms, outcome) ~ ridge(cv_S_cat, theta=1) + ridge(cv_D_cat, theta=1) + ridge(Gender_num, theta=1) + 
                         ridge(SmokingStatus_num, theta=1) + ridge(fglucose, theta=1) + ridge(bmi, theta=1) + 
                         ridge(Value_TCHDL, theta=1) + ridge(egfr, theta=1) + ridge(FH_num, theta=1) + 
                         ridge(com_ht_num, theta=1) + ridge(com_dm_num, theta=1) + ridge(med_ht_num, theta=1) + 
                         ridge(med_dm_num, theta=1) + ridge(med_lipid_num, theta=1) + ridge(ObservationValue_S, theta=1) + ridge(ObservationValue_D, theta=1),
                       data = train_data, x = TRUE, y = TRUE)
      
      class(fit_std) <- "coxph"
      class(fit_new) <- "coxph"
      
      nri_res <- nricens(mdl.std = fit_std, mdl.new = fit_new, t0 = 60, cut = c(0.05, 0.1), niter = 0)
      
      # Extract all three components
      fold_total[k]    <- nri_res$nri["NRI", "Estimate"]
      fold_event[k]    <- nri_res$nri["NRI+", "Estimate"]
      fold_nonevent[k] <- nri_res$nri["NRI-", "Estimate"]
    }
    
    # Average across folds for this imputation
    imp_nri_total[m_idx]    <- mean(fold_total, na.rm = TRUE)
    imp_nri_event[m_idx]    <- mean(fold_event, na.rm = TRUE)
    imp_nri_nonevent[m_idx] <- mean(fold_nonevent, na.rm = TRUE)
  }
  
  # Final pooling across imputations
  return(list(
    Total_NRI    = pool_metric(imp_nri_total),
    Event_NRI    = pool_metric(imp_nri_event),
    NonEvent_NRI = pool_metric(imp_nri_nonevent)
  ))
}

# --- Execute ---
nri_results <- run_total_analysis_detailed(imputed_with_orig)

# --- Print Results ---
cat("\n--- FINAL POOLED RESULTS ---")
cat("\nTotal Population NRI:\n"); print(nri_results$Total_NRI)
cat("\nEvent NRI (NRI+): \n");   print(nri_results$Event_NRI)
cat("\nNon-Event NRI (NRI-): \n"); print(nri_results$NonEvent_NRI)
