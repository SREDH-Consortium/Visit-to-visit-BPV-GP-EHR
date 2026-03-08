run_total_analysis <- function(imputed_data, K_folds = 10, times_auc_eval = c(12, 24, 36, 48, 60)) {
  
  cat("\n>>> Running Total Population Analysis <<<\n")
  
  # 1. Generate folds for the WHOLE dataset
  data_first <- complete(imputed_data, 1)
  n_total <- nrow(data_first)
  set.seed(123)
  fold_ids <- sample(rep(1:K_folds, length.out = n_total))
  
  imputation_nri_totals <- c()
  
  for (m_idx in 1:imputed_data$m) {
    cat("  Processing Imputation:", m_idx, "\n")
    
    current_data <- complete(imputed_data, m_idx)
    
    # --- CRITICAL FIX: Convert Categorical Variables to Numeric for ridge() ---
    # This converts "Male"/"Female" to 0/1 so ridge() can handle them
    current_data <- current_data %>%
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
    fold_nris <- c()
    
    for (k in 1:K_folds) {
      train_data <- current_data %>% filter(fold_id != k)
      test_data  <- current_data %>% filter(fold_id == k)
      
      # MODEL A: Standard (Using numeric versions of variables)
      fit_std <- coxph(
        Surv(time_ms, outcome) ~ ridge(ObservationValue_S, theta=1) + ridge(Gender_num, theta=1) + 
          ridge(SmokingStatus_num, theta=1) + ridge(fglucose, theta=1) + ridge(bmi, theta=1) + 
          ridge(Value_TCHDL, theta=1) + ridge(egfr, theta=1) + ridge(FH_num, theta=1) + 
          ridge(com_ht_num, theta=1) + ridge(com_dm_num, theta=1) + ridge(med_ht_num, theta=1) + 
          ridge(med_dm_num, theta=1) + ridge(med_lipid_num, theta=1),
        data = train_data, x = TRUE, y = TRUE
      )
      
      # MODEL B: New (Using numeric versions of variables)
      fit_new <- coxph(
        Surv(time_ms, outcome) ~ ridge(ObservationValue_S, theta=1) + ridge(sd_S_cat, theta=1) + ridge(Gender_num, theta=1) + 
          ridge(SmokingStatus_num, theta=1) + ridge(fglucose, theta=1) + ridge(bmi, theta=1) + 
          ridge(Value_TCHDL, theta=1) + ridge(egfr, theta=1) + ridge(FH_num, theta=1) + 
          ridge(com_ht_num, theta=1) + ridge(com_dm_num, theta=1) + ridge(med_ht_num, theta=1) + 
          ridge(med_dm_num, theta=1) + ridge(med_lipid_num, theta=1),
        data = train_data, x = TRUE, y = TRUE
      )
      
      # Apply Class Hack
      class(fit_std) <- "coxph"
      class(fit_new) <- "coxph"
      
      # Calculate NRI
      nri_res <- nricens(mdl.std = fit_std, mdl.new = fit_new, 
                         t0 = 60, cut = c(0.05, 0.1), niter = 0)
      
      fold_nris[k] <- nri_res$nri["NRI", "Estimate"]
    }
    imputation_nri_totals[m_idx] <- mean(fold_nris, na.rm = TRUE)
  }
  
  return(pool_metric(imputation_nri_totals))
}

# --- Execute ---
nri_total_population <- run_total_analysis(imputed_with_orig)
print(nri_total_population)


##################### GENDER ##################
# Define the thresholds
nri_cuts <- c(0.05, 0.1) 

# Inside the fold loop (k in 1:K_folds):
nri_res <- nricens(
  mdl.std = fit_std, 
  mdl.new = fit_new, 
  t0 = 60,              # evaluation at 5 years / 60 months
  cut = nri_cuts,       # <5%, 5-10%, >10%
  niter = 0             # 0 because we are using CV/MI for stability
)


run_gender_analysis <- function(imputed_data, gender_label, K_folds = 10, times_auc_eval = c(12, 24, 36, 48, 60)) {
  
  cat("\nProcessing Gender:", gender_label, "\n")
  
  data_first <- complete(imputed_data, 1) %>% filter(Gender == gender_label)
  n_g <- nrow(data_first)
  set.seed(123)
  fold_ids <- sample(rep(1:K_folds, length.out = n_g))
  
  imputation_nri_totals <- c()
  
  for (m_idx in 1:imputed_data$m) {
    cat("  Imputation:", m_idx, "\r")
    current_data <- complete(imputed_data, m_idx) %>% filter(Gender == gender_label)
    current_data$fold_id <- fold_ids
    fold_nris <- c()
    
    for (k in 1:K_folds) {
      train_data <- current_data %>% filter(fold_id != k)
      test_data  <- current_data %>% filter(fold_id == k)
      
      # 1. Fit Ridge Models
      fit_std <- coxph(
        Surv(time_ms, outcome) ~ ridge(ObservationValue_S, theta=1) + ridge(SmokingStatus, theta=1) + 
          ridge(fglucose, theta=1) + ridge(bmi, theta=1) + ridge(Value_TCHDL, theta=1) + 
          ridge(egfr, theta=1) + ridge(FH, theta=1) + ridge(com_ht, theta=1) + 
          ridge(com_dm, theta=1) + ridge(med_ht, theta=1) + 
          ridge(med_dm, theta=1) + ridge(med_lipid, theta=1),
        data = train_data, x = TRUE, y = TRUE
      )
      
      fit_new <- coxph(
        Surv(time_ms, outcome) ~ ridge(sd_S_cat, theta=1) + ridge(SmokingStatus, theta=1) + 
          ridge(fglucose, theta=1) + ridge(bmi, theta=1) + ridge(Value_TCHDL, theta=1) + 
          ridge(egfr, theta=1) + ridge(FH, theta=1) + ridge(com_ht, theta=1) + 
          ridge(com_dm, theta=1) + ridge(med_ht, theta=1) + 
          ridge(med_dm, theta=1) + ridge(med_lipid, theta=1),
        data = train_data, x = TRUE, y = TRUE
      )
      
      # 2. THE CLASS HACK
      # This fixes the "condition has length > 1" error by forcing the 
      # model to report only ONE class name ("coxph").
      class(fit_std) <- "coxph"
      class(fit_new) <- "coxph"
      
      # 3. Run NRI
      # We use the test_data for evaluation
      nri_res <- nricens(
        mdl.std = fit_std, 
        mdl.new = fit_new, 
        t0      = 60, 
        cut     = c(0.05, 0.1), 
        niter   = 0
      )
      
      fold_nris[k] <- nri_res$nri["NRI", "Estimate"]
    }
    imputation_nri_totals[m_idx] <- mean(fold_nris, na.rm = TRUE)
  }
  
  return(pool_metric(imputation_nri_totals))
}
# RUN ANALYSIS
nri_male <- run_gender_analysis(imputed_with_orig, "Male")
nri_female <- run_gender_analysis(imputed_with_orig, "Female")

# PRINT RESULTS
cat("\nPooled NRI for Males (Categories: <5, 5-10, >10):\n")
print(nri_male)

cat("\nPooled NRI for Females (Categories: <5, 5-10, >10):\n")
print(nri_female)
