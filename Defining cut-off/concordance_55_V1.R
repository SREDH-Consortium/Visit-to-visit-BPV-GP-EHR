rm(list=ls())

library(survival)

df = BPV.55_3022
df$outcome = as.numeric(df$outcome)

##### sd_S #######

# Create a vector of cutoff values to try
cutoffs <- quantile(df$sd_S, probs = seq(0.1, 0.9, by = 0.05))  # or use a custom sequence
results_sd_S <- data.frame(cutoff = cutoffs, concordance = NA)

# Loop through each cutoff
for (i in seq_along(cutoffs)) {
  cutoff <- cutoffs[i]

  # Create binary BP indicator
  df$sd_S_binary <- as.numeric(df$sd_S >= cutoff)

  # Fit Cox model
  model <- coxph(Surv(time_ms, outcome) ~ sd_S_binary, data = df)

  # Extract concordance index
  concordance <- summary(model)$concordance[1]  # first element is the C-index
  results_sd_S$concordance[i] <- concordance
}

print(results_sd_S)

write.csv (results_sd_S, "concordance_sd_S_results.csv", row.names = FALSE)

plot(results_sd_S$cutoff, results_sd_S$concordance, type = "b", pch = 16,
     xlab = "BPV Cutoff", ylab = "Concordance Index (C-index)",
     main = "C-index vs SD of Systolic BPV Threshold")

###### sd_D #######

# Create a vector of cutoff values to try
cutoffs <- quantile(df$sd_D, probs = seq(0.1, 0.9, by = 0.05))  # or use a custom sequence
results_sd_D <- data.frame(cutoff = cutoffs, concordance = NA)

# Loop through each cutoff
for (i in seq_along(cutoffs)) {
  cutoff <- cutoffs[i]

  # Create binary BP indicator
  df$sd_D_binary <- as.numeric(df$sd_D >= cutoff)

  # Fit Cox model
  model <- coxph(Surv(time_ms, outcome) ~ sd_D_binary, data = df)

  # Extract concordance index
  concordance <- summary(model)$concordance[1]  # first element is the C-index
  results_sd_D$concordance[i] <- concordance
}

print(results_sd_D)

write.csv (results_sd_D, "concordance_sd_D_results.csv", row.names = FALSE)

plot(results_sd_D$cutoff, results_sd_D$concordance, type = "b", pch = 16,
     xlab = "BPV Cutoff", ylab = "Concordance Index (C-index)",
     main = "C-index vs SD of Diastolic BPV Threshold")


###### cv_S #######

# Create a vector of cutoff values to try
cutoffs <- quantile(df$cv_S, probs = seq(0.1, 0.9, by = 0.05))  # or use a custom sequence
results_cv_S <- data.frame(cutoff = cutoffs, concordance = NA)

# Loop through each cutoff
for (i in seq_along(cutoffs)) {
  cutoff <- cutoffs[i]

  # Create binary BP indicator
  df$cv_S_binary <- as.numeric(df$cv_S >= cutoff)

  # Fit Cox model
  model <- coxph(Surv(time_ms, outcome) ~ cv_S_binary, data = df)

  # Extract concordance index
  concordance <- summary(model)$concordance[1]  # first element is the C-index
  results_cv_S$concordance[i] <- concordance
}

print(results_cv_S)

write.csv (results_cv_S, "concordance_cv_S_results.csv", row.names = FALSE)

plot(results_cv_S$cutoff, results_cv_S$concordance, type = "b", pch = 16,
     xlab = "BPV Cutoff", ylab = "Concordance Index (C-index)",
     main = "C-index vs CV of Systolic BPV Threshold")


###### cv_D #######

# Create a vector of cutoff values to try
cutoffs <- quantile(df$cv_D, probs = seq(0.1, 0.9, by = 0.05))  # or use a custom sequence
results_cv_D <- data.frame(cutoff = cutoffs, concordance = NA)

# Loop through each cutoff
for (i in seq_along(cutoffs)) {
  cutoff <- cutoffs[i]

  # Create binary BP indicator
  df$cv_D_binary <- as.numeric(df$cv_D >= cutoff)

  # Fit Cox model
  model <- coxph(Surv(time_ms, outcome) ~ cv_D_binary, data = df)

  # Extract concordance index
  concordance <- summary(model)$concordance[1]  # first element is the C-index
  results_cv_D$concordance[i] <- concordance
}

print(results_cv_D)

write.csv (results_cv_D, "concordance_cv_D_results.csv", row.names = FALSE)

plot(results_cv_S$cutoff, results_cv_S$concordance, type = "b", pch = 16,
     xlab = "BPV Cutoff", ylab = "Concordance Index (C-index)",
     main = "C-index vs CV of Diastolic BPV Threshold")


###### arv_S #######

# Create a vector of cutoff values to try
cutoffs <- quantile(df$arv_S, probs = seq(0.1, 0.9, by = 0.05))  # or use a custom sequence
results_arv_S <- data.frame(cutoff = cutoffs, concordance = NA)

# Loop through each cutoff
for (i in seq_along(cutoffs)) {
  cutoff <- cutoffs[i]

  # Create binary BP indicator
  df$arv_S_binary <- as.numeric(df$arv_S >= cutoff)

  # Fit Cox model
  model <- coxph(Surv(time_ms, outcome) ~ arv_S_binary, data = df)

  # Extract concordance index
  concordance <- summary(model)$concordance[1]  # first element is the C-index
  results_arv_S$concordance[i] <- concordance
}

print(results_arv_S)

write.csv (results_arv_S, "concordance_arv_S_results.csv", row.names = FALSE)

plot(results_arv_S$cutoff, results_arv_S$concordance, type = "b", pch = 16,
     xlab = "BPV Cutoff", ylab = "Concordance Index (C-index)",
     main = "C-index vs ARV of Systolic BPV Threshold")


###### arv_D #######

# Create a vector of cutoff values to try
cutoffs <- quantile(df$arv_D, probs = seq(0.1, 0.9, by = 0.05))  # or use a custom sequence
results_arv_D <- data.frame(cutoff = cutoffs, concordance = NA)

# Loop through each cutoff
for (i in seq_along(cutoffs)) {
  cutoff <- cutoffs[i]

  # Create binary BP indicator
  df$arv_D_binary <- as.numeric(df$arv_D >= cutoff)

  # Fit Cox model
  model <- coxph(Surv(time_ms, outcome) ~ arv_D_binary, data = df)

  # Extract concordance index
  concordance <- summary(model)$concordance[1]  # first element is the C-index
  results_arv_D$concordance[i] <- concordance
}

print(results_arv_D)

write.csv (results_arv_D, "concordance_arv_D_results.csv", row.names = FALSE)

plot(results_arv_D$cutoff, results_arv_D$concordance, type = "b", pch = 16,
     xlab = "BPV Cutoff", ylab = "Concordance Index (C-index)",
     main = "C-index vs ARV of Diastolic BPV Threshold")
