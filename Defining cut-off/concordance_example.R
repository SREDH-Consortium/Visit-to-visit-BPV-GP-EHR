rm(list=ls())

library(survival)

setwd("C:/Users/nicks/OneDrive - UNSW/Documents/Consults/Mifetika Lukitasari")
df = readxl::read_xlsx("example data.xlsx")
df$outcome = as.numeric(df$outcome)

# Create a vector of cutoff values to try
cutoffs <- quantile(df$sd_S, probs = seq(0.1, 0.9, by = 0.05))  # or use a custom sequence
results <- data.frame(cutoff = cutoffs, concordance = NA)

# Loop through each cutoff
for (i in seq_along(cutoffs)) {
  cutoff <- cutoffs[i]
  
  # Create binary BP indicator
  df$sd_S_binary <- as.numeric(df$sd_S >= cutoff)
  
  # Fit Cox model
  model <- coxph(Surv(time_at_event, outcome) ~ sd_S_binary, data = df)
  
  # Extract concordance index
  concordance <- summary(model)$concordance[1]  # first element is the C-index
  results$concordance[i] <- concordance
}

print(results)

plot(results$cutoff, results$concordance, type = "b", pch = 16,
     xlab = "BPV Cutoff", ylab = "Concordance Index (C-index)",
     main = "C-index vs BPV Threshold")






