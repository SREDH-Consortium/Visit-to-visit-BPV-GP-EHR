library(metafor)
library(ggplot2)

meta_data2 <- SD_DOSRESMETA_V7

# Ensure data is formatted correctly
# Columns: study_id, Dose, logrr, Hazard_Ratio, cases, n, se
meta_data2 <- meta_data2[meta_data2$Hazard_Ratio != 1 & meta_data2$logrr != 0, ]  # Exclude reference rows


###### QUADRATIC ########

# Fit the Meta-Analysis Model
meta_analysisSD <- rma(yi = yi, 
                     sei = se, 
                     mods = ~ Dose + I(Dose^2), 
                     data = meta_data2, 
                     method = "REML")

# Print the results
print(meta_analysisSD)

# Create a sequence of Dose values
dose_seq <- seq(0, 20, by = 5)

# Create a new dataset with the desired Dose values
new_doses <- data.frame(Dose = dose_seq)
new_doses$Dose2 <- new_doses$Dose^2

# Predict log relative risk (logrr) for the new Dose values
predictionsSD <- predict(meta_analysisSD, newmods = cbind(new_doses$Dose, new_doses$Dose2))

# Convert log relative risk to hazard ratio
predictionsSD$hr <- exp(predictionsSD$pred)

# Print the predictions
print(predictionsSD)

# Calculate AIC and BIC
aic_valueSD <- AIC(meta_analysisSD)
bic_valueSD <- BIC(meta_analysisSD)

print (aic_valueSD)
print (bic_valueSD)

loglik_valueSD <- logLik(meta_analysisSD)
print(loglik_valueSD)

as.numeric(loglik_valueSD)

# Extract coefficients
coeffs <- coef(meta_analysisSD)

target_logrr <- log(1.10)


# Define the equation to solve
equation <- function(dose) {
  target_logrr - (coeffs[1] + coeffs[2] * dose + coeffs[3] * dose^2)
}

# Solve for Dose
solution <- uniroot(equation, lower = 0, upper = 20)

# Extract the dose corresponding to HR = 1.10
dose_for_hr_1_10 <- solution$root

# Print the dose
cat("Dose corresponding to HR = 1.10:", dose_for_hr_1_10, "\n")

# Data for plotting
# plot_dataSD <- data.frame(
#   dose = c(0, 5, 10, 15, 20),
#   pred = c(-0.1614, -0.0077, 0.1458, 0.2993, 0.4527),
#   se = c(0.1081, 0.0496, 0.0196, 0.0203, 0.0229)
# )


# Data for plotting
plot_dataSD <- data.frame(
  dose = c(0, 5, 10, 15, 20),
  pred = c(-0.3273, -0.0036, 0.2519, 0.4393, 0.5586),
  se = c(0.3123, 0.1392, 0.0551, 0.0633, 0.0748)
)

# Calculate confidence intervals
plot_dataSD$ci.lb <- plot_dataSD$pred - 1.96 * plot_dataSD$se
plot_dataSD$ci.ub <- plot_dataSD$pred + 1.96 * plot_dataSD$se

# Convert log relative risk to hazard ratio
plot_dataSD$hr <- exp(plot_dataSD$pred)
plot_dataSD$hr.ci.lb <- exp(plot_dataSD$ci.lb)
plot_dataSD$hr.ci.ub <- exp(plot_dataSD$ci.ub)


# Create the plot
ggplot(plot_dataSD, aes(x = dose, y = hr)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = hr.ci.lb, ymax = hr.ci.ub), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 1.10, linetype = "dotted", color = "red") +
  labs(title = "Dose-Response Meta-Analysis of Standard Deviation",
       x = "Standard Deviation (mmHg)",
       y = "Hazard Ratio",
       caption = "Shaded area: 95% Confidence Intervals") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal()

SD <- ggplot(plot_dataSD, aes(x = dose, y = hr)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = hr.ci.lb, ymax = hr.ci.ub), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 1.10, linetype = "dotted", color = "red") +
  labs(title = "(A) Dose-Response Meta-Analysis of Standard Deviation",
       x = "Standard Deviation (mmHg)",
       y = "Hazard Ratio",
       caption = "Shaded area: 95% Confidence Intervals") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),  # Remove all grid lines
    axis.line = element_line(color = "black")  # Add axis lines back
  )

# Save
ggsave("dose_response_clean_axes SD.png", plot = SD, width = 8, height = 6, dpi = 600)


# Specify dose values for prediction
dose_seq <- c(0, 5, 10, 15, 20)
new_doses <- data.frame(Dose = dose_seq,
                        Dose2 = dose_seq^2)

# Predict log-HR and 95% CI
predicted <- predict(meta_analysisSD, newmods = cbind(new_doses$Dose, new_doses$Dose2))

# Convert to HRs
predicted$Dose      <- dose_seq
predicted$HR        <- exp(predicted$pred)
predicted$HR.ci.lb  <- exp(predicted$ci.lb)
predicted$HR.ci.ub  <- exp(predicted$ci.ub)

# Print results
print(predicted[, c("Dose", "HR", "HR.ci.lb", "HR.ci.ub")])



#### RCS ########

library(metafor)
library(splines)

# Prepare the data (ensure columns are correctly formatted)
meta_data2$Dose <- as.numeric(meta_data2$Dose)
meta_data2$logrr <- as.numeric(meta_data2$logrr)
meta_data2$se <- as.numeric(meta_data2$se)

# Define the number of knots and their positions
knots <- quantile(meta_data$Dose, probs = c(0.25, 0.5, 0.75))

# Create the spline basis
spline_basis <- ns(meta_data2$Dose, knots = knots)

# Fit the meta-analysis model
meta_analysis_SDspline <- rma(yi = logrr, 
                     sei = se, 
                     mods = ~ spline_basis, 
                     data = meta_data2, 
                     method = "REML")

# Print the results
print(meta_analysis_SDspline)

# Create a sequence of Dose values
dose_seq <- seq(0, 20, by = 5)

# Create the spline basis for the new dose values
new_spline_basis_SD <- ns(dose_seq, knots = knots)

# Predict log relative risk (logrr) for the new Dose values
predictions_SDspline <- predict(meta_analysis_SDspline, newmods = new_spline_basis_SD)

# Convert log relative risk to hazard ratio
predictions_SDspline$hr <- exp(predictions_SDspline$pred)

# Print the predictions
print(predictions_SDspline)

# Calculate AIC and BIC
aic_value_SDspline <- AIC(meta_analysis_SDspline)
bic_value_SDspline <- BIC(meta_analysis_SDspline)

print (aic_value_SDspline)
print (bic_value_SDspline)

# Print AIC and BIC values
cat("AIC:", aic_value_SDspline, "\n")
cat("BIC:", bic_value_SDspline, "\n")


###### LINEAR ######

# Ensure your data is correctly formatted
meta_data2$Dose <- as.numeric(meta_data2$Dose)
meta_data2$logrr <- as.numeric(meta_data2$logrr)
meta_data2$se <- as.numeric(meta_data2$se)

# Fit the meta-analysis model
meta_analysis_linSD <- rma(yi = logrr, 
                     sei = se, 
                     mods = ~ Dose, 
                     data = meta_data2, 
                     method = "REML")

# Print the results
print(meta_analysis_linSD)

# Calculate AIC and BIC
aic_value_linSD <- AIC(meta_analysis_linSD)
bic_value_linSD <- BIC(meta_analysis_linSD)

print (aic_value_linSD)
print (bic_value_linSD)

# Create the specified dose sequence
dose_seq <- seq(0, 20, by = 5)

# Create new data frame for predictions
new_data <- data.frame(Dose = dose_seq)

# Convert new_data to a matrix
new_mods <- as.matrix(new_data)


# Predict log relative risk (logrr) for the new Dose values
predictions_linSD <- predict(meta_analysis_linSD, newmods = new_mods)

# Convert log relative risk to hazard ratio
predictions_linSD_df <- data.frame(
  Dose = dose_seq,
  logrr = predictions_linSD$pred,
  ci.lb = predictions_linSD$ci.lb,
  ci.ub = predictions_linSD$ci.ub
)
predictions_linSD_df$hr <- exp(predictions_linSD_df$logrr)
predictions_linSD_df$hr.ci.lb <- exp(predictions_linSD_df$ci.lb)
predictions_linSD_df$hr.ci.ub <- exp(predictions_linSD_df$ci.ub)

# Print the predictions
print(predictions_linSD_df)


# Create the plot
ggplot(predictions_linSD_df, aes(x = Dose, y = hr)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = hr.ci.lb, ymax = hr.ci.ub), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 1.10, linetype = "dotted", color = "red") +
  labs(title = "Dose-Response Meta-Analysis of Standard Deviation",
       x = "Standard Deviation",
       y = "Hazard Ratio",
       caption = "Shaded area: 95% Confidence Intervals") +
  theme_minimal()


target_logrr <- log(1.10)  # This is approximately 0.0953

# Extract coefficients from the model
coeffs <- coef(meta_analysis_linSD)

# Define the equation to solve
equation <- function(dose) {
  logrr_pred <- coeffs[1] + coeffs[2] * dose  # Linear model: intercept + slope * dose
  target_logrr - logrr_pred
}

# Solve for the Dose using uniroot
solution <- uniroot(equation, lower = min(meta_data2$Dose), upper = max(meta_data2$Dose))

# Extract the dose corresponding to HR = 1.10
dose_for_hr_1_10 <- solution$root

# Print the dose
cat("Dose corresponding to HR = 1.10:", dose_for_hr_1_10, "\n")

# Data for interpolation
dose_seq <- c(0, 5, 10, 15, 20)
hr_values <- c(0.9684305, 1.1187487, 1.2923991, 1.4930031, 1.7247446)

# Target HR
target_hr <- 1.10

# Linear interpolation to find the dose corresponding to HR = 1.10
interpolated_dose <- approx(x = hr_values, y = dose_seq, xout = target_hr)$y

# Print the interpolated dose
cat("Dose corresponding to HR = 1.10:", interpolated_dose, "\n")

