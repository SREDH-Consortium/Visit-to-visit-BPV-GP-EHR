# Install and load the metafor package
# install.packages("metafor")
meta_data <- CV_DOSRESMETA_V4

library(metafor)
library(ggplot2)
library(splines)

# Calculate variance (SE squared)
meta_data$var <- meta_data$se^2

# Define assumed correlation (rho)
rho <- 0.5  # Common assumption for dose-response studies

# Create variance-covariance matrix
V <- matrix(0, nrow = nrow(meta_data), ncol = nrow(meta_data)) 

for (i in 1:nrow(meta_data)) {
  for (j in 1:nrow(meta_data)) {
    V[i, j] <- ifelse(i == j, meta_data$var[i], rho * sqrt(meta_data$var[i] * meta_data$var[j]))
  }
}

# Print variance-covariance matrix
print(V)

# Add quadratic term
meta_data$dose_sq <- meta_data$Dose^2

meta_data <- meta_data %>%
  rename (yi = logrr)

# Fit quadratic dose-response meta-analysis
meta_analysis <- rma.mv(yi, V, mods = ~ Dose + dose_sq, 
                        random = ~ 1 | study_id, data = meta_data)

# View results
summary(meta_analysis)

# Load necessary libraries
library(metafor)
library(ggplot2)

# Create the specified dose sequence
dose_seq <- seq(0, 20, by = 5)

# Create a new dataset with the desired Dose values
new_doses <- data.frame(Dose = dose_seq)
new_doses$Dose2 <- new_doses$Dose^2

# Predict log relative risk (logrr) for the new Dose values
predictions <- predict(meta_analysis, newmods = cbind(new_doses$Dose, new_doses$Dose2))

# Convert log relative risk to hazard ratio
predictions_df <- data.frame(
  Dose = dose_seq,
  logrr = predictions$pred,
  ci.lb = predictions$ci.lb,
  ci.ub = predictions$ci.ub
)
predictions_df$hr <- exp(predictions_df$logrr)
predictions_df$hr.ci.lb <- exp(predictions_df$ci.lb)
predictions_df$hr.ci.ub <- exp(predictions_df$ci.ub)

# Print the predictions
print(predictions_df)


# Extract coefficients
coeffs <- coef(meta_analysis)

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

# Specify dose values for prediction
dose_seq <- c(0, 5, 10, 15, 20)
new_doses <- data.frame(Dose = dose_seq,
                        Dose2 = dose_seq^2)

# Predict log-HR and 95% CI
predicted <- predict(meta_analysis, newmods = cbind(new_doses$Dose, new_doses$Dose2))

# Convert to HRs
predicted$Dose      <- dose_seq
predicted$HR        <- exp(predicted$pred)
predicted$HR.ci.lb  <- exp(predicted$ci.lb)
predicted$HR.ci.ub  <- exp(predicted$ci.ub)

# Print results
print(predicted[, c("Dose", "HR", "HR.ci.lb", "HR.ci.ub")])


# # Create the plot
# ggplot(predictions_df, aes(x = Dose, y = hr)) +
#   geom_line(color = "blue", linewidth = 1) +
#   geom_ribbon(aes(ymin = hr.ci.lb, ymax = hr.ci.ub), fill = "blue", alpha = 0.2) +
#   geom_hline(yintercept = 1.10, linetype = "dotted", color = "red") +
#   labs(title = "Dose-Response Meta-Analysis of Coefficient of Variation",
#        x = "Coefficient of Variation",
#        y = "Hazard Ratio",
#        caption = "Shaded area: 95% Confidence Intervals") +
#   theme_minimal()


# Create the plot
ggplot(predictions_df, aes(x = Dose, y = hr)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = hr.ci.lb, ymax = hr.ci.ub), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 1.10, linetype = "dotted", color = "red") +
  labs(title = "Dose-Response Meta-Analysis of Coefficient of Variation",
       x = "Coefficient of Variation (%)",
       y = "Hazard Ratio",
       caption = "Shaded area: 95% Confidence Intervals") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal()

library(ggplot2)

# Your plot
p <- ggplot(predictions_df, aes(x = Dose, y = hr)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = hr.ci.lb, ymax = hr.ci.ub), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 1.10, linetype = "dotted", color = "red") +
  labs(title = "Dose-Response Meta-Analysis of Coefficient of Variation",
       x = "Coefficient of Variation (%)",
       y = "Hazard Ratio",
       caption = "Shaded area: 95% Confidence Intervals") +
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal()

# Save with high resolution
ggsave("dose_response_plot.png", plot = p, width = 8, height = 6, dpi = 300)


library(ggplot2)

p <- ggplot(predictions_df, aes(x = Dose, y = hr)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = hr.ci.lb, ymax = hr.ci.ub), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 1.10, linetype = "dotted", color = "red") +
  labs(
    title = "(B) Dose-Response Meta-Analysis of Coefficient of Variation",
    x = "Coefficient of Variation (%)",
    y = "Hazard Ratio",
    caption = "Shaded area: 95% Confidence Intervals"
  ) +
  coord_cartesian(ylim = c(0, 2)) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid = element_blank(),  # Remove all grid lines
    axis.line = element_line(color = "black")  # Add axis lines back
  )

# Save
ggsave("dose_response_clean_axes_CV.png", plot = p, width = 8, height = 6, dpi = 600)

###### QUADRATIC #######

# Ensure data is formatted correctly
# Assuming your data is called `meta_data`
# Columns: study_id, Dose, logrr, Hazard_Ratio, cases, n, se
meta_data <- meta_data[meta_data$Hazard_Ratio != 1 & meta_data$se != 0, ]  # Exclude reference rows


# Run a random-effects meta-analysis
meta_analysis <- rma(yi=yi, sei = se, data = meta_data, mods = ~ Dose + I(Dose^2))

# Print the results
print(meta_analysis)


# Random-effects meta-analysis with dose-response relationship
meta_analysis <- rma(yi = yi, 
                     sei = se, 
                     mods = ~ Dose + I(Dose^2), 
                     data = meta_data, 
                     method = "REML")

# Print the results
print(meta_analysis)

# Create a sequence of Dose values
dose_seq <- seq(0, 20, by = 5)

# Create a new dataset with the desired Dose values
new_doses <- data.frame(Dose = dose_seq)
new_doses$Dose2 <- new_doses$Dose^2

# Predict log relative risk (logrr) for the new Dose values
predictions <- predict(meta_analysis, newmods = cbind(new_doses$Dose, new_doses$Dose2))

# Convert log relative risk to hazard ratio
predictions$hr <- exp(predictions$pred)

# Print the predictions
print(predictions)

# Calculate AIC and BIC
aic_value <- AIC(meta_analysis)
bic_value <- BIC(meta_analysis)

print (aic_value)
print (bic_value)


# Extract coefficients
coeffs <- coef(meta_analysis)

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


##### RCS ######

# Prepare the data (ensure columns are correctly formatted)
meta_data$Dose <- as.numeric(meta_data$Dose)
meta_data$logrr <- as.numeric(meta_data$logrr)
meta_data$se <- as.numeric(meta_data$se)

# Define the number of knots and their positions
knots <- quantile(meta_data$Dose, probs = c(0.25, 0.5, 0.75))

# Create the spline basis
spline_basis <- ns(meta_data$Dose, knots = knots)

# Fit the meta-analysis model
meta_analysis_spline_CV <- rma(yi = logrr, 
                     sei = se, 
                     mods = ~ spline_basis, 
                     data = meta_data, 
                     method = "REML")

# Print the results
print(meta_analysis_spline_CV)

# Create a sequence of Dose values
dose_seq <- seq(0, 20, by = 5)

# Create the spline basis for the new dose values
new_spline_basis <- ns(dose_seq, knots = knots)

# Predict log relative risk (logrr) for the new Dose values
predictions <- predict(meta_analysis_spline_CV, newmods = new_spline_basis)

# Convert log relative risk to hazard ratio
predictions$hr <- exp(predictions$pred)

# Print the predictions
print(predictions)

# Calculate AIC and BIC
aic_value_splineCV <- AIC(meta_analysis_spline_CV)
bic_value_splineCV <- BIC(meta_analysis_spline_CV)

print (aic_value_splineCV)
print (bic_value_splineCV)


###### LINEAR ######

# Ensure your data is correctly formatted
meta_data$Dose <- as.numeric(meta_data$Dose)
meta_data$logrr <- as.numeric(meta_data$logrr)
meta_data$se <- as.numeric(meta_data$se)

# Fit the meta-analysis model
meta_analysis_linCV <- rma(yi = logrr, 
                           sei = se, 
                           mods = ~ Dose, 
                           data = meta_data, 
                           method = "REML")

# Print the results
print(meta_analysis_linCV)

# Calculate AIC and BIC
aic_value_linCV <- AIC(meta_analysis_linCV)
bic_value_linCV <- BIC(meta_analysis_linCV)

print (aic_value_linCV)
print (bic_value_linCV)

# Create the specified dose sequence
dose_seq <- seq(0, 20, by = 5)

# Create new data frame for predictions
new_data <- data.frame(Dose = dose_seq)

# Convert new_data to a matrix
new_mods <- as.matrix(new_data)


# Predict log relative risk (logrr) for the new Dose values
predictions_linCV <- predict(meta_analysis_linCV, newmods = new_mods)

# Convert log relative risk to hazard ratio
predictions_linCV_df <- data.frame(
  Dose = dose_seq,
  logrr = predictions_linCV$pred,
  ci.lb = predictions_linCV$ci.lb,
  ci.ub = predictions_linCV$ci.ub
)
predictions_linCV_df$hr <- exp(predictions_linCV_df$logrr)
predictions_linCV_df$hr.ci.lb <- exp(predictions_linCV_df$ci.lb)
predictions_linCV_df$hr.ci.ub <- exp(predictions_linCV_df$ci.ub)

# Print the predictions
print(predictions_linCV_df)


# Create the plot
ggplot(predictions_linCV_df, aes(x = Dose, y = hr)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_ribbon(aes(ymin = hr.ci.lb, ymax = hr.ci.ub), fill = "blue", alpha = 0.2) +
  geom_hline(yintercept = 1.10, linetype = "dotted", color = "red") +
  labs(title = "Dose-Response Meta-Analysis of Standard Deviation",
       x = "Standard Deviation",
       y = "Hazard Ratio",
       caption = "Shaded area: 95% Confidence Intervals") +
  theme_minimal()


target_logrr <- log(1.10)  # This is approximately 0.0953


# Data for interpolation
dose_seq <- c(0, 5, 10, 15, 20)
hr_values <- c(1.106051, 1.160960, 1.218596, 1.279093, 1.342593)

# Target HR
target_hr <- 1.10

# Linear interpolation to find the dose corresponding to HR = 1.10
interpolated_dose <- approx(x = hr_values, y = dose_seq, xout = target_hr)$y

# Print the interpolated dose
cat("Dose corresponding to HR = 1.10:", interpolated_dose, "\n")







