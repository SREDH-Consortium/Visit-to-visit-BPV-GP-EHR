# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Create the data
bp_data <- data.frame(
  time = c(12, 24, 36, 48, 60),
  SBP_AUC = c(0.699, 0.657, 0.679, 0.749, 0.765),
  SBP_lower = c(0.552, 0.544, 0.578, 0.632, 0.662),
  SBP_upper = c(0.847, 0.770, 0.779, 0.865, 0.868),
  BPV_SD_AUC = c(0.805, 0.794, 0.822, 0.858, 0.875),
  BPV_SD_lower = c(0.723, 0.734, 0.781, 0.782, 0.799),
  BPV_SD_upper = c(0.877, 0.861, 0.876, 0.875, 0.899),
  BPV_CV_AUC = c(0.773, 0.772, 0.826, 0.827, 0.849),
  BPV_CV_lower = c(0.671, 0.686, 0.781, 0.782, 0.799),
  BPV_CV_upper = c(0.876, 0.859, 0.872, 0.873, 0.899)
)

# Reshape the data for plotting
bp_long <- bp_data %>%
  pivot_longer(
    cols = -time,
    names_to = c("measure", ".value"),
    names_pattern = "(SBP|BPV_SD|BPV_CV)_(AUC|lower|upper)"
  ) %>%
  mutate(measure = recode(measure,
                          SBP = "Single BP Measurement",
                          BPV_SD = "Systolic BPV (SD)",
                          BPV_CV = "Systolic BPV (CV)"),
         measure = factor(measure, levels = c("Single BP Measurement", "Systolic BPV (SD)", "Systolic BPV (CV)")))

# Create the plot
auc_plot <- ggplot(bp_long, aes(x = time, y = AUC, color = measure)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = measure), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = c(12, 24, 36, 48, 60)) +
  labs(
    title = "A. Systolic Blood Pressure",
    x = "Time (Months)",
    y = "AUC Value",
    color = "Measure",
    fill = "Measure"
  ) +
  theme_minimal(base_size = 14)+
  theme(legend.position = "none")



# Save the plot as a high-resolution PNG
ggsave("AUC_comparison_plot_S.png", plot = auc_plot, width = 10, height = 6, dpi = 600)

# Define the data
auc_data <- data.frame(
  time = c(12, 24, 36, 48, 60),
  SBP_AUC = c(0.579, 0.641, 0.700, 0.720, 0.735),
  SBP_lower = c(0.510, 0.485, 0.606, 0.653, 0.675),
  SBP_upper = c(0.848, 0.796, 0.794, 0.786, 0.836),
  BPV_SD_AUC = c(0.775, 0.774, 0.828, 0.829, 0.849),
  BPV_SD_lower = c(0.674, 0.687, 0.781, 0.782, 0.799),
  BPV_SD_upper = c(0.877, 0.861, 0.876, 0.875, 0.899),
  BPV_CV_AUC = c(0.773, 0.772, 0.826, 0.827, 0.849),
  BPV_CV_lower = c(0.671, 0.686, 0.781, 0.782, 0.799),
  BPV_CV_upper = c(0.876, 0.859, 0.872, 0.873, 0.899)
)

# Function to calculate SE from CI
calc_se <- function(lower, upper) {
  (upper - lower) / (2 * 1.96)
}

# Compute P values
p_values <- auc_data %>%
  rowwise() %>%
  mutate(
    SE_SBP = calc_se(SBP_lower, SBP_upper),
    SE_SD = calc_se(BPV_SD_lower, BPV_SD_upper),
    SE_CV = calc_se(BPV_CV_lower, BPV_CV_upper),

    Z_SBP_SD = (SBP_AUC - BPV_SD_AUC) / sqrt(SE_SBP^2 + SE_SD^2),
    Z_SBP_CV = (SBP_AUC - BPV_CV_AUC) / sqrt(SE_SBP^2 + SE_CV^2),

    P_SBP_SD = 2 * (1 - pnorm(abs(Z_SBP_SD))),
    P_SBP_CV = 2 * (1 - pnorm(abs(Z_SBP_CV)))
  ) %>%
  select(time, P_SBP_SD, P_SBP_CV)

# View results
print(p_values)



################# TVB #####################

# Create the data
bp_data_tvb <- data.frame(
  time = c(12, 24, 36, 48, 60),
  SBP_TVB = c(0.002, 0.002, 0.006, 0.006, 0.007),
  SBP_lower = c(0.0018, 0.0018, 0.0058, 0.0058, 0.0068),  # Slightly lower
  SBP_upper = c(0.0022, 0.0022, 0.0062, 0.0062, 0.0072),  # Slightly upper
  BPV_SD_TVB = c(0.004, 0.004, 0.005, 0.005, 0.005),
  BPV_SD_lower = c(0.0038, 0.0038, 0.0048, 0.0048, 0.0048),
  BPV_SD_upper = c(0.0042, 0.0042, 0.0052, 0.0052, 0.0052),
  BPV_CV_TVB = c(0.002, 0.002, 0.006, 0.006, 0.007),
  BPV_CV_lower = c(0.0018, 0.0018, 0.0058, 0.0058, 0.0068),
  BPV_CV_upper = c(0.0022, 0.0022, 0.0062, 0.0062, 0.0072)
)

# Reshape the data for plotting
bp_long <- bp_data_tvb %>%
  pivot_longer(
    cols = -time,
    names_to = c("measure", ".value"),
    names_pattern = "(SBP|BPV_SD|BPV_CV)_(TVB|lower|upper)"
  ) %>%
  mutate(measure = recode(measure,
                          SBP = "Single BP Measurement",
                          BPV_SD = "Systolic BPV (SD)",
                          BPV_CV = "Systolic BPV (CV)"),
         measure = factor(measure, levels = c("Single BP Measurement", "Systolic BPV (SD)", "Systolic BPV (CV)")))

# Create the plot
tvb_plot <- ggplot(bp_long, aes(x = time, y = TVB, color = measure)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = measure), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = c(12, 24, 36, 48, 60)) +
  labs(
    title = "A. Systolic Blood Pressure",
    x = "Time (Months)",
    y = "Brier Score",
    color = "Measure",
    fill = "Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")


# Save the plot as a high-resolution PNG
ggsave("TVB_comparison_plot_S.png", plot = tvb_plot, width = 10, height = 6, dpi = 600)

# Define the data
auc_data <- data.frame(
  time = c(12, 24, 36, 48, 60),
  SBP_AUC = c(0., 0.641, 0.700, 0.720, 0.735),
  SBP_lower = c(0.510, 0.485, 0.606, 0.653, 0.675),
  SBP_upper = c(0.848, 0.796, 0.794, 0.786, 0.836),
  BPV_SD_AUC = c(0.775, 0.774, 0.828, 0.829, 0.849),
  BPV_SD_lower = c(0.674, 0.687, 0.781, 0.782, 0.799),
  BPV_SD_upper = c(0.877, 0.861, 0.876, 0.875, 0.899),
  BPV_CV_AUC = c(0.773, 0.772, 0.826, 0.827, 0.849),
  BPV_CV_lower = c(0.671, 0.686, 0.781, 0.782, 0.799),
  BPV_CV_upper = c(0.876, 0.859, 0.872, 0.873, 0.899)
)

# Function to calculate SE from CI
calc_se <- function(lower, upper) {
  (upper - lower) / (2 * 1.96)
}

# Compute P values
p_values <- auc_data %>%
  rowwise() %>%
  mutate(
    SE_SBP = calc_se(SBP_lower, SBP_upper),
    SE_SD = calc_se(BPV_SD_lower, BPV_SD_upper),
    SE_CV = calc_se(BPV_CV_lower, BPV_CV_upper),

    Z_SBP_SD = (SBP_AUC - BPV_SD_AUC) / sqrt(SE_SBP^2 + SE_SD^2),
    Z_SBP_CV = (SBP_AUC - BPV_CV_AUC) / sqrt(SE_SBP^2 + SE_CV^2),

    P_SBP_SD = 2 * (1 - pnorm(abs(Z_SBP_SD))),
    P_SBP_CV = 2 * (1 - pnorm(abs(Z_SBP_CV)))
  ) %>%
  select(time, P_SBP_SD, P_SBP_CV)

# View results
print(p_values)



