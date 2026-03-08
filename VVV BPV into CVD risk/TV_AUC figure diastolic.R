# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Create the data
bp_data <- data.frame(
  time = c(12, 24, 36, 48, 60),
  DBP_AUC = c(0.688, 0.646, 0.713, 0.728, 0.755),
  DBP_lower = c(0.514, 0.489, 0.621, 0.655, 0.665),
  DBP_upper = c(0.861, 0.802, 0.805, 0.800, 0.846),
  BPV_SD_AUC = c(0.753, 0.759, 0.820, 0.841, 0.869),
  BPV_SD_lower = c(0.665, 0.673, 0.774, 0.802, 0.830),
  BPV_SD_upper = c(0.841, 0.845, 0.867, 0.880, 0.908),
  BPV_CV_AUC = c(0.820, 0.801, 0.837, 0.851, 0.871),
  BPV_CV_lower = c(0.734, 0.718, 0.791, 0.810, 0.892),
  BPV_CV_upper = c(0.907, 0.884, 0.883, 0.892, 0.916)
)

# Reshape the data for plotting
bp_long <- bp_data %>%
  pivot_longer(
    cols = -time,
    names_to = c("measure", ".value"),
    names_pattern = "(DBP|BPV_SD|BPV_CV)_(AUC|lower|upper)"
  ) %>%
  mutate(measure = recode(measure,
                          DBP = "Single BP Measurement",
                          BPV_SD = "Diastolic BPV (SD)",
                          BPV_CV = "Diastolic BPV (CV)"),
         measure = factor(measure, levels = c("Single BP Measurement", "Diastolic BPV (SD)", "Diastolic BPV (CV)")))

# Create the plot
auc_plot <- ggplot(bp_long, aes(x = time, y = AUC, color = measure)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = measure), alpha = 0.2, color = NA) +
  scale_x_continuous(breaks = c(12, 24, 36, 48, 60)) +
  labs(
    title = "B. Diastolic Blood Pressure",
    x = "Time (Months)",
    y = "AUC Value",
    color = "Measure",
    fill = "Measure"
  ) +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")



# Save the plot as a high-resolution PNG
ggsave("AUC_comparison_plot_D.png", plot = auc_plot, width = 10, height = 6, dpi = 600)

# Define the data
auc_data <- data.frame(
  time = c(12, 24, 36, 48, 60),
  DBP_AUC = c(0.688, 0.646, 0.713, 0.728, 0.755),
  DBP_lower = c(0.514, 0.489, 0.621, 0.655, 0.665),
  DBP_upper = c(0.861, 0.802, 0.805, 0.800, 0.846),
  BPV_SD_AUC = c(0.753, 0.759, 0.820, 0.841, 0.869),
  BPV_SD_lower = c(0.665, 0.673, 0.774, 0.802, 0.830),
  BPV_SD_upper = c(0.841, 0.845, 0.867, 0.880, 0.908),
  BPV_CV_AUC = c(0.820, 0.801, 0.837, 0.851, 0.871),
  BPV_CV_lower = c(0.734, 0.718, 0.791, 0.810, 0.892),
  BPV_CV_upper = c(0.907, 0.884, 0.883, 0.892, 0.916)
)

# Function to calculate SE from CI
calc_se <- function(lower, upper) {
  (upper - lower) / (2 * 1.96)
}

# Compute P values
p_values <- auc_data %>%
  rowwise() %>%
  mutate(
    SE_DBP = calc_se(DBP_lower, DBP_upper),
    SE_SD = calc_se(BPV_SD_lower, BPV_SD_upper),
    SE_CV = calc_se(BPV_CV_lower, BPV_CV_upper),
    
    Z_SBP_SD = (DBP_AUC - BPV_SD_AUC) / sqrt(SE_DBP^2 + SE_SD^2),
    Z_SBP_CV = (DBP_AUC - BPV_CV_AUC) / sqrt(SE_DBP^2 + SE_CV^2),
    
    P_SBP_SD = 2 * (1 - pnorm(abs(Z_SBP_SD))),
    P_SBP_CV = 2 * (1 - pnorm(abs(Z_SBP_CV)))
  ) %>%
  select(time, P_SBP_SD, P_SBP_CV)

# View results
print(p_values)

