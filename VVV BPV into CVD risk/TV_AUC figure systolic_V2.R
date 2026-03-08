#######SYSTOLIC#######


library(tidyr)
library(dplyr)
library(ggplot2)

# 1. Create the data
bp_data <- data.frame(
  time = c(12, 24, 36, 48, 60),
  SBP_AUC = c(0.681, 0.691, 0.705, 0.736, 0.757),
  SBP_lower = c(0.557, 0.601, 0.630, 0.653, 0.700),
  SBP_upper = c(0.804, 0.782, 0.780, 0.819, 0.815),
  BPV_SD_AUC = c(0.784, 0.821, 0.807, 0.848, 0.852),
  BPV_SD_lower = c(0.723, 0.781, 0.769, 0.805, 0.820),
  BPV_SD_upper = c(0.846, 0.861, 0.848, 0.890, 0.885),
  BPV_CV_AUC = c(0.838, 0.843, 0.831, 0.857, 0.856),
  BPV_CV_lower = c(0.788, 0.807, 0.794, 0.817, 0.824),
  BPV_CV_upper = c(0.885, 0.879, 0.867, 0.897, 0.888),
  BPV_SDcom_AUC = c(0.783, 0.819, 0.807, 0.846, 0.850),
  BPV_SDcom_lower = c(0.721, 0.778, 0.766, 0.802, 0.816),
  BPV_SDcom_upper = c(0.846, 0.861, 0.848, 0.889, 0.884),
  BPV_CVcom_AUC = c(0.836, 0.841, 0.828, 0.854, 0.853),
  BPV_CVcom_lower = c(0.785, 0.802, 0.790, 0.812, 0.820),
  BPV_CVcom_upper = c(0.886, 0.879, 0.866, 0.896, 0.887)
)

# 2. Reshape the data
bp_long <- bp_data %>%
  pivot_longer(
    cols = -time,
    names_to = c("measure_raw", ".value"),
    # This regex captures everything before the suffix (_AUC, _lower, etc.)
    names_pattern = "^(.*)_(AUC|lower|upper)$" 
  ) %>%
  mutate(
    measure = case_when(
      measure_raw == "SBP" ~ "Single SBP Measurement",
      measure_raw == "BPV_SD" ~ "Systolic BPV (SD)",
      measure_raw == "BPV_CV" ~ "Systolic BPV (CV)",
      measure_raw == "BPV_SDcom" ~ "Single SBP and Systolic BPV (SD)",
      measure_raw == "BPV_CVcom" ~ "Single SBP and Systolic BPV (CV)"
    ),
    # Set the order of the items in the legend
    measure = factor(measure, levels = c(
      "Single SBP Measurement", 
      "Systolic BPV (SD)", 
      "Systolic BPV (CV)",
      "Single SBP and Systolic BPV (SD)", 
      "Single SBP and Systolic BPV (CV)"
    ))
  )

# 3. Create the plot
auc_plot_S <- ggplot(bp_long, aes(x = time, y = AUC, color = measure)) +
  geom_line(linewidth = 1) + # 'size' is deprecated, 'linewidth' is preferred
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = measure), alpha = 0.1, color = NA) +
  scale_x_continuous(breaks = c(12, 24, 36, 48, 60)) +
  labs(
    title = "A. Systolic Blood Pressure",
    x = "Time (Months)",
    y = "AUC Value",
    color = NULL, # Removes the title above the legend keys
    fill = NULL
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "bottom", # Moved legend to bottom so you can see the names
    legend.direction = "vertical" # Stack legend items vertically
  )

# Display the plot
print(auc_plot_S)

# Save the plot as a high-resolution PNG
ggsave("AUC_comparison_plot_S_2.png", plot = auc_plot_S, width = 10, height = 6, dpi = 600)
