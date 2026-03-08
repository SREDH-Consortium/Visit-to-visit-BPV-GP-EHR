###### SYSTOLIC and DIASTOLIC #####



library(tidyr)
library(dplyr)
library(ggplot2)

# 1. Create the data
bp_data <- data.frame(
  time = c(12, 24, 36, 48, 60),
  SBP_AUC = c(0.682, 0.693, 0.704, 0.736, 0.757),
  SBP_lower = c(0.558, 0.601, 0.630, 0.653, 0.700),
  SBP_upper = c(0.806, 0.784, 0.777, 0.818, 0.814),
  BPV_SD_AUC = c(0.773, 0.814, 0.809, 0.850, 0.854),
  BPV_SD_lower = c(0.711, 0.772, 0.769, 0.809, 0.821),
  BPV_SD_upper = c(0.836, 0.854, 0.849, 0.891, 0.886),
  BPV_CV_AUC = c(0.809, 0.829, 0.821, 0.857, 0.857),
  BPV_CV_lower = c(0.756, 0.788, 0.783, 0.818, 0.825),
  BPV_CV_upper = c(0.862, 0.869, 0.858, 0.897, 0.887),
  BPV_SDcom_AUC = c(0.773, 0.813, 0.807, 0.848, 0.852),
  BPV_SDcom_lower = c(0.707, 0.770, 0.765, 0.806, 0.818),
  BPV_SDcom_upper = c(0.838, 0.856, 0.849, 0.891, 0.887),
  BPV_CVcom_AUC = c(0.808, 0.828, 0.818, 0.855, 0.855),
  BPV_CVcom_lower = c(0.753, 0.786, 0.779, 0.813, 0.821),
  BPV_CVcom_upper = c(0.863, 0.870, 0.858, 0.896, 0.888)
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
      measure_raw == "SBP" ~ "Single SBP and DBP Measurement",
      measure_raw == "BPV_SD" ~ "Systolic and Diastolic BPV (SD)",
      measure_raw == "BPV_CV" ~ "Systolic and Diastolic BPV (CV)",
      measure_raw == "BPV_SDcom" ~ "Single SBP and DBP + Systolic and Diastolic BPV (SD)",
      measure_raw == "BPV_CVcom" ~ "Single SBP and DBP and Systolic and Diastolic BPV (CV)"
    ),
    # Set the order of the items in the legend
    measure = factor(measure, levels = c(
      "Single SBP and DBP Measurement", 
      "Systolic and Diastolic BPV (SD)", 
      "Systolic and Diastolic BPV (CV)",
      "Single SBP and DBP + Systolic and Diastolic BPV (SD)", 
      "Single SBP and DBP and Systolic and Diastolic BPV (CV)"
    ))
  )

# 3. Create the plot
auc_plot_SD <- ggplot(bp_long, aes(x = time, y = AUC, color = measure)) +
  geom_line(linewidth = 1) + # 'size' is deprecated, 'linewidth' is preferred
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = measure), alpha = 0.1, color = NA) +
  scale_x_continuous(breaks = c(12, 24, 36, 48, 60)) +
  labs(
    title = "C. Systolic and Diastolic Blood Pressure",
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
print(auc_plot_SD)

# Save the plot as a high-resolution PNG
ggsave("AUC_comparison_plot_SD_2.png", plot = auc_plot_SD, width = 10, height = 6, dpi = 600)
