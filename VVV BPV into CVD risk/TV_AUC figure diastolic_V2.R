###### DIASTOLIC #####



library(tidyr)
library(dplyr)
library(ggplot2)

# 1. Create the data
bp_data <- data.frame(
  time = c(12, 24, 36, 48, 60),
  SBP_AUC = c(0.684, 0.696, 0.714, 0.742, 0.762),
  SBP_lower = c(0.561, 0.604, 0.643, 0.661, 0.707),
  SBP_upper = c(0.807, 0.787, 0.784, 0.824, 0.817),
  BPV_SD_AUC = c(0.747, 0.792, 0.797, 0.839, 0.843),
  BPV_SD_lower = c(0.679, 0.744, 0.756, 0.796, 0.810),
  BPV_SD_upper = c(0.815, 0.839, 0.839, 0.882, 0.876),
  BPV_CV_AUC = c(0.770, 0.802, 0.801, 0.845, 0.846),
  BPV_CV_lower = c(0.707, 0.755, 0.760, 0.800, 0.812),
  BPV_CV_upper = c(0.833, 0.850, 0.843, 0.889, 0.880),
  BPV_SDcom_AUC = c(0.748, 0.792, 0.792, 0.839, 0.843),
  BPV_SDcom_lower = c(0.678, 0.743, 0.755, 0.796, 0.809),
  BPV_SDcom_upper = c(0.818, 0.842, 0.840, 0.883, 0.877),
  BPV_CVcom_AUC = c(0.769, 0.802, 0.801, 0.843, 0.845),
  BPV_CVcom_lower = c(0.706, 0.754, 0.760, 0.798, 0.810),
  BPV_CVcom_upper = c(0.832, 0.850, 0.842, 0.888, 0.879)
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
      measure_raw == "SBP" ~ "Single DBP Measurement",
      measure_raw == "BPV_SD" ~ "Diastolic BPV (SD)",
      measure_raw == "BPV_CV" ~ "Diastolic BPV (CV)",
      measure_raw == "BPV_SDcom" ~ "Single DBP and Diastolic BPV (SD)",
      measure_raw == "BPV_CVcom" ~ "Single DBP and Diastolic BPV (CV)"
    ),
    # Set the order of the items in the legend
    measure = factor(measure, levels = c(
      "Single DBP Measurement", 
      "Diastolic BPV (SD)", 
      "Diastolic BPV (CV)",
      "Single DBP and Diastolic BPV (SD)", 
      "Single DBP and Diastolic BPV (CV)"
    ))
  )

# 3. Create the plot
auc_plot_D <- ggplot(bp_long, aes(x = time, y = AUC, color = measure)) +
  geom_line(linewidth = 1) + # 'size' is deprecated, 'linewidth' is preferred
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = measure), alpha = 0.1, color = NA) +
  scale_x_continuous(breaks = c(12, 24, 36, 48, 60)) +
  labs(
    title = "B. Diastolic Blood Pressure",
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
print(auc_plot_D)

# Save the plot as a high-resolution PNG
ggsave("AUC_comparison_plot_D_2.png", plot = auc_plot_D, width = 10, height = 6, dpi = 600)
