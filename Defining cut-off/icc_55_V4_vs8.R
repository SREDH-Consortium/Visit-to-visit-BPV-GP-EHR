library(dplyr)
library(purrr)
library(tidyr)

filtered_BPV.55_3022 <- eligible_analysis_55_final %>%
  filter(Patient_UUID %in% BPV.55_3022$Patient_UUID)

# Step 1: Keep only systolic values and sort by date
systolic_data <- filtered_BPV.55_3022 %>%
  filter(!is.na(ObservationValue_S)) %>%
  arrange(Patient_UUID, ObservationDate)

sd_list <- map(3:12, function(n) {
  systolic_data %>%
    group_by(Patient_UUID) %>%
    summarise(
      !!paste0("sd_S_", n) := if (n() >= n) sd(head(ObservationValue_S, n)) else NA_real_,
      .groups = "drop"
    )
})

# Combine into single data frame
sd_df <- reduce(sd_list, left_join, by = "Patient_UUID")




sd_table <- sd_df

sd_S3 <- cor.test(sd_table$sd_S_3, sd_table$sd_S_8, method = "pearson")
sd_S4 <- cor.test(sd_table$sd_S_4, sd_table$sd_S_8, method = "pearson")
sd_S5 <- cor.test(sd_table$sd_S_5, sd_table$sd_S_8, method = "pearson")
sd_S6 <- cor.test(sd_table$sd_S_6, sd_table$sd_S_8, method = "pearson")
sd_S7 <- cor.test(sd_table$sd_S_7, sd_table$sd_S_8, method = "pearson")
sd_S8 <- cor.test(sd_table$sd_S_8, sd_table$sd_S_12, method = "pearson")
sd_S9 <- cor.test(sd_table$sd_S_9, sd_table$sd_S_12, method = "pearson")
sd_S10 <- cor.test(sd_table$sd_S_10, sd_table$sd_S_12, method = "pearson")
sd_S11 <- cor.test(sd_table$sd_S_11, sd_table$sd_S_12, method = "pearson")


print(sd_S3)
print(sd_S4)
print(sd_S5)
print(sd_S6)
print(sd_S7)
print(sd_S8)
print(sd_S9)
print(sd_S10)
print(sd_S11)

#For ICC you can use something like
icc_sd_S3 <- icc(data.frame(sd_table$sd_S_3, sd_table$sd_S_8), model = "twoway", type = "consistency", unit = "single")
icc_sd_S4 <- icc(data.frame(sd_table$sd_S_4, sd_table$sd_S_8), model = "twoway", type = "consistency", unit = "single")
icc_sd_S5 <- icc(data.frame(sd_table$sd_S_5, sd_table$sd_S_8), model = "twoway", type = "consistency", unit = "single")
icc_sd_S6 <- icc(data.frame(sd_table$sd_S_6, sd_table$sd_S_8), model = "twoway", type = "consistency", unit = "single")
icc_sd_S7 <- icc(data.frame(sd_table$sd_S_7, sd_table$sd_S_8), model = "twoway", type = "consistency", unit = "single")
icc_sd_S8 <- icc(data.frame(sd_table$sd_S_8, sd_table$sd_S_12), model = "twoway", type = "consistency", unit = "single")
icc_sd_S9 <- icc(data.frame(sd_table$sd_S_9, sd_table$sd_S_12), model = "twoway", type = "consistency", unit = "single")
icc_sd_S10 <- icc(data.frame(sd_table$sd_S_10, sd_table$sd_S_12), model = "twoway", type = "consistency", unit = "single")
icc_sd_S11 <- icc(data.frame(sd_table$sd_S_11, sd_table$sd_S_12), model = "twoway", type = "consistency", unit = "single")

print(icc_sd_S3)
print(icc_sd_S4)
print(icc_sd_S5)
print(icc_sd_S6)
print(icc_sd_S7)
print(icc_sd_S8)
print(icc_sd_S9)
print(icc_sd_S10)
print(icc_sd_S11)


# Convert wide to long: columns sd_S_3...sd_S_12 → rows
sd_long <- sd_table %>%
  pivot_longer(
    cols = starts_with("sd_S_"),
    names_to = "n",
    names_prefix = "sd_S_",
    values_to = "sd_value"
  ) %>%
  mutate(n = as.integer(n))  # convert n to numeric



summary_stats <- sd_long %>%
  group_by(n) %>%
  summarise(
    mean_sd = mean(sd_value, na.rm = TRUE),
    sd_sd = sd(sd_value, na.rm = TRUE),
    .groups = "drop"
  )


library(dplyr)
library(irr)      # for icc()
library(broom)    # for tidying cor.test output

# helper function to extract ICC + CI
get_icc <- function(x, y) {
  out <- icc(data.frame(x, y), model = "twoway", type = "consistency", unit = "single")
  tibble(
    estimate = out$value,
    lower = out$lbound,
    upper = out$ubound
  )
}

# helper function to extract correlation + CI
get_cor <- function(x, y) {
  out <- cor.test(x, y, method = "pearson")
  tibble(
    estimate = out$estimate,
    lower = out$conf.int[1],
    upper = out$conf.int[2]
  )
}

# build results for n = 3:7
results_ICC <- bind_rows(
  tibble(
    n = 3,
    metric = "ICC",
    get_icc(sd_table$sd_S_3, sd_table$sd_S_8)
  ),
  tibble(
    n = 4,
    metric = "ICC",
    get_icc(sd_table$sd_S_4, sd_table$sd_S_8)
  ),
  tibble(
    n = 5,
    metric = "ICC",
    get_icc(sd_table$sd_S_5, sd_table$sd_S_8)
  ),
  tibble(
    n = 6,
    metric = "ICC",
    get_icc(sd_table$sd_S_6, sd_table$sd_S_8)
  ),
  tibble(
    n = 7,
    metric = "ICC",
    get_icc(sd_table$sd_S_7, sd_table$sd_S_8)
  ),
  tibble(
    n = 3,
    metric = "Correlation",
    get_cor(sd_table$sd_S_3, sd_table$sd_S_8)
  ),
  tibble(
    n = 4,
    metric = "Correlation",
    get_cor(sd_table$sd_S_4, sd_table$sd_S_8)
  ),
  tibble(
    n = 5,
    metric = "Correlation",
    get_cor(sd_table$sd_S_5, sd_table$sd_S_8)
  ),
  tibble(
    n = 6,
    metric = "Correlation",
    get_cor(sd_table$sd_S_6, sd_table$sd_S_8)
  ),
  tibble(
    n = 7,
    metric = "Correlation",
    get_cor(sd_table$sd_S_7, sd_table$sd_S_8)
  )
)


library(ggplot2)


p <- ggplot(results_ICC, aes(x = n, y = estimate, color = metric, group = metric, fill = metric)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA) +
  geom_line(size = 1.2) +
  geom_point(size = 3, shape = 21, fill = "white") +
  scale_color_manual(values = c("ICC" = "steelblue", "Correlation" = "darkorange")) +
  scale_fill_manual(values = c("ICC" = "steelblue", "Correlation" = "darkorange")) +
  scale_x_continuous(breaks = 3:7) +
  labs(
    title = "Agreement of Standard Deviation of\nSystolic Blood Pressure with 8 Measurements",
    subtitle = "ICC and Pearson Correlation with 95% CI",
    x = "Number of Measurements (n)",
    y = "Agreement Statistic",
    color = "Metric",
    fill = "Metric"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, color = "grey30"),
    legend.position = "top"
  )

# Save high-resolution TIFF
ggsave(
  filename = "agreement_plot.tiff",
  plot = p,
  width = 7, height = 6, units = "in",   # Adjust size for journal specs
  dpi = 600,                             # 600 dpi = publication quality
  device = "tiff", compression = "lzw"   # LZW compression
)

