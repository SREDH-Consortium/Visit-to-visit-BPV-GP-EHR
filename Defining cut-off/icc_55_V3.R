library(dplyr)
library(tibble)
library(purrr)
library(irr)


# Convert wide to long: columns sd_S_3...sd_S_12 → rows
sd_long <- sd_combined_5 %>%
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

library(ggplot2)

ggplot(summary_stats, aes(x = n, y = mean_sd)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  geom_errorbar(aes(ymin = mean_sd - sd_sd, ymax = mean_sd + sd_sd),
                width = 0.2, color = "blue") +
  labs(
    title = "Trend of BPV (SD of BP) by Number of Measurements",
    x = "Number of Measurements (n)",
    y = "Mean ± SD of BP SD"
  ) +
  theme_minimal()



sd_S3_cvd <- cor.test(sd_combined_5$sd_S_3, sd_combined_5$sd_S_11, method = "pearson")
sd_S4_cvd <- cor.test(sd_combined_5$sd_S_4, sd_combined_5$sd_S_11, method = "pearson")
sd_S5_cvd <- cor.test(sd_combined_5$sd_S_5, sd_combined_5$sd_S_11, method = "pearson")
sd_S6_cvd <- cor.test(sd_combined_5$sd_S_6, sd_combined_5$sd_S_11, method = "pearson")
sd_S7_cvd <- cor.test(sd_combined_5$sd_S_7, sd_combined_5$sd_S_11, method = "pearson")
sd_S8_cvd <- cor.test(sd_combined_5$sd_S_8, sd_combined_5$sd_S_11, method = "pearson")
sd_S9_cvd <- cor.test(sd_combined_5$sd_S_9, sd_combined_5$sd_S_11, method = "pearson")
sd_S10_cvd <- cor.test(sd_combined_5$sd_S_10, sd_combined_5$sd_S_11, method = "pearson")
sd_S11_cvd <- cor.test(sd_combined_5$sd_S_11, sd_combined_5$sd_S_12, method = "pearson")


print(sd_S3_cvd)
print(sd_S4_cvd)
print(sd_S5_cvd)
print(sd_S6_cvd)
print(sd_S7_cvd)
print(sd_S8_cvd)
print(sd_S9_cvd)
print(sd_S10_cvd)
print(sd_S11_cvd)

# Create a data frame manually
cor_summary <- data.frame(
  n = 3:11,
  r = c(sd_S3$estimate, sd_S4$estimate, sd_S5$estimate, sd_S6$estimate,
        sd_S7$estimate, sd_S8$estimate, sd_S9$estimate, sd_S10$estimate, sd_S11$estimate),
  p_value = c(sd_S3$p.value, sd_S4$p.value, sd_S5$p.value, sd_S6$p.value,
              sd_S7$p.value, sd_S8$p.value, sd_S9$p.value, sd_S10$p.value, sd_S11$p.value),
  ci_lower = c(sd_S3$conf.int[1], sd_S4$conf.int[1], sd_S5$conf.int[1], sd_S6$conf.int[1],
               sd_S7$conf.int[1], sd_S8$conf.int[1], sd_S9$conf.int[1], sd_S10$conf.int[1], sd_S11$conf.int[1]),
  ci_upper = c(sd_S3$conf.int[2], sd_S4$conf.int[2], sd_S5$conf.int[2], sd_S6$conf.int[2],
               sd_S7$conf.int[2], sd_S8$conf.int[2], sd_S9$conf.int[2], sd_S10$conf.int[2], sd_S11$conf.int[2])
)

write.csv(cor_summary, "correlation_summary_3_to_11_vs_S12.csv", row.names = FALSE)


#For ICC you can use something like  
icc_sd_S3_cvd <- icc(data.frame(sd_combined_5$sd_S_3, sd_combined_5$sd_S_8), model = "twoway", type = "consistency", unit = "single") 
icc_sd_S4_cvd <- icc(data.frame(sd_combined_5$sd_S_4, sd_combined_5$sd_S_8), model = "twoway", type = "consistency", unit = "single") 
icc_sd_S5_cvd <- icc(data.frame(sd_combined_5$sd_S_5, sd_combined_5$sd_S_8), model = "twoway", type = "consistency", unit = "single") 
icc_sd_S6_cvd <- icc(data.frame(sd_combined_5$sd_S_6, sd_combined_5$sd_S_8), model = "twoway", type = "consistency", unit = "single") 
icc_sd_S7_cvd <- icc(data.frame(sd_combined_5$sd_S_7, sd_combined_5$sd_S_8), model = "twoway", type = "consistency", unit = "single") 
icc_sd_S8_cvd <- icc(data.frame(sd_combined_5$sd_S_8, sd_combined_5$sd_S_9), model = "twoway", type = "consistency", unit = "single") 
icc_sd_S9_cvd <- icc(data.frame(sd_combined_5$sd_S_9, sd_combined_5$sd_S_10), model = "twoway", type = "consistency", unit = "single") 
icc_sd_S10_cvd <- icc(data.frame(sd_combined_5$sd_S_10, sd_combined_5$sd_S_11), model = "twoway", type = "consistency", unit = "single")
icc_sd_S11_cvd <- icc(data.frame(sd_combined_5$sd_S_11, sd_combined_5$sd_S_12), model = "twoway", type = "consistency", unit = "single")

print(icc_sd_S3_cvd)
print(icc_sd_S4_cvd)
print(icc_sd_S5_cvd)
print(icc_sd_S6_cvd)
print(icc_sd_S7_cvd)
print(icc_sd_S8_cvd)
print(icc_sd_S9_cvd)
print(icc_sd_S10_cvd)
print(icc_sd_S11_cvd)

outcome_time_data <- BPV.55_3022 %>%
  select(Patient_UUID, outcome, time_ms) %>%
  distinct(Patient_UUID, .keep_all = TRUE)

icc_table_complete <- sd_combined %>%
  left_join (outcome_time_data, by = "Patient_UUID")

icc_table_complete_12 <- icc_table_complete %>%
  filter(sd_S_12 > 0)


########### CPH sd_S_3 ##############
# Step 1: Standardize the sd_S_3 variable
icc_table_complete$sd_S_3_std <- scale(icc_table_complete$sd_S_3)

# Step 2: Fit the Cox proportional hazards model
sd_S_3_std_CPH <- coxph(Surv(time_ms, outcome) ~ sd_S_3_std, data = icc_table_complete)

# Step 3: Display the results
summary(sd_S_3_std_CPH)

########### CPH sd_S_4 ##############
# Step 1: Standardize the sd_S_4 variable
icc_table_complete$sd_S_4_std <- scale(icc_table_complete$sd_S_4)

# Step 2: Fit the Cox proportional hazards model
sd_S_4_std_CPH <- coxph(Surv(time_ms, outcome) ~ sd_S_4_std, data = icc_table_complete)

# Step 3: Display the results
summary(sd_S_4_std_CPH)

########### CPH sd_S_5 ##############
# Step 1: Standardize the sd_S_5 variable
icc_table_complete$sd_S_5_std <- scale(icc_table_complete$sd_S_5)

# Step 2: Fit the Cox proportional hazards model
sd_S_5_std_CPH <- coxph(Surv(time_ms, outcome) ~ sd_S_5_std, data = icc_table_complete)

# Step 3: Display the results
summary(sd_S_5_std_CPH)

########### CPH sd_S_6 ##############
# Step 1: Standardize the sd_S_6 variable
icc_table_complete$sd_S_6_std <- scale(icc_table_complete$sd_S_6)

# Step 2: Fit the Cox proportional hazards model
sd_S_6_std_CPH <- coxph(Surv(time_ms, outcome) ~ sd_S_6_std, data = icc_table_complete)

# Step 3: Display the results
summary(sd_S_6_std_CPH)


########### CPH sd_S_7 ##############
# Step 1: Standardize the sd_S_7 variable
icc_table_complete$sd_S_7_std <- scale(icc_table_complete$sd_S_7)

# Step 2: Fit the Cox proportional hazards model
sd_S_7_std_CPH <- coxph(Surv(time_ms, outcome) ~ sd_S_7_std, data = icc_table_complete)

# Step 3: Display the results
summary(sd_S_7_std_CPH)


########### CPH sd_S_8 ##############
# Step 1: Standardize the sd_S_8 variable
icc_table_complete$sd_S_8_std <- scale(icc_table_complete$sd_S_8)

# Step 2: Fit the Cox proportional hazards model
sd_S_8_std_CPH <- coxph(Surv(time_ms, outcome) ~ sd_S_8_std, data = icc_table_complete)

# Step 3: Display the results
summary(sd_S_8_std_CPH)


########### CPH sd_S_9 ##############
# Step 1: Standardize the sd_S_9 variable
icc_table_complete$sd_S_9_std <- scale(icc_table_complete$sd_S_9)

# Step 2: Fit the Cox proportional hazards model
sd_S_9_std_CPH <- coxph(Surv(time_ms, outcome) ~ sd_S_9_std, data = icc_table_complete)

# Step 3: Display the results
summary(sd_S_9_std_CPH)


########### CPH sd_S_10 ##############
# Step 1: Standardize the sd_S_10 variable
icc_table_complete$sd_S_10_std <- scale(icc_table_complete$sd_S_10)

# Step 2: Fit the Cox proportional hazards model
sd_S_10_std_CPH <- coxph(Surv(time_ms, outcome) ~ sd_S_10_std, data = icc_table_complete)

# Step 3: Display the results
summary(sd_S_10_std_CPH)


########### CPH sd_S_11 ##############
# Step 1: Standardize the sd_S_11 variable
icc_table_complete$sd_S_11_std <- scale(icc_table_complete$sd_S_11)

# Step 2: Fit the Cox proportional hazards model
sd_S_11_std_CPH <- coxph(Surv(time_ms, outcome) ~ sd_S_11_std, data = icc_table_complete)

# Step 3: Display the results
summary(sd_S_11_std_CPH)


########### CPH sd_S_12 ##############
# Step 1: Standardize the sd_S_12 variable
icc_table_complete$sd_S_12_std <- scale(icc_table_complete$sd_S_12)

# Step 2: Fit the Cox proportional hazards model
sd_S_12_std_CPH <- coxph(Surv(time_ms, outcome) ~ sd_S_12_std, data = icc_table_complete)

# Step 3: Display the results
summary(sd_S_12_std_CPH)
