# Manually extract p-values from pooled summaries
p_vals <- c(
  sd_S_std_1 = summary(est_sd_S_std_1)$p.value[1],
  sd_D_std_1 = summary(est_sd_D_std_1)$p.value[1],
  cv_S_std_1 = summary(est_cv_S_std_1)$p.value[1],
  cv_D_std_1 = summary(est_cv_D_std_1)$p.value[1],
  sd_S_std_2 = summary(est_sd_S_std_2)$p.value[1],
  sd_D_std_2 = summary(est_sd_D_std_2)$p.value[1],
  cv_S_std_2 = summary(est_cv_S_std_2)$p.value[1],
  cv_D_std_2 = summary(est_cv_D_std_2)$p.value[1],
  sd_S_std_3 = summary(est_sd_S_std_3)$p.value[1],
  sd_D_std_3 = summary(est_sd_D_std_3)$p.value[1],
  cv_S_std_3 = summary(est_cv_S_std_3)$p.value[1],
  cv_D_std_3 = summary(est_cv_D_std_3)$p.value[1],
  sd_S_std_4 = summary(est_sd_S_std_4)$p.value[1],
  sd_D_std_4 = summary(est_sd_D_std_4)$p.value[1],
  cv_S_std_4 = summary(est_cv_S_std_4)$p.value[1],
  cv_D_std_4 = summary(est_cv_D_std_4)$p.value[1],
  sd_S_std_5 = summary(est_sd_S_std_5)$p.value[1],
  sd_D_std_5 = summary(est_sd_D_std_5)$p.value[1],
  cv_S_std_5 = summary(est_cv_S_std_5)$p.value[1],
  cv_D_std_5 = summary(est_cv_D_std_5)$p.value[1]
)

p_vals <- c(
  sd_S_std_5 = summary(est_sd_S_std_5)$p.value[1],
  sd_D_std_5 = summary(est_sd_D_std_5)$p.value[1],
  cv_S_std_5 = summary(est_cv_S_std_5)$p.value[1],
  cv_D_std_5 = summary(est_cv_D_std_5)$p.value[1]
)


p_adjusted <- p.adjust(p_vals, method = "BH")

results_table <- data.frame(
  model = names(p_vals),
  raw_p = p_vals,
  adjusted_p = p_adjusted
)

print(results_table)


write.csv(results_table, "BH_adjusted_pvalues_full.csv", row.names = FALSE)



summary(est_sd_S_std_1)
summary(est_sd_D_std_1)
summary(est_cv_S_std_1)
summary(est_cv_D_std_1)
summary(est_sd_S_std_2)
summary(est_sd_D_std_2)
summary(est_cv_S_std_2)
summary(est_cv_D_std_2)
summary(est_sd_S_std_3)
summary(est_sd_D_std_3)
summary(est_cv_S_std_3)
summary(est_cv_D_std_3)
summary(est_sd_S_std_4)
summary(est_sd_D_std_4)
summary(est_cv_S_std_4)
summary(est_cv_D_std_4)
summary(est_sd_S_std_5)
summary(est_sd_D_std_5)
summary(est_cv_S_std_5)
summary(est_cv_D_std_5)