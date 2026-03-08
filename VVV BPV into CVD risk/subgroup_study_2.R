library(dplyr)
library(purrr)
library(writexl)
library(forestplot)
library(dplyr)
library(grid)

BPV_data <- BPV.55_3022




run_subgroup_models <- function(var) {
  results <- list()
  BPV_data[[var]] <- factor(BPV_data[[var]], levels = c("yes", "no"))  # Ensure subgroup is a factor
  
  for (level in levels(BPV_data[[var]])) {
    subset_data <- BPV_data %>% filter(.data[[var]] == level)
    formula <- as.formula("Surv(time_ms, outcome) ~ sd_S_std")
    model <- coxph(formula, data = subset_data)
    summary_model <- summary(model)
    
    hr <- summary_model$coefficients[1, "exp(coef)"]
    lower <- summary_model$conf.int[1, "lower .95"]
    upper <- summary_model$conf.int[1, "upper .95"]
    pval <- summary_model$coefficients[1, "Pr(>|z|)"]
    
    results[[level]] <- data.frame(
      Subgroup = var,
      Category = level,
      HR = hr,
      Lower = lower,
      Upper = upper,
      p.value = pval
    )
  }
  do.call(rbind, results)
}

# Run for all subgroups
subgroup_results <- lapply(subgroups, run_subgroup_models)
subgroup_df <- do.call(rbind, subgroup_results)

subgroup_df <- subgroup_df %>%
  arrange(Subgroup, Category)


run_interaction_pval <- function(var) {
  tryCatch({
    BPV_data[[var]] <- as.factor(BPV_data[[var]])  # Ensure it's a factor
    formula <- as.formula(paste("Surv(time_ms, outcome) ~ sd_S_std *", var))
    model <- coxph(formula, data = BPV_data)
    sm <- summary(model)
    
    interaction_rows <- grep(":", rownames(sm$coefficients), value = TRUE)
    
    if (length(interaction_rows) > 0) {
      pvals <- sm$coefficients[interaction_rows, "Pr(>|z|)"]
      data.frame(Subgroup = var, Interaction_p = min(pvals, na.rm = TRUE))
    } else {
      data.frame(Subgroup = var, Interaction_p = NA)
    }
  }, error = function(e) {
    data.frame(Subgroup = var, Interaction_p = NA)
  })
}

interaction_df <- do.call(rbind, lapply(subgroups, run_interaction_pval))


subgroup_df <- subgroup_df %>%
  left_join(interaction_df, by = "Subgroup")

write.csv (subgroup_df, "subgroup_study_2.csv", row.names = FALSE)
write_xlsx(subgroup_df, "subgroup_study_2.xlsx")



# Prepare dataset
subgroup_study_2 <- subgroup_study_2_V2 %>%
  mutate(
    HR_CI = sprintf("%.2f (%.2f–%.2f)", HR, Lower, Upper),
    p_int = formatC(`p value for interaction`, format = "f", digits = 3)
  )

# Build label matrix
label_matrix <- list()
mean_vals  <- c()
lower_vals <- c()
upper_vals <- c()

# Add top column headers (bold)
label_matrix[[1]] <- c("Subgroup", "N", "HR (95% CI)", "p-value for interaction")
mean_vals[1]  <- NA
lower_vals[1] <- NA
upper_vals[1] <- NA

# Loop through each subgroup
subgroups <- unique(subgroup_study_2$Subgroup)

for (sg in subgroups) {
  sub_data <- filter(subgroup_study_2, Subgroup == sg)
  
  # Add header row with Subgroup and p_int
  label_matrix <- append(label_matrix, list(c(sg, "", "", sub_data$p_int[1])))
  mean_vals  <- c(mean_vals, NA)
  lower_vals <- c(lower_vals, NA)
  upper_vals <- c(upper_vals, NA)
  
  # Add category rows
  for (i in 1:nrow(sub_data)) {
    label_matrix <- append(label_matrix, list(c(sub_data$Category[i],
                                                as.character(sub_data$N[i]),
                                                sub_data$HR_CI[i],
                                                "")))
    mean_vals  <- c(mean_vals, sub_data$HR[i])
    lower_vals <- c(lower_vals, sub_data$Lower[i])
    upper_vals <- c(upper_vals, sub_data$Upper[i])
  }
}

# Convert list to matrix
label_matrix <- do.call(rbind, label_matrix)

# Create high-resolution TIFF
tiff("subgroup_forestplot_final_V2.tiff", width = 8.5, height = 6, units = "in", res = 600)

forestplot(
  labeltext = label_matrix,
  mean  = mean_vals,
  lower = lower_vals,
  upper = upper_vals,
  zero = 1,
  boxsize = 0.2,
  col = fpColors(box = "royalblue", line = "darkblue"),
  xlab = "Hazard Ratio (95% CI)",
  align = c("l", "c", "c", "c"),
  colgap = unit(8, "mm"),
  graphwidth = unit(55, "mm"),
  is.summary = c(TRUE, rep(FALSE, nrow(label_matrix) - 1)),  # make first row bold
  txt_gp = fpTxtGp(
    label = gpar(cex = 0.8),
    ticks = gpar(cex = 0.8),
    xlab  = gpar(cex = 0.9),
    title = gpar(cex = 1.0),
    summary = gpar(fontface = "bold")  # bold first row
  )
)

dev.off()
