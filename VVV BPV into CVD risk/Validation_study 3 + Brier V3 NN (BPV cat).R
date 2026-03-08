rm(list=ls())

install.packages("glmnet")
install.packages("Matrix")
install.packages("mice")
install.packages("survcomp")

install.packages("https://cran.r-project.org/src/contrib/Archive/Matrix/Matrix_1.5-4.tar.gz",
                 repos = NULL, type = "source")
install.packages("lme4", type = "source")

install.packages("BiocManager")
BiocManager::install("survcomp")


library(mice)
library(dplyr)
library(survival)
library(pec)
library(survcomp)
library(timeROC)
library(dplyr)
library(mice)
library (riskRegression)

setwd("C:/Users/nicks/OneDrive - UNSW/Documents/Consults/Mifetika Lukitasari")

imputed_S3 = readRDS("imputed_S3_V1.rds")




imputed_55_CVD_complete <- readRDS("imputed_55_CVD_complete_V2.rds")

# Extract all imputations into a long format data frame
long_data <- complete(imputed_55_CVD_complete, action = "long", include = TRUE)

# Add your new variable
long_data <- long_data %>%
  mutate(
    sd_S_cat   = ifelse(sd_S  >= 19, 1, 0),
    sd_D_cat   = ifelse(sd_D  >= 11, 1, 0),
    cv_S_cat   = ifelse(cv_S  >= 14, 1, 0),
    cv_D_cat   = ifelse(cv_D  >= 12, 1, 0)
  )


# Recreate the mids object
imputed_55_CVD_complete <- as.mids(long_data)

saveRDS(imputed_55_CVD_complete, file = "imputed_55_CVD_complete_BPVcat.rds")


imputed = imputed_55_CVD_complete

# --- STEP 1: Train/Test split ---
train_fraction <- 0.7
n <- nrow(complete(imputed, 1))
train_index <- sample(1:n, size = floor(train_fraction * n))

imputed_list <- lapply(1:imputed$m, function(i) {
  data <- complete(imputed, i)
  data$dataset <- ifelse(1:nrow(data) %in% train_index, "train", "test")
  data
})

for (i in 1:length(imputed_list)) {
  df <- imputed_list[[i]]

  train_data <- df[df$dataset == "train", ]
  test_data  <- df[df$dataset == "test", ]

  # save as CSV
  write.csv(train_data, paste0("train_data_imp", i, ".csv"), row.names = FALSE)
  write.csv(test_data, paste0("test_data_imp", i, ".csv"), row.names = FALSE)
}



# --- STEP 2: Fit Cox models on train data ---
cox_models <- lapply(imputed_list, function(df) {
  train_data <- df %>% filter(dataset == "train")
  coxph(Surv(time_ms, outcome) ~ Gender + SmokingStatus + tr_fglucose + tr_bmi + Value_TCHDL +
          egfr + FH + com_ht + com_dm + ObservationValue_S + ObservationValue_D, data = train_data, x = TRUE, y = TRUE)
})


### save as RDS #####
#saveRDS(train_data, paste0("train_data_imp", i, ".rds"))
#saveRDS(test_data, paste0("test_data_imp", i, ".rds"))


# Helper: function to get metrics for a dataset
compute_metrics <- function(model, data, times_auc = c(12, 24, 36, 48, 60)) {
  lp <- predict(model, newdata = data, type = "lp")

  # Harrell's C-index
  harrell_c <- concordance.index(lp, surv.time = data$time_ms, surv.event = data$outcome)$c.index

  # Time-varying Brier score + Integrated Brier Score
  pec_result <- pec(
    object = list("Cox" = model),
    formula = Surv(time_ms, outcome) ~ 1,
    data = data,
    times = times_auc,
    exact = TRUE,
    cens.model = "cox"
  )

  time_brier <- pec_result$AppErr$Cox  # vector of Brier scores at each time
  int_brier <- crps(pec_result)  # Integrated Brier Score

  # Calibration slope
  cal_slope <- coef(coxph(Surv(time_ms, outcome) ~ lp, data = data))

  # Time-varying AUC
  tv_auc <- timeROC(
    T = data$time_ms,
    delta = data$outcome,
    marker = lp,
    cause = 1,
    times = times_auc,
    iid = TRUE
  )$AUC

  list(
    Harrell_C = harrell_c,
    Brier = int_brier,
    Brier_Times = time_brier,
    CalSlope = cal_slope,
    TV_AUC = tv_auc
  )
}


# --- STEP 3: Compute metrics for train and test sets ---
train_metrics <- lapply(1:length(imputed_list), function(i) {
  df <- imputed_list[[i]] %>% filter(dataset == "train")
  compute_metrics(cox_models[[i]], df)
})

test_metrics <- lapply(1:length(imputed_list), function(i) {
  df <- imputed_list[[i]] %>% filter(dataset == "test")
  compute_metrics(cox_models[[i]], df)
})

# --- STEP 4: Pooling function ---
pool_metric <- function(values) {
  m <- length(values)
  qbar <- mean(values)
  ubar <- mean((values - qbar)^2) # between-imputation variance
  t <- ubar + (1 + 1/m) * var(values)
  se <- sqrt(t)
  list(mean = qbar, se = se, ci = c(qbar - 1.96*se, qbar + 1.96*se))
}

# Helper: pool all scalar metrics
pool_all <- function(metric_list, field) sapply(metric_list, function(x) x[[field]])

# --- STEP 5: Pool metrics ---
# Scalars
pooled_train <- list(
  Harrell_C = pool_metric(pool_all(train_metrics, "Harrell_C")),
  Brier = pool_metric(pool_all(train_metrics, "Brier")),
  CalSlope = pool_metric(pool_all(train_metrics, "CalSlope"))
)

pooled_test <- list(
  Harrell_C = pool_metric(pool_all(test_metrics, "Harrell_C")),
  Brier = pool_metric(pool_all(test_metrics, "Brier")),
  CalSlope = pool_metric(pool_all(test_metrics, "CalSlope"))
)

# Time-varying AUC pooling (each time point separately)
pool_tv_auc <- function(metric_list) {
  auc_mat <- do.call(rbind, lapply(metric_list, function(x) x$TV_AUC))
  lapply(1:ncol(auc_mat), function(j) pool_metric(auc_mat[, j]))
}

pooled_train$TV_AUC <- pool_tv_auc(train_metrics)
pooled_test$TV_AUC <- pool_tv_auc(test_metrics)

# Time-varying Brier score pooling (each time point separately)
pool_tvb <- function(metric_list) {
  tvb_mat <- do.call(rbind, lapply(metric_list, function(x) x$Brier_Times))
  lapply(1:ncol(tvb_mat), function(j) pool_metric(tvb_mat[, j]))
}

pooled_train$tvb <- pool_tvb(train_metrics)
pooled_test$tvb <- pool_tvb(test_metrics)

# --- STEP 6: Final results ---
results <- list(
  Train = pooled_train,
  Test = pooled_test
)

results

saveRDS(results, file = "results_SD.rds")


# --- Extract pooled TV_AUC values 12,24,36,48,60 ---

# Manually define the times you pooled for
time_points_auc <- c(12, 24, 36, 48, 60)

# Build dataframe of pooled TV_AUC
tvauc_df <- data.frame(
  time  = time_points_auc,
  mean  = sapply(pooled_test$TV_AUC, function(x) x$mean),
  lower = sapply(pooled_test$TV_AUC, function(x) x$ci[1]),
  upper = sapply(pooled_test$TV_AUC, function(x) x$ci[2])
)

print(tvauc_df)
tvb_df = matrix(nrow = 5, ncol = 4)

tvb_df[,1] = c(12, 24, 36, 48, 60)

for (i in 1:5) {
  tvb_df[i,2] = pooled_test$tvb[[i+1]]$mean
  tvb_df[i,3] = pooled_test$tvb[[i+1]]$ci[1]
  tvb_df[i,4] = pooled_test$tvb[[i+1]]$ci[2]
}

tvb_df = data.frame(tvb_df)
colnames(tvb_df) = c('time','mean','lower','upper')

print (tvb_df)

 library(ggplot2)

ggplot(tvauc_df, aes(x = time, y = mean)) +
  # Confidence ribbon
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "firebrick", alpha = 0.2) +
  # Line + points
  geom_line(color = "firebrick", linewidth = 1.2) +
  geom_point(size = 3, color = "white", fill = "firebrick", shape = 21, stroke = 1) +
  # Labels and theme
  labs(
    title = "Time-varying AUC",
    x = "Time (months)",
    y = "AUC",
    caption = "Error ribbon = 95% CI"
  ) +
  # Custom axis breaks
  scale_x_continuous(breaks = c(12, 24, 36, 48, 60)) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "grey90"),
    plot.caption = element_text(hjust = 1, face = "italic", size = 10)
  )


ggplot(tvb_df, aes(x = time, y = mean)) +
  # Confidence ribbon
  geom_ribbon(aes(ymin = lower, ymax = upper),
              fill = "firebrick", alpha = 0.2) +
  # Line + points
  geom_line(color = "firebrick", linewidth = 1.2) +
  geom_point(size = 3, color = "white", fill = "firebrick", shape = 21, stroke = 1) +
  # Labels and theme
  labs(
    title = "Time-varying Brier Score (Test set)",
    x = "Time (months)",
    y = "Brier Score",
    caption = "Error ribbon = 95% CI"
  ) +
  # Custom axis breaks to zoom into small values
  scale_x_continuous(breaks = c(12, 24, 36, 48, 60)) +
  scale_y_continuous(limits = c(0.0021, 0.0035),
                     breaks = seq(0.0021, 0.0035, 0.0002),
                     labels = scales::scientific_format(digits = 2)) +
  theme_minimal(base_size = 16) +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5, size = 18),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 12),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.major.y = element_line(color = "grey90"),
    plot.caption = element_text(hjust = 1, face = "italic", size = 10)
  )


library(ggplot2)
library(splines)

# Create spline interpolation
spline_df <- data.frame(
  time = seq(min(tvb_df$time), max(tvb_df$time), length.out = 200)
)
spline_df$mean <- spline(tvb_df$time, tvb_df$mean, xout = spline_df$time)$y

ggplot() +
  geom_ribbon(data = tvb_df, aes(x = time, ymin = lower, ymax = upper),
              fill = "firebrick", alpha = 0.2) +
  geom_line(data = spline_df, aes(x = time, y = mean),
            color = "darkred", linewidth = 1.2) +
  geom_point(data = tvb_df, aes(x = time, y = mean),
             size = 3, color = "white", fill = "firebrick", shape = 21, stroke = 1) +
  labs(
    title = "Time-varying Brier Score (Test set)",
    x = "Time (months)",
    y = "Brier Score",
    caption = "Error ribbon = 95% CI"
  ) +
  scale_x_continuous(breaks = c(12, 24, 36, 48, 60)) +
  scale_y_continuous(limits = c(0.002, 0.0035),
                     breaks = seq(0.002, 0.0035, 0.0002),
                     labels = scales::scientific_format(digits = 2)) +
  theme_minimal(base_size = 16)

