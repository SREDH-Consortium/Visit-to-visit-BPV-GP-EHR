# Load required packages
library(pROC)
library(dplyr)
library(MLmetrics)
library(caret)  # Needed for createDataPartition


# Ensure reproducibility
set.seed(123)

# Split data
train_index <- createDataPartition(BPV_55$outcome, p = 0.75, list = FALSE)
train_data <- BPV_55[train_index, ]
test_data  <- BPV_55[-train_index, ]

######## Systolic SD ########
model_sd_S <- glm(outcome ~ sd_S, data = train_data, family = "binomial")
test_data_sd_S <- test_data %>%
  mutate(pred_prob = predict(model_sd_S, newdata = ., type = "response"))

roc_curve_sd_S <- roc(test_data_sd_S$outcome, test_data_sd_S$sd_S)
opt_thresh_sd_S <- coords(roc_curve_sd_S, "best", best.method = "youden", transpose = FALSE)
opt_thresh_val_sd_S <- as.numeric(opt_thresh_sd_S["threshold"])

cat("Optimal cutoff for sd_S is:", opt_thresh_val_sd_S, "\n")

test_data_sd_S <- test_data_sd_S %>%
  mutate(pred_class = if_else(sd_S >= opt_thresh_val_sd_S, 1, 0))

conf_matrix_sd_S <- table(Observed = test_data_sd_S$outcome, Predicted = test_data_sd_S$pred_class)
print(conf_matrix_sd_S)

auc_val_sd_S <- auc(roc_curve_sd_S)
cat("AUC for sd_S model is:", auc_val_sd_S, "\n")

# Accuracy & F1
acc_sd_S <- mean(test_data_sd_S$pred_class == test_data_sd_S$outcome)
f1_sd_S <- F1_Score(as.factor(test_data_sd_S$pred_class), as.factor(test_data_sd_S$outcome), positive = "1")
cat("Accuracy (sd_S):", acc_sd_S, "\n")
cat("F1 Score (sd_S):", f1_sd_S, "\n")



######## Diastolic SD ########
model_sd_D <- glm(outcome ~ sd_D, data = train_data, family = "binomial")
test_data_sd_D <- test_data %>%
  mutate(pred_prob = predict(model_sd_D, newdata = ., type = "response"))

roc_curve_sd_D <- roc(test_data_sd_D$outcome, test_data_sd_D$sd_D)
opt_thresh_sd_D <- coords(roc_curve_sd_D, "best", best.method = "youden", transpose = FALSE)
opt_thresh_val_sd_D <- as.numeric(opt_thresh_sd_D["threshold"])

cat("Optimal cutoff for sd_D is:", opt_thresh_val_sd_D, "\n")

test_data_sd_D <- test_data_sd_D %>%
  mutate(pred_class = if_else(sd_D >= opt_thresh_val_sd_D, 1, 0))

conf_matrix_sd_D <- table(Observed = test_data_sd_D$outcome, Predicted = test_data_sd_D$pred_class)
print(conf_matrix_sd_D)

auc_val_sd_D <- auc(roc_curve_sd_D)
cat("AUC for sd_D model is:", auc_val_sd_D, "\n")

# Accuracy & F1
acc_sd_D <- mean(test_data_sd_D$pred_class == test_data_sd_D$outcome)
f1_sd_D <- F1_Score(as.factor(test_data_sd_D$pred_class), as.factor(test_data_sd_D$outcome), positive = "1")
cat("Accuracy (sd_D):", acc_sd_D, "\n")
cat("F1 Score (sd_D):", f1_sd_D, "\n")

######## Systolic CV ########
model_cv_S <- glm(outcome ~ cv_S, data = train_data, family = "binomial")
test_data_cv_S <- test_data %>%
  mutate(pred_prob = predict(model_cv_S, newdata = ., type = "response"))

roc_curve_cv_S <- roc(test_data_cv_S$outcome, test_data_cv_S$cv_S)
opt_thresh_cv_S <- coords(roc_curve_cv_S, "best", best.method = "youden", transpose = FALSE)
opt_thresh_val_cv_S <- as.numeric(opt_thresh_cv_S["threshold"])

cat("Optimal cutoff for cv_S is:", opt_thresh_val_cv_S, "\n")

test_data_cv_S <- test_data_cv_S %>%
  mutate(pred_class = if_else(cv_S >= opt_thresh_val_cv_S, 1, 0))

conf_matrix_cv_S <- table(Observed = test_data_cv_S$outcome, Predicted = test_data_cv_S$pred_class)
print(conf_matrix_cv_S)

auc_val_cv_S <- auc(roc_curve_cv_S)
cat("AUC for cv_S model is:", auc_val_cv_S, "\n")

# Accuracy & F1
acc_cv_S <- mean(test_data_cv_S$pred_class == test_data_cv_S$outcome)
f1_cv_S <- F1_Score(as.factor(test_data_cv_S$pred_class), as.factor(test_data_cv_S$outcome), positive = "1")
cat("Accuracy (cv_S):", acc_cv_S, "\n")
cat("F1 Score (cv_S):", f1_cv_S, "\n")


######## Diastolic CV ########
model_cv_D <- glm(outcome ~ cv_D, data = train_data, family = "binomial")
test_data_cv_D <- test_data %>%
  mutate(pred_prob = predict(model_cv_D, newdata = ., type = "response"))

roc_curve_cv_D <- roc(test_data_cv_D$outcome, test_data_cv_D$cv_D)
opt_thresh_cv_D <- coords(roc_curve_cv_D, "best", best.method = "youden", transpose = FALSE)
opt_thresh_val_cv_D <- as.numeric(opt_thresh_cv_D["threshold"])

cat("Optimal cutoff for cv_D is:", opt_thresh_val_cv_D, "\n")

test_data_cv_D <- test_data_cv_D %>%
  mutate(pred_class = if_else(cv_D >= opt_thresh_val_cv_D, 1, 0))

conf_matrix_cv_D <- table(Observed = test_data_cv_D$outcome, Predicted = test_data_cv_D$pred_class)
print(conf_matrix_cv_D)

auc_val_cv_D <- auc(roc_curve_cv_D)
cat("AUC for cv_D model is:", auc_val_cv_D, "\n")

# Accuracy & F1
acc_cv_D <- mean(test_data_cv_D$pred_class == test_data_cv_D$outcome)
f1_cv_D <- F1_Score(as.factor(test_data_cv_D$pred_class), as.factor(test_data_cv_D$outcome), positive = "1")
cat("Accuracy (cv_D):", acc_cv_D, "\n")
cat("F1 Score (cv_D):", f1_cv_D, "\n")







