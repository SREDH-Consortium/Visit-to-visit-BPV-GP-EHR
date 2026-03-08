library(pROC)
library(dplyr)

# Ensure reproducibility
set.seed(123)

# Step 1: Split the data 75% train, 25% test
train_index <- createDataPartition(BPV_55_cph$outcome, p = 0.75, list = FALSE)
train_data <- BPV_55[train_index, ]
test_data  <- BPV_55[-train_index, ]

######### Cut Off Value using SD ##############

######## Systolic ##########

# Step 2: Train a logistic regression model on the training set
model_sd_S <- glm(outcome ~ sd_S, data = train_data, family = "binomial")
summary(model_sd_S)

# Step 3: Predict probabilities in the test set
test_data_sd_S <- test_data %>%
  mutate(pred_prob = predict(model_sd_S, newdata = test_data, type = "response"))

# Step 4: Create ROC curve and find optimal threshold using Youden's index
roc_curve_sd_S <- roc(test_data_sd_S$outcome, test_data_sd_S$sd_S)
opt_thresh_sd_S <- coords(roc_curve_sd_S, "best", best.method = "youden", transpose = FALSE)
opt_thresh_val_sd_S <- as.numeric(opt_thresh_sd_S["threshold"])


cat("Optimal cutoff for sd_S is:", opt_thresh_val_sd_S, "\n")

# Step 5: Apply cutoff and classify
test_data_sd_S <- test_data_sd_S %>%
  mutate(pred_class = if_else(sd_S >= opt_thresh_val_sd_S, 1, 0))

# Step 6: Confusion matrix and AUC
conf_matrix_sd_S <- table(Observed = test_data_sd_S$outcome, Predicted = test_data_sd_S$pred_class)
print(conf_matrix_sd_S)

auc_val_sd_S <- auc(roc_curve_sd_S)
cat("AUC for sd_S model is:", auc_val_sd_S, "\n")


######## Diastolic ##########

# Step 2: Train a logistic regression model on the training set
model_sd_D <- glm(outcome ~ sd_D, data = train_data, family = "binomial")
summary(model_sd_D)

# Step 3: Predict probabilities in the test set
test_data_sd_D <- test_data %>%
  mutate(pred_prob = predict(model_sd_D, newdata = test_data, type = "response"))

# Step 4: Create ROC curve and find optimal threshold using Youden's index
roc_curve_sd_D <- roc(test_data_sd_D$outcome, test_data_sd_D$sd_D)
opt_thresh_sd_D <- coords(roc_curve_sd_D, "best", best.method = "youden", transpose = FALSE)
opt_thresh_val_sd_D <- as.numeric(opt_thresh_sd_D["threshold"])


cat("Optimal cutoff for sd_D is:", opt_thresh_val_sd_D, "\n")

# Step 5: Apply cutoff and classify
test_data_sd_D <- test_data_sd_D %>%
  mutate(pred_class = if_else(sd_D >= opt_thresh_val_sd_D, 1, 0))

# Step 6: Confusion matrix and AUC
conf_matrix_sd_D <- table(Observed = test_data_sd_D$outcome, Predicted = test_data_sd_D$pred_class)
print(conf_matrix_sd_D)

auc_val_sd_D <- auc(roc_curve_sd_D)
cat("AUC for sd_D model is:", auc_val_sd_D, "\n")

####################Cut off CV ######################
# --- For Systolic ---
model_cv_S <- glm(outcome ~ cv_S, data = train_data, family = "binomial")
test_data_cv_S <- test_data %>%
  mutate(pred_prob = predict(model_cv_S, newdata = ., type = "response"))

roc_curve_cv_S <- roc(test_data_cv_S$outcome, test_data_cv_S$pred_prob)
opt_thresh_cv_S <- coords(roc_curve_cv_S, "best", ret = "threshold")
opt_thresh_val_cv_S <- opt_thresh_cv_S[[1]]

cat("Optimal cutoff for cv_S is:", opt_thresh_val_cv_S, "\n")

test_data_cv_S <- test_data_cv_S %>%
  mutate(pred_class = if_else(pred_prob >= opt_thresh_val_cv_S, 1, 0))

conf_matrix_cv_S <- table(Observed = test_data_cv_S$outcome, Predicted = test_data_cv_S$pred_class)
print(conf_matrix_cv_S)

auc_val_cv_S <- auc(roc_curve_cv_S)
cat("AUC for cv_S model:", auc_val_cv_S, "\n")


# --- For Diastolic ---
model_cv_D <- glm(outcome ~ cv_D, data = train_data, family = "binomial")
test_data_cv_D <- test_data %>%
  mutate(pred_prob = predict(model_cv_D, newdata = ., type = "response"))

roc_curve_cv_D <- roc(test_data_cv_D$outcome, test_data_cv_D$pred_prob)
opt_thresh_cv_D <- coords(roc_curve_cv_D, "best", ret = "threshold")
opt_thresh_val_cv_D <- opt_thresh_cv_D[[1]]

cat("Optimal cutoff for cv_D is:", opt_thresh_val_cv_D, "\n")

test_data_cv_D <- test_data_cv_D %>%
  mutate(pred_class = if_else(pred_prob >= opt_thresh_val_cv_D, 1, 0))

conf_matrix_cv_D <- table(Observed = test_data_cv_D$outcome, Predicted = test_data_cv_D$pred_class)
print(conf_matrix_cv_D)

auc_val_cv_D <- auc(roc_curve_cv_D)
cat("AUC for cv_D model:", auc_val_cv_D, "\n")
