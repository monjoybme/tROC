# ====================
# PART 1: TRAINING DATA
# ====================

# Load necessary libraries
install.packages(c("survival", "survivalROC"))
library(survival)
library(survivalROC)

# === 1. Load or prepare training data ===
# Assuming your training data has: time, status, and predictors (X1, X2, ...)
# Replace this with your actual data
train_data <- read.csv("train_data.csv")

# === 2. Fit Cox model and calculate log-risk score ===
cox_model <- coxph(Surv(time, status) ~ ., data = train_data)
train_data$log_risk_score <- predict(cox_model, type = "lp")  # linear predictor

# === 3. Compute time-dependent ROC at 10 years (120 months) ===
roc <- survivalROC(
  Stime = train_data$time,
  status = train_data$status,
  marker = train_data$log_risk_score,
  predict.time = 120,  # 10 years
  method = "KM"
)

# === 4. Find optimal cutoff using Youden Index ===
youden_index <- roc$TP - roc$FP
optimal_idx <- which.max(youden_index)
cutoff <- roc$cut.values[optimal_idx]
cat("Optimal cutoff (Youden Index):", cutoff, "\n")

# === 5. Stratify training patients ===
train_data$risk_group <- ifelse(train_data$log_risk_score >= cutoff, "High", "Low")

# === 6. Plot KM curves in training set ===
fit_train <- survfit(Surv(time, status) ~ risk_group, data = train_data)
plot(fit_train, col = c("blue", "red"), main = "Training Data: KM Curves by Risk Group",
     xlab = "Time (months)", ylab = "Survival Probability")
legend("bottomleft", legend = c("Low Risk", "High Risk"), col = c("blue", "red"), lty = 1)

# === 7. Log-rank test ===
logrank_train <- survdiff(Surv(time, status) ~ risk_group, data = train_data)
print(logrank_train)

# === 8. Save model and cutoff for test set use ===
save(cox_model, cutoff, file = "cox_model_and_cutoff.RData")
