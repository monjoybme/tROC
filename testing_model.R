# ====================
# PART 2: TESTING DATA (using timeROC)
# ====================

# Load libraries
library(survival)
library(timeROC)

# === 1. Load test data ===
test_data <- read.csv("test_data.csv")

# === 2. Load trained Cox model and cutoff ===
load("cox_model_and_cutoff.RData")

# === 3. Compute log-risk score for test data ===
test_data$log_risk_score <- predict(cox_model, newdata = test_data, type = "lp")

# === 4. Apply cutoff to stratify test patients ===
test_data$risk_group <- ifelse(test_data$log_risk_score >= cutoff, "High", "Low")

# === 5. Plot KM curves for test data ===
fit_test <- survfit(Surv(time, status) ~ risk_group, data = test_data)
plot(fit_test, col = c("blue", "red"), main = "Test Data: KM Curves by Risk Group",
     xlab = "Time (months)", ylab = "Survival Probability")
legend("bottomleft", legend = c("Low Risk", "High Risk"), col = c("blue", "red"), lty = 1)

# === 6. Log-rank test on test data ===
logrank_test <- survdiff(Surv(time, status) ~ risk_group, data = test_data)
print(logrank_test)
