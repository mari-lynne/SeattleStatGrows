
# Typhoid Risk Classification using caret
# ----------------------------------------
# In this tutorial, we'll use a simulated dataset of 45 countries to classify
# whether a country has high typhoid incidence using machine learning.
# We'll use the `caret` package to build, tune, and evaluate our models.

# Load required packages
install.packages("caret")
library(caret)
library(tidyr)
library(dplyr)
library(tidylog)

# Load the dataset
data <- read.csv("C:/Users/mjohnso5/Documents/teaching/data/typhoid_simplified_caret.csv")

# Preview the dataset
str(data)
summary(data)
table(data$high_typhoid_incidence)

# Convert target to factor for classification
data$high_typhoid_incidence <- factor(data$high_typhoid_incidence, labels = c("Low", "High"))

# Split into training (80%) and testing (20%) sets
set.seed(123)
train_index <- createDataPartition(data$high_typhoid_incidence, p = 0.7, list = FALSE)
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Define predictor variables
predictors <- c("access_to_clean_water_pct", "population_density",
                "hiv_prevalence_pct", "median_age", "typhoid_vax_coverage_pct")

predictors

# Optional: Scale and center numeric predictors
preproc <- preProcess(train_data[, predictors], method = c("center", "scale"))


train_scaled <- predict(preproc, train_data[, predictors])
test_scaled <- predict(preproc, test_data[, predictors])

# Add target back in
train_scaled$high_typhoid_incidence <- train_data$high_typhoid_incidence
test_scaled$high_typhoid_incidence <- test_data$high_typhoid_incidence

# Define training control (cross-validation)
ctrl <- trainControl(method = "cv", number = 5, classProbs = TRUE, summaryFunction = twoClassSummary)

# Train a Random Forest model
set.seed(42)
model_rf <- train(high_typhoid_incidence ~ .,
                  data = train_scaled,
                  method = "rf",
                  trControl = ctrl,
                  metric = "ROC",
                  importance = TRUE)


# Print model summary
print(model_rf)

# Plot model performance
plot(model_rf)

# Predict on test data
pred_rf <- predict(model_rf, newdata = test_scaled)
confusionMatrix(pred_rf, test_scaled$high_typhoid_incidence)

# Predict probabilities and calculate ROC
library(pROC)
probs_rf <- predict(model_rf, newdata = test_scaled, type = "prob")
roc_rf <- roc(test_scaled$high_typhoid_incidence, probs_rf$High)
plot(roc_rf, main = "ROC Curve - Random Forest")

# Variable importance
varImp(model_rf)

