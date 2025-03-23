# Install required packages (if not already installed)
if (!require("mice")) install.packages("mice")
if (!require("modelsummary")) install.packages("modelsummary")

# Load the packages after installation
library(mice)
library(modelsummary)
wages_data <- read.csv("C:/Users/ASUSCenter/Downloads/wages.csv")

# Take a look at the structure of the data
str(wages_data)
head(wages_data)
wages_clean <- wages_data[!is.na(wages_data$hgc) & !is.na(wages_data$tenure), ]
cat("Original number of rows:", nrow(wages_data), "\n")
cat("Number of rows after removing missing hgc or tenure:", nrow(wages_clean), "\n")
cat("Number of rows removed:", nrow(wages_data) - nrow(wages_clean), "\n")
# Question 6: Create a summary table using modelsummary
datasummary_skim(wages_clean, output = "markdown")

# still Question 6/ Calculate missing rate for logwage
missing_logwage <- sum(is.na(wages_clean$logwage))
cat("Number of missing logwage values:", missing_logwage, "\n")
cat("Percentage of missing logwage values:", round(missing_logwage/nrow(wages_clean)*100, 2), "%\n")
#we should examine whether the missing logwage values are related to other variables
# Create a missing indicator
wages_data$logwage_missing <- is.na(wages_data$logwage)

# Compare variables between groups with missing and non-missing logwage
aggregate(wages_data[, c("age", "hgc", "tenure")], 
          by = list(Missing = wages_data$logwage_missing), 
          FUN = mean, na.rm = TRUE)
#Question 7: Compare different imputation methods
# 1. Complete case analysis (listwise deletion)
complete_data <- na.omit(wages_clean)
model_complete <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = complete_data)

# 2. Mean imputation
wages_mean_imp <- wages_clean
wages_mean_imp$logwage[is.na(wages_mean_imp$logwage)] <- mean(wages_mean_imp$logwage, na.rm = TRUE)
model_mean <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages_mean_imp)

# 3. Predicted value imputation 
# First, fit a model on complete cases
pred_model <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = complete_data)

# Create dataset for predicted imputation
wages_pred_imp <- wages_clean
# Get predictions for missing values
missing_indices <- which(is.na(wages_pred_imp$logwage))
wages_pred_imp$logwage[missing_indices] <- predict(pred_model, newdata = wages_pred_imp[missing_indices, ])
model_pred <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married, data = wages_pred_imp)

# 4. Multiple imputation using mice
# Set a seed for reproducibility
set.seed(123)
# Create imputation model
imp <- mice(wages_clean, m = 5, printFlag = FALSE)
# Fit model to each imputed dataset
model_mice <- with(imp, lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married))
# Pool results
model_mice_pooled <- pool(model_mice)

# Create a table with all model results
models <- list(
  "Complete Cases" = model_complete,
  "Mean Imputation" = model_mean,
  "Predicted Imputation" = model_pred,
  "Multiple Imputation" = model_mice_pooled
)

# Generate the table with modelsummary
ms_table <- modelsummary(models, stars = TRUE, output = "markdown")
ms_table
