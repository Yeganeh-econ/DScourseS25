#######################
# Question 3: Install required packages
#######################
# Install required packages if not already installed
# First, detach any loaded packages that might be causing conflicts
if ("package:tidymodels" %in% search()) detach("package:tidymodels", unload = TRUE)

# Update the cli package specifically
install.packages("cli")

# Then reinstall tidymodels
install.packages("tidymodels")

# Load the packages
library(tidymodels)
library(glmnet)

#######################
# Question 4: Load housing data from UCI, got the link from lecture 
#######################
# Load housing data from UCI
housing_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data"
housing <- read.table(housing_url, header = FALSE)

# Add column names (since the original data doesn't include them)
colnames(housing) <- c("crim", "zn", "indus", "chas", "nox", "rm", "age", 
                       "dis", "rad", "tax", "ptratio", "b", "lstat", "medv")

# Display the first few rows to verify the data was loaded correctly
head(housing)

# Check the structure of the data
str(housing)

#######################
# Question 5: Set the seed
#######################
# Set the seed for reproducibility
set.seed(123456)

#######################
# Question 6: Create training and testing sets
#######################
# Load required package from tidymodels for data splitting, rsample is for data splitting
library(rsample)

# Create training and testing sets (default is 75% training, 25% testing)
housing_split <- initial_split(housing, prop = 0.75)
housing_train <- training(housing_split)
housing_test <- testing(housing_split)

# Check dimensions of the datasets
dim(housing_train)
dim(housing_test)

#######################
# Question 7: Create the recipe and prepare the data, recepies is for data preprocessing 
#######################
# Load the recipes package
library(recipes)

# Create the recipe as specified in question 7
housing_recipe <- recipe(medv ~ ., data = housing) %>%
  # convert outcome variable to logs
  step_log(all_outcomes()) %>%
  # convert 0/1 chas to a factor
  step_bin2factor(chas) %>%
  # create interaction term between all variables
  step_interact(terms = ~ crim:zn:indus:rm:age:rad:tax:ptratio:b:lstat:dis:nox) %>%
  # create 6th degree polynomials of continuous variables
  step_poly(crim, zn, indus, rm, age, rad, tax, ptratio, b, lstat, dis, nox, degree = 6)

# Run the recipe
housing_prep <- housing_recipe %>% prep(housing_train, retain = TRUE)
housing_train_prepped <- housing_prep %>% juice()
housing_test_prepped <- housing_prep %>% bake(new_data = housing_test)

# Create x and y training and test data
housing_train_x <- housing_train_prepped %>% select(-medv)
housing_test_x <- housing_test_prepped %>% select(-medv)
housing_train_y <- housing_train_prepped %>% select(medv)
housing_test_y <- housing_test_prepped %>% select(medv)

# Check dimensions to answer question 7
dim(housing_train_x)
dim(housing)

# Calculate how many more variables we have compared to original data
cat("Original data dimensions:", dim(housing)[2], "columns\n")
cat("Transformed training data dimensions:", dim(housing_train_x)[2], "columns\n")
cat("Additional variables created:", dim(housing_train_x)[2] - dim(housing)[2] + 1, "\n") # +1 because we removed medv

#######################
# Question 8: LASSO Model with 6-fold CV
#######################
# Convert data to matrix format for glmnet
x_train_matrix <- as.matrix(housing_train_x)
y_train_vector <- as.vector(housing_train_y$medv)
x_test_matrix <- as.matrix(housing_test_x)
y_test_vector <- as.vector(housing_test_y$medv)

# Handle NA values that might be causing warnings
x_train_matrix[is.na(x_train_matrix)] <- 0
x_test_matrix[is.na(x_test_matrix)] <- 0

# Set up 6-fold cross-validation
set.seed(123456)  # Keep the same seed for consistency
cv_folds <- 6

# Make sure glmnet is properly loaded
library(glmnet)

# Check if the function exists
exists("cv.glmnet")

# If the above returns FALSE, try with the full namespace
# Replace:
lasso_cv <- cv.glmnet(
  x = x_train_matrix,
  y = y_train_vector,
  alpha = 1,  # 1 for LASSO
  nfolds = cv_folds
)

# With:
lasso_cv <- glmnet::cv.glmnet(
  x = x_train_matrix,
  y = y_train_vector,
  alpha = 1,  # 1 for LASSO
  nfolds = cv_folds
)

# Find the optimal lambda value
optimal_lambda_lasso <- lasso_cv$lambda.min
cat("Optimal lambda for LASSO:", optimal_lambda_lasso, "\n")

# Train final LASSO model with optimal lambda
lasso_model <- glmnet(
  x = x_train_matrix,
  y = y_train_vector,
  alpha = 1,
  lambda = optimal_lambda_lasso
)

# Calculate in-sample RMSE
lasso_predict_train <- predict(lasso_model, newx = x_train_matrix)
lasso_rmse_train <- sqrt(mean((lasso_predict_train - y_train_vector)^2))
cat("In-sample RMSE for LASSO:", lasso_rmse_train, "\n")

# Calculate out-of-sample RMSE
lasso_predict_test <- predict(lasso_model, newx = x_test_matrix)
lasso_rmse_test <- sqrt(mean((lasso_predict_test - y_test_vector)^2))
cat("Out-of-sample RMSE for LASSO:", lasso_rmse_test, "\n")

#######################
# Question 9: Ridge Model with 6-fold CV
#######################
# Perform Ridge Regression with cross-validation
ridge_cv <- cv.glmnet(
  x = x_train_matrix,
  y = y_train_vector,
  alpha = 0,  # 0 for Ridge
  nfolds = cv_folds
)

# Find the optimal lambda value
optimal_lambda_ridge <- ridge_cv$lambda.min
cat("Optimal lambda for Ridge:", optimal_lambda_ridge, "\n")

# Train final Ridge model with optimal lambda
ridge_model <- glmnet(
  x = x_train_matrix,
  y = y_train_vector,
  alpha = 0,
  lambda = optimal_lambda_ridge
)

# Calculate in-sample RMSE
ridge_predict_train <- predict(ridge_model, newx = x_train_matrix)
ridge_rmse_train <- sqrt(mean((ridge_predict_train - y_train_vector)^2))
cat("In-sample RMSE for Ridge:", ridge_rmse_train, "\n")

# Calculate out-of-sample RMSE
ridge_predict_test <- predict(ridge_model, newx = x_test_matrix)
ridge_rmse_test <- sqrt(mean((ridge_predict_test - y_test_vector)^2))
cat("Out-of-sample RMSE for Ridge:", ridge_rmse_test, "\n")

#######################
# Question 10: Analysis for LaTeX file (notes for writing)
#######################
# Bias-Variance Analysis - print information that will help with analysis
cat("\n=== Comparing Ridge and LASSO Models ===\n")
cat("LASSO in-sample RMSE:", lasso_rmse_train, "\n")
cat("LASSO out-of-sample RMSE:", lasso_rmse_test, "\n")
cat("LASSO RMSE difference (out-sample minus in-sample):", lasso_rmse_test - lasso_rmse_train, "\n\n")

cat("Ridge in-sample RMSE:", ridge_rmse_train, "\n")
cat("Ridge out-of-sample RMSE:", ridge_rmse_test, "\n")
cat("Ridge RMSE difference (out-sample minus in-sample):", ridge_rmse_test - ridge_rmse_train, "\n\n")

cat("Number of non-zero coefficients in LASSO model:", sum(coef(lasso_model) != 0), "\n")
cat("Total number of coefficients:", length(coef(lasso_model)), "\n")