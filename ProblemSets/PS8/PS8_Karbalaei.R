install.packages("nloptr")
set.seed(100)
N <- 100000
K <- 10

# Create matrix X with normally distributed random numbers
# First column should be all 1's
X <- matrix(rnorm(N * (K-1)), nrow = N, ncol = K-1)
X <- cbind(rep(1, N), X)

# Create the error term (eps) with normal distribution N(0, sigma^2)
sigma <- 0.5  # so sigma^2 = 0.25
eps <- rnorm(N, mean = 0, sd = sigma)

# Create the beta vector with the specified values
beta <- c(1.5, -1, -0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

# Generate Y = X*beta + eps
Y <- X %*% beta + eps

# Verify dimensions
cat("Dimensions of X:", dim(X), "\n")
cat("Length of eps:", length(eps), "\n")
cat("Length of beta:", length(beta), "\n")
cat("Length of Y:", length(Y), "\n")

# Display the first few rows of data to verify
head(X)
head(Y)


## Question 5: Compute OLS estimate using closed-form solution
# Compute beta_hat_OLS = (X'X)^(-1)X'Y, t(X) is the transpose of X. solve() is used to compute the inverse of a matrix
beta_hat_OLS <- solve(t(X) %*% X) %*% t(X) %*% Y

# Compare with the true beta
comparison <- data.frame(
  True_Beta = beta,
  Estimated_Beta = as.vector(beta_hat_OLS),
  Difference = as.vector(beta_hat_OLS) - beta
)

# Display the comparison
print(comparison)

# Calculate how close the estimate is to the true value
cat("Sum of squared differences:", sum((as.vector(beta_hat_OLS) - beta)^2), "\n")
cat("Mean absolute error:", mean(abs(as.vector(beta_hat_OLS) - beta)), "\n")


## Question 6: Compute OLS estimate using gradient descent, For more info about gradient descent function look at your notes 
# Define the objective function (sum of squared residuals), we want to minimize this function
ssr <- function(beta_params, X, Y) {
  residuals <- Y - X %*% beta_params
  return(sum(residuals^2))
}

# Define the gradient of the objective function
gradient <- function(beta_params, X, Y) {
  # Gradient of SSR with respect to beta is -2*X'*(Y - X*beta)
  residuals <- Y - X %*% beta_params
  grad <- -2 * t(X) %*% residuals
  return(grad)
}

# Implement gradient descent, learning rate is the step size.
gradient_descent <- function(X, Y, learning_rate, max_iterations = 10000, 
                             tolerance = 1e-8, beta_init = NULL) {
  # Initialize beta (start with zeros if not provided)
  if (is.null(beta_init)) {
    beta_current <- matrix(0, nrow = ncol(X), ncol = 1)
  } else {
    beta_current <- beta_init
  }
  
  # Initialize variables to track progress
  iteration <- 0
  converged <- FALSE
  loss_history <- numeric(max_iterations)
  
  # Run gradient descent
  while (!converged && iteration < max_iterations) {
    # Calculate gradient, It calculates the direction of steepest increase in the loss function at the current point
    grad <- gradient(beta_current, X, Y)
    
    # Update beta
    beta_new <- beta_current - learning_rate * grad
    
    # Calculate loss
    loss_current <- ssr(beta_current, X, Y)
    loss_new <- ssr(beta_new, X, Y)
    loss_history[iteration + 1] <- loss_current
    
    # Check for convergence
    if (abs(loss_new - loss_current) < tolerance) {
      converged <- TRUE
    }
    
    # Update for next iteration
    beta_current <- beta_new
    iteration <- iteration + 1
  }
  
  return(list(
    beta = beta_current,
    iterations = iteration,
    converged = converged,
    loss_history = loss_history[1:iteration]
  ))
}

# Run gradient descent with the specified learning rate
learning_rate <- 0.0000003
gd_result <- gradient_descent(X, Y, learning_rate)

# Extract the estimated beta
beta_hat_GD <- gd_result$beta

# Compare with the true beta and the OLS solution
comparison_gd <- data.frame(
  True_Beta = beta,
  OLS_Beta = as.vector(beta_hat_OLS),
  GD_Beta = as.vector(beta_hat_GD),
  Diff_GD_True = as.vector(beta_hat_GD) - beta,
  Diff_GD_OLS = as.vector(beta_hat_GD) - as.vector(beta_hat_OLS)
)

# Display the comparison
print(comparison_gd)
cat("Gradient descent iterations:", gd_result$iterations, "\n")
cat("Gradient descent converged:", gd_result$converged, "\n")
cat("Final loss value:", tail(gd_result$loss_history, 1), "\n")

## Question 7: Compute OLS using nloptr's L-BFGS and Nelder-Mead algorithms
library(nloptr)

# Define the objective function for nloptr (sum of squared residuals)
obj_fun <- function(beta_params, X, Y) {
  residuals <- Y - X %*% beta_params
  return(sum(residuals^2))
}

# Define the gradient function for nloptr
obj_grad <- function(beta_params, X, Y) {
  residuals <- Y - X %*% beta_params
  grad <- -2 * t(X) %*% residuals
  return(as.vector(grad))
}

# Initialize beta to zeros
beta_init <- rep(0, ncol(X))

# Compute OLS using L-BFGS algorithm
lbfgs_result <- nloptr(
  x0 = beta_init,
  eval_f = obj_fun,
  eval_grad_f = obj_grad,
  opts = list(
    algorithm = "NLOPT_LD_LBFGS",
    xtol_rel = 1e-8,
    maxeval = 1000
  ),
  X = X,
  Y = Y
)

# Extract L-BFGS beta estimate
beta_hat_LBFGS <- lbfgs_result$solution

# Compute OLS using Nelder-Mead algorithm (doesn't use gradient)
nm_result <- nloptr(
  x0 = beta_init,
  eval_f = obj_fun,
  opts = list(
    algorithm = "NLOPT_LN_NELDERMEAD",
    xtol_rel = 1e-8,
    maxeval = 5000
  ),
  X = X,
  Y = Y
)

# Extract Nelder-Mead beta estimate
beta_hat_NM <- nm_result$solution

# Compare results from all methods
comparison_all <- data.frame(
  True_Beta = beta,
  OLS_Beta = as.vector(beta_hat_OLS),
  GD_Beta = as.vector(beta_hat_GD),
  LBFGS_Beta = beta_hat_LBFGS,
  NM_Beta = beta_hat_NM
)

# Display the comparison
print(comparison_all)

# Calculate differences between methods
cat("Difference between L-BFGS and OLS:", sum((beta_hat_LBFGS - as.vector(beta_hat_OLS))^2), "\n")
cat("Difference between Nelder-Mead and OLS:", sum((beta_hat_NM - as.vector(beta_hat_OLS))^2), "\n")
cat("Difference between L-BFGS and Nelder-Mead:", sum((beta_hat_LBFGS - beta_hat_NM)^2), "\n")



## Question 8: Compute MLE using nloptr's L-BFGS algorithm
# Define the negative log-likelihood function for normal linear regression
#The function penalizes negative values of sigma to ensure a valid solution
neg_log_likelihood <- function(theta, Y, X) {
  beta <- theta[1:(length(theta) - 1)]
  sigma <- theta[length(theta)]
  
  # Check if sigma is positive
  if (sigma <= 0) {
    return(1e10)  # Return a large value if constraint is violated
  }
  
  n <- length(Y)
  residuals <- Y - X %*% beta
  
  # Negative log-likelihood for normal distribution
  nll <- n * log(sigma) + sum(residuals^2) / (2 * sigma^2)
  
  return(nll)
}

# Define the gradient of the negative log-likelihood (as provided in the problem set)
gradient <- function(theta, Y, X) {
  grad <- as.vector(rep(0, length(theta)))
  beta <- theta[1:(length(theta) - 1)]
  sig <- theta[length(theta)]
  #In the code above, theta is the vector of parameters, Y is the response vector, and X is the design matrix.
  #By parameters I mean coefficients and sigma.
  
  grad[1:(length(theta) - 1)] <- -t(X) %*% (Y - X %*% beta)/(sig^2)
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y - X %*% beta)/(sig^3)
  
  return(grad)
}

# Initialize parameters (beta starts at zeros, sigma at 1)
theta_init <- c(rep(0, ncol(X)), 1)

# Compute MLE using L-BFGS algorithm
mle_result <- nloptr(
  x0 = theta_init,
  eval_f = neg_log_likelihood,
  eval_grad_f = gradient,
  opts = list(
    algorithm = "NLOPT_LD_LBFGS",
    xtol_rel = 1e-8,
    maxeval = 1000
  ),
  Y = Y,
  X = X
)

# Extract MLE estimates
beta_hat_MLE <- mle_result$solution[1:ncol(X)]
sigma_hat_MLE <- mle_result$solution[length(mle_result$solution)]

# Compare MLE and OLS beta estimates
comparison_mle <- data.frame(
  True_Beta = beta,
  OLS_Beta = as.vector(beta_hat_OLS),
  MLE_Beta = beta_hat_MLE,
  Difference = beta_hat_MLE - as.vector(beta_hat_OLS)
)

# Display the results
print(comparison_mle)
cat("Estimated sigma (MLE):", sigma_hat_MLE, "\n")
cat("True sigma:", sigma, "\n")
cat("Difference between MLE and OLS (sum of squared differences):", 
    sum((beta_hat_MLE - as.vector(beta_hat_OLS))^2), "\n")



## Question 9: Compute OLS using lm() and export results with modelsummary
# Load required packages
if (!requireNamespace("modelsummary", quietly = TRUE)) {
  install.packages("modelsummary")
}
library(modelsummary)

# The X matrix has a column of 1s for the intercept, but lm() adds its own intercept
# So we'll use the X matrix without the first column and tell lm() to include an intercept
X_no_intercept <- X[, -1]  # Remove the first column (all 1s)

# Fit the OLS model using lm()
ols_model <- lm(Y ~ X_no_intercept)

# Alternatively, we can use the original X matrix but tell lm() not to include its own intercept
# (this is the approach suggested in the problem set)
ols_model_no_intercept <- lm(Y ~ X - 1)

# Compare coefficients from different methods
comparison_final <- data.frame(
  True_Beta = beta,
  OLS_ClosedForm = as.vector(beta_hat_OLS),
  OLS_lm = coef(ols_model_no_intercept)
)

# Calculate differences
cat("Difference between lm() and closed-form OLS (sum of squared differences):", 
    sum((coef(ols_model_no_intercept) - as.vector(beta_hat_OLS))^2), "\n")

# Create LaTeX output using modelsummary
# Note: This will create a .tex file in your working directory
modelsummary(
  list("OLS Model" = ols_model_no_intercept),
  output = "PS8_regression_results.tex",
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs")
)

# For the .tex file content, add a section comparing estimates to ground truth
tex_content <- c(
  "\\section{Comparison of Estimated Coefficients to Ground Truth}",
  "\\begin{tabular}{lrrr}",
  "\\hline",
  "Parameter & True Value & Estimated Value & Difference \\\\",
  "\\hline"
)

# Add rows for each coefficient
for (i in 1:length(beta)) {
  param_name <- ifelse(i == 1, "Intercept", paste0("X", i-1))
  tex_content <- c(tex_content, 
                   sprintf("%s & %.4f & %.4f & %.4f \\\\", 
                           param_name, 
                           beta[i], 
                           coef(ols_model_no_intercept)[i], 
                           coef(ols_model_no_intercept)[i] - beta[i]))
}

# Close the table
tex_content <- c(tex_content,
                 "\\hline",
                 "\\end{tabular}",
                 "",
                 "The estimates are very close to the ground truth values, which is expected given the large sample size (N = 100,000). The differences are due to the random error term that was added during data generation. With this large sample size, the Law of Large Numbers ensures that our estimates converge to the true parameter values.")

# Write the additional content to a separate .tex file
writeLines(tex_content, "PS8_coefficient_comparison.tex")

# Print information about the files created
cat("Created two .tex files:\n")
cat("1. PS8_regression_results.tex - Contains the regression output from modelsummary\n")
cat("2. PS8_coefficient_comparison.tex - Contains a comparison of estimated coefficients to ground truth\n")
cat("These files need to be included in your main .tex file\n")