# Iterative Solution for Case-Shiller Model

# Simulated Case-Shiller-like data
P11 <- 100; P21 <- 90;  P51 <- 80
P10 <- 95;  P20 <- 85;  P52 <- 75
P32 <- 120; P42 <- 110; P52_2 <- 90
P30 <- 105; P40 <- 95;  P51_2 <- 85

# Initial guesses for beta
beta1 <- .1
beta2 <- 10

tolerance <- 1e-6
max_iter <- 1000
iter <- 0
converged <- FALSE

# Iterative process
while (iter < max_iter && !converged) {
  iter <- iter + 1
  
  # Update beta1 based on current beta2
  beta1_new <- 1 / ((P11 + P21 + P51) / (P10 + P20 + beta2 * P52))
  
  # Update beta2 based on updated beta1
  beta2_new <- 1 / ((P32 + P42 + P52_2) / (P30 + P40 + beta1_new * P51_2))
  
  # Check for convergence
  if (max(abs(beta1_new - beta1), abs(beta2_new - beta2)) < tolerance) {
    converged <- TRUE
  }
  
  # Update betas for next iteration
  beta1 <- beta1_new
  beta2 <- beta2_new
}

# Final output
cat("Converged after", iter, "iterations\n")
cat("Estimated beta1:", round(beta1, 4), "\n")
cat("Estimated beta2:", round(beta2, 4), "\n")

################# nleqslv
# Load necessary package
#install.packages("nleqslv")
library(nleqslv)

# Simulated Case-Shiller-like data
# P11 <- 100; P21 <- 90;  P51 <- 80
# P10 <- 95;  P20 <- 85;  P52 <- 75
# P32 <- 120; P42 <- 110; P52_2 <- 90
# P30 <- 105; P40 <- 95;  P51_2 <- 85

# Define the system of nonlinear equations
case_shiller_system_small <- function(beta) {
  beta1 <- beta[1]
  beta2 <- beta[2]
  
  eq1 <- 1/beta1 - ( (P11 + P21 + P51) / (P10 + P20 + beta2 * P52) )
  eq2 <- 1/beta2 - ( (P32 + P42 + P52_2) / (P30 + P40 + beta1 * P51_2) )
  
  return(c(eq1, eq2))
}

case_shiller_system_large <- function(beta, sales_matrix) {
  T <- ncol(sales_matrix) - 1  # Number of time periods (excluding Period 0)
  equations <- numeric(T)      # Store the system of equations
  
  for (t in 1:T) {
    # Identify sales in period t and t-1
    current_sales  <- sales_matrix[, t + 1]
    previous_sales <- sales_matrix[, t]
    
    # Adjustments for overlapping periods
    cross_terms <- 0
    if (t < T) {
      # Iterate over future periods to apply beta adjustments properly
      for (j in (t + 1):T) {
        cross_terms <- cross_terms + beta[j] * sum(sales_matrix[, j + 1], na.rm = TRUE)
      }
    }
    
    # Non-linear equation for beta_t
    numerator <- sum(current_sales, na.rm = TRUE)
    denominator <- sum(previous_sales, na.rm = TRUE) + cross_terms
    
    equations[t] <- 1 / beta[t] - (numerator / denominator)
  }
  
  return(equations)
}


# Initial guesses for beta
initial_guess <- rep(1, ncol(sales_matrix) - 1)

# Solve the system using nleqslv
#result1 <- nleqslv(initial_guess[-1], case_shiller_system_small)
result2 <- nleqslv(initial_guess, case_shiller_system_large, sales_matrix = sales_matrix)
result2 <- nleqslv(result2$x, case_shiller_system_large, sales_matrix = sales_matrix)
plot(1/result2$x)

# Display the results
cat("Converged after", result$iter, "iterations\n")
cat("Estimated beta1:", round(result$x[1], 4), "\n")
cat("Estimated beta2:", round(result$x[2], 4), "\n")
