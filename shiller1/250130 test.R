# Simulated price data (replace with actual Case-Shiller data)
P <- matrix(c(
  100, 90,  80,  # P11, P21, P51
  95, 85,  75,  # P10, P20, P52
  120, 110, 90,  # P32, P42, P52 (time period 2)
  105, 95,  85   # P30, P40, P51
), ncol = 3, byrow = TRUE)

# Initialize beta estimates
beta <- matrix(c(1, 1), ncol = 1)  # Initial guesses for β1 and β2
tolerance <- 1e-6
max_iter <- 100
iter <- 0
converged <- FALSE

# Define the instrument matrix Z (1s and -1s indicating sales)
Z <- matrix(c(1, 1, 1, 1, 1, 1), ncol = 2)  # Example structure

# Iterative estimation
while (iter < max_iter && !converged) {
  iter <- iter + 1
  
  # Construct the X matrix dynamically based on beta
  X <- matrix(c(
    P[1,1] + P[2,1] + P[3,1],  # Numerator for β1
    P[4,1] + P[5,1] + beta[2,1] * P[6,1],  # Denominator for β1
    P[1,2] + P[2,2] + P[3,2],  # Numerator for β2
    P[4,2] + P[5,2] + beta[1,1] * P[6,2]   # Denominator for β2
  ), ncol = 2, byrow = TRUE)
  
  # Apply the IV estimator β = (Z'X)^-1 Z'Y
  beta_new <- solve(t(Z) %*% X) %*% (t(Z) %*% P[,3])
  
  # Check for convergence
  if (max(abs(beta_new - beta)) < tolerance) {
    converged <- TRUE
  }
  
  beta <- beta_new  # Update beta estimates
}

# Output final estimates
cat("Estimated beta1:", beta[1,1], "\n")
cat("Estimated beta2:", beta[2,1], "\n")