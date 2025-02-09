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
case_shiller_system <- function(beta) {
  beta1 <- beta[1]
  beta2 <- beta[2]
  
  eq1 <- 1/beta1 - ( (P11 + P21 + P51) / (P10 + P20 + beta2 * P52) )
  eq2 <- 1/beta2 - ( (P32 + P42 + P52_2) / (P30 + P40 + beta1 * P51_2) )
  
  return(c(eq1, eq2))
}

# Initial guesses for beta
initial_guess <- c(1, 1)

# Solve the system using nleqslv
result <- nleqslv(initial_guess, case_shiller_system)

# Display the results
cat("Converged after", result$iter, "iterations\n")
cat("Estimated beta1:", round(result$x[1], 4), "\n")
cat("Estimated beta2:", round(result$x[2], 4), "\n")
