beta <- initial_guess

tolerance <- 1e-6
max_iter <- 1000
iter <- 0
converged <- FALSE
nper <- ncol(sales_matrix)-1

while (iter < max_iter && !converged) {
  iter <- iter + 1
  beta_new <- beta
  
  for (t in 1:nper) {
    numerator <- sum(sales_matrix[, t + 1], na.rm = TRUE)
    denominator <- sum(sales_matrix[, t], na.rm = TRUE) + sum(beta[-t] * sales_matrix[, t + 1][1:min(length(beta[-t]), length(sales_matrix[, t + 1]))], na.rm = TRUE)
    beta_new[t] <- 1 / (numerator / denominator)
  }
  
  # Check for convergence
  if (max(abs(beta_new - beta)) < tolerance) {
    converged <- TRUE
  }
  beta <- beta_new
}
cat("Iterative method converged after", iter, "iterations\n")
cat("Estimated beta values:\n")
print(round(beta, 4))
barplot(beta)
