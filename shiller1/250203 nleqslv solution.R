#now rows=properties, cols = periods 0:5

# Load necessary package
if (!require("nleqslv")) install.packages("nleqslv")
library(nleqslv)

# Simulated sales matrix (Properties as rows, Time Periods as columns)
sales_matrix <- matrix(c(
  95, 100, NA,  NA,  NA,  NA,
  90, 110, 125, NA,  NA,  NA,
  105,  NA, 130, 135, NA,  NA,
  NA,  NA, 119, 140, 145, 155,
  92, 105,  NA,  NA, 148, 157
), nrow = 5, byrow = TRUE)

colnames(sales_matrix) <- paste0("Period_", 0:5)
rownames(sales_matrix) <- paste0("Property_", 1:5)

# Generalized Case-Shiller System using the matrix
# Generalized Case-Shiller System using the matrix
case_shiller_system <- function(beta, sales_matrix) {
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
# Initial guess for beta values
initial_guess <- rep(1, ncol(sales_matrix) - 1)

# Solve using nleqslv
result <- nleqslv(initial_guess, case_shiller_system, sales_matrix = sales_matrix)

# Display results
cat("Converged after", result$iter, "iterations\n")
cat("Estimated beta values:\n")
print(round(result$x, 4))
