# Solve using nleqslv
if (!require("nleqslv")) install.packages("nleqslv")
library(nleqslv)

case_shiller_system <- function(beta, sales_matrix) {
  nper <- ncol(sales_matrix)-1 #excluding base period t=0
  equations <- numeric(nper)
  
  for (t in 1:nper) {
    numerator <- sum(sales_matrix[, t + 1], na.rm = TRUE)
    denominator <- sum(sales_matrix[, t], na.rm = TRUE) + sum(beta[-t] * sales_matrix[, t + 1][1:min(length(beta[-t]), length(sales_matrix[, t + 1]))], na.rm = TRUE)
    equations[t] <- 1 / beta[t] - (numerator / denominator)
  }
  
  return(equations)
}

nleq_result <- 
  nleqslv(initial_guess, case_shiller_system, sales_matrix = sales_matrix)

cat("nleqslv method converged after", nleq_result$iter, "iterations\n")
cat("Estimated beta values:\n")
print(round(nleq_result$x, 4))

#beta is from manual iteration
print(beta-nleq_result$x)
