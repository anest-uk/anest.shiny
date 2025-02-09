require(data.table)
require(magrittr)
# Simulated price data (replace with actual Case-Shiller data)
# P <- matrix(c(
#   100, 90,  80,  # P11, P21, P51
#   95, 85,  75,  # P10, P20, P52
#   120, 110, 90,  # P32, P42, P52 (time period 2)
#   105, 95,  85   # P30, P40, P51
# ), ncol = 3, bYrow = TRUE)
P <- matrix(c(
  100, 95,  0,  # P11, P21, P51
  95, 85,  0,  # P10, P20, P52
  120, 0, 90,  # P32, P42, P52 (time period 2)
  105, 0,  85,   # P30, P40, P51
  0, 99,  88   # P30, P40, P51
), ncol = 3, byrow = TRUE)
P
X <- P[,2:3]
X[5,1] <- -abs(X[5,1])
Y <- P[,1,drop=F]
Z <- X/abs(X)
Z[is.nan(Z)] <- 0
Z
# InitialiZe beta estimates
beta <- matrix(c(1, 1), ncol = 1)  # Initial guesses for β1 and β2
tolerance <- 1e-6
max_iter <- 100
iter <- 0
converged <- FALSE


#first check that AER can be replicated by (Z'X)-1 Z'Y
Z
X
Y
data.table(Z)%>%setnames(.,c(z1,z2))
betaiv <- solve(t(Z)%*%X)%*%(t(Z)%*%Y)
AER::ivreg(formula=)

# Define the instrument matrix Z (1s and -1s indicating sales)
#Z <- matrix(c(1, 1, 1, 1, 1, 1), ncol = 2)  # Example structure

# Iterative estimation
while (iter < max_iter && !converged) {
  iter <- iter + 1
  
  # Construct the X matrix dYnamicallY based on beta
  X <- matrix(c(
    P[1,1] + P[2,1] + beta[2,1] * P[5,3],  # Numerator for β1
    P[1,2] + P[2,2] + P[5,2],  # Denominator for β1
    P[3,1] + P[4,1] + beta[1,1] * P[5,2],              # Numerator for β2
    P[3,3] + P[4,3] +  P[5,3]   # Denominator for β2
  ), ncol = 2, byrow = TRUE)
  
  # ApplY the IV estimator β = (Z'X)^-1 Z'Y
  beta_new <- solve(t(Z) %*% X) %*% (t(Z) %*% Y)
  
  # Check for convergence
  if (max(abs(beta_new - beta)) < tolerance) {
    converged <- TRUE
  }
  
  beta <- beta_new  # Update beta estimates
}

# Output final estimates
cat("Estimated beta1:", beta[1,1], "\n")
cat("Estimated beta2:", beta[2,1], "\n")