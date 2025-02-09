# Load necessary package
#install.packages("systemfit")
library(systemfit)

# Simulated Case-Shiller-like data: 5 properties, 3 time periods (0,1,2)
P <- matrix(c(
  100, 110, NA,  # Property 1: sold at t=0, t=1
  90, 100, NA,   # Property 2: sold at t=0, t=1
  NA, 95, 110,   # Property 3: sold at t=1, t=2
  NA, 85, 100,   # Property 4: sold at t=1, t=2
  80, 90, 105    # Property 5: sold at t=0, t=2
), nrow = 5, byrow = TRUE)

# Step 1: Constructing X, Y, and Z
n <- nrow(P)
X <- matrix(0, nrow = n, ncol = 2)  # 2 time periods
Y <- matrix(0, nrow = n, ncol = 1)
Z <- matrix(0, nrow = n, ncol = 2)

for (i in 1:n) {
  if (!is.na(P[i, 1]) && !is.na(P[i, 2])) {  # Sale between t0 and t1
    X[i, 1] <- P[i, 1]  # Initial sale price
    Z[i, 1] <- 1        # Instrument for t1
    Y[i, 1] <- P[i, 2]  # Second sale price
  }
  if (!is.na(P[i, 2]) && !is.na(P[i, 3])) {  # Sale between t1 and t2
    X[i, 2] <- P[i, 2]
    Z[i, 2] <- 1  # Instrument for t2
    Y[i, 1] <- P[i, 3]
  }
}

# Step 2: Iterative Solution using qr.solve()
beta <- matrix(c(1, 1), ncol = 1)  # Initial beta estimates
tolerance <- 1e-6
max_iter <- 100
iter <- 0
converged <- FALSE

while (iter < max_iter && !converged) {
  iter <- iter + 1
  
  # Compute new beta using qr.solve for numerical stability
  beta_new <- qr.solve(t(Z) %*% X, t(Z) %*% Y)#<<<<<<<<<   qr.solve()
  
  # Check convergence
  if (max(abs(beta_new - beta)) < tolerance) {
    converged <- TRUE
  }
  
  beta <- beta_new  # Update beta
}

# Final results
cat("Estimated Beta using qr.solve:\n", beta, "\n")

# Step 3: Solve using systemfit
formula1 <- Y ~ X[,1]
formula2 <- Y ~ X[,2]

system <- list(eq1 = formula1, eq2 = formula2)
inst <- ~ Z[,1] + Z[,2]

data <- data.frame(Y = Y[,1], X1 = X[,1], X2 = X[,2], Z1 = Z[,1], Z2 = Z[,2])
fit <- systemfit(system, method = "3SLS", inst = inst, data = data)
summary(fit)


####fails^^^^
#but not fixed sign
x1 <- data.table(data)
x1[5,X1:=-X1]
x1[5,Z1:=-Z1]
x1[,Y:=Y+rnorm(5)]
fit <- systemfit(system, method = "3SLS", inst = inst, data = x1)
#####this does not fail, but need to solve same problem using solve() above

# Step 4: Extend to Large Data (Replace with actual 235,000 x 30 matrix)
library(Matrix)

# Simulated large dataset (replace with actual)
large_P <- matrix(runif(235000 * 30, min = 50, max = 500), nrow = 235000, ncol = 30)
large_P[large_P < 60] <- NA  # Introduce missing values

# Construct X, Y, and Z for large data
large_X <- large_P[, -1]  # Remove first column
large_Y <- large_P[, -ncol(large_P)]  # Remove last column
large_Z <- matrix(1, nrow = nrow(large_X), ncol = ncol(large_X))  # Instrument matrix

# Solve using qr.solve for large data
large_beta <- qr.solve(t(large_Z) %*% large_X, t(large_Z) %*% large_Y)

cat("Estimated Beta for Large Dataset:\n", large_beta, "\n")
