# Load required library
library(AER)  # For IV regression

# Perform IV estimation without an intercept
#iv_model <- ivreg(ymat ~ xmat - 1 | zmat - 1)  # "- 1" removes intercept
iv_model <- ivreg(ymat ~ xmat -1| zmat - 1)  # "- 1" removes intercept
sol1 <- solve(t(zmat)%*%xmat,t(zmat)%*%ymat)
as.matrix(coef(iv_model))-sol1

# Summary of the IV regression, including coefficients, standard errors, t-stats, and R-squared
summary(iv_model)

# Extract useful regression statistics
coefficients <- coef(iv_model)  # Estimated coefficients
standard_errors <- sqrt(diag(vcov(iv_model)))  # Standard errors
t_stats <- coefficients / standard_errors  # t-statistics
r_squared <- summary(iv_model)$r.squared  # R-squared
adj_r_squared <- summary(iv_model)$adj.r.squared  # Adjusted R-squared
residuals_iv <- residuals(iv_model)  # Residuals

r2_uncentered <- sum((fitted(iv_model)*as.numeric(ymat>0))^2) / sum(ymat^2)
r2_uncentered

# Store results in a list for further analysis
iv_results <- list(
  model = iv_model,
  coefficients = coefficients,
  standard_errors = standard_errors,
  t_statistics = t_stats,
  r_squared = r_squared,
  adjusted_r_squared = adj_r_squared,
  residuals = residuals_iv
)

# Print the results
summary(iv_model)
plot(log(1/coefficients))

