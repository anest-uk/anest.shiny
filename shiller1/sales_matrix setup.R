# Case-Shiller Model: Iterative vs. nleqslv Solution

# Simulated Case-Shiller-like data using sales_matrix format
sales_matrix <- matrix(
  c(
    NA, 100, 105, 110, 115,
    NA,  NA,  95, 100, 105,
    80,  NA,  NA,  95, 100,
    75,  80,  NA,  NA,  95,
    70,  75,  80,  NA,  NA
  ),
  nrow = 5, byrow = TRUE
)

colnames(sales_matrix) <- paste0("Period_", 0:4)
rownames(sales_matrix) <- paste0("Property_", 1:5)

# Iterative Solution
initial_guess <- rep(1, 4)  # Initialize beta values


ibar <- 100
jbar <- 30
x1 <- runif(ibar*jbar)*10+10
x2 <- ifelse(x1<11,NA,x1)
sales_matrix <- matrix(x2,ibar,jbar)
colnames(sales_matrix) <- paste0("Period_", 0:(jbar-1))
rownames(sales_matrix) <- paste0("Property_", 1:(ibar))
initial_guess <- rep(1, jbar-1)  # Initialize beta values
jbar
