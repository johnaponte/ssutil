# Close procedure with elimination
# Procedure Ntb
# Define the main function with parameters k, d, and P*
usentb <- function(k, P_star) {

  # Check that P_star is in the correct range 1/k < P_star < 1
  if (P_star <= 1/k || P_star >= 1) {
    stop("P* must satisfy 1/k < P* < 1.")
  }

  # Define the objective function
  objective_function <- function(c) {
    c1 <- c[1]
    c2 <- c[2]
    d  <- c[3]

    # The integral part of the objective function
    integral_result <- integrate(function(x) {
      (pnorm(x + d)^(k - 1) - pnorm(x - d)^(k - 1))*dnorm(x)
    }, lower = -Inf, upper = Inf)$value

    # The full objective function
    return(k * c1^2 + k * c2^2 * integral_result)
  }

  # Define the constraint function
  constraint_function <- function(c) {
    c1 <- c[1]
    c2 <- c[2]
    d <- c[3]

    # The integral part of the constraint
    integral_result <- integrate(function(x) {
      (pnorm(x + c1 + d)^(k - 1) + pnorm(x + sqrt(c1^2 + c2^2))^(k - 1))*dnorm(x)
    }, lower = -Inf, upper = Inf)$value

    # The full constraint (should be >= P*)
    return(P_star - (integral_result - 1))
  }

  # Define the constraint wrapper for nloptr
  constraints <- function(c) {
    return(c(constraint_function(c)))
  }

  # Set initial values for c1 and c2 (positive values)
  start_values <- c(5, 5,1)  # Positive starting points for c1 and c2

  # Optimization using nloptr
  result <- nloptr(
    x0 = start_values,
    eval_f = objective_function,
    eval_g_ineq = constraints,
    lb = c(1, 1, 0.1),  # Lower bounds to ensure c1 and c2 and d are positive
    opts = list(
      algorithm = "NLOPT_LN_COBYLA",
      xtol_rel = 1e-8,
      maxeval = 5000
    )
  )

  # Return the optimization result
  return(result$solution)
}
#
# # Example usage:
# # Replace k, d, and P_star with your specific values
# k_value <- 7 # Example value of k
# P_star_value <- 0.9  # Example value of P* in the range 1/k < P* < 1
#
# # Call the function
# result <- usentb(k_value, P_star_value)
#
# # Print the result
# print(result)
