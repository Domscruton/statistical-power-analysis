
pValueFun <- function(){
  # double assignment ensures object available outside function
  P[i, 1] <<- randomFun(data = dtNew, K = K, colTest = "xTest", 
                     colResponse = "xResponse", test.args = FALSE, 
                     level)
  # Actual t-test p-value
  P[i, 2] <<- t.test(H0[1:N, 2], H0[(N + 1):(2 * N), 2])$p.value
}


SimulationFun <- function(N = 1000, K = 1000, R = 100, alpha = 0.05, 
                          distribution = "gaussian", diff.means = 0, 
                          sd = 1, shape = NULL, rate = NULL){
  # Function to calculate size and power varying sample sizes
  # Inputs:
    # N: sample size to simulate for each level
    # K: number of randomization samples to calculate
    # R: number of test replications
    # alpha: significance level
    # distribution: distribution from which to simulate data
      # (can be either gaussian or gamma)
    # diff.means: difference in means of 2 levels if gaussian
    # sd: standard deviation for the gaussian distribution simulations
    # shape: length 2 vector of gamma shape parameters for each level
      # (only if gamma distribution specified)
    # rate: length 2 vector of gamma rate parameters for each level
    # (only if gamma distribution specified)  
  # Output:
    # Results: (4x1)- list containing the size and power of the two tests
  
  # Create matrix to store p-values from tests
  P <- matrix(data = NA, nrow = R, ncol = 2)
  # Create data.table to store simulated datasets
  # (pre-defining size reduces computational burden)
  dtNew <- data.table(xTest = c(rep("A", N), rep("B", N)), 
                      xResponse = NA)
  if (distribution == "gaussian") {
    if (!is.null(shape) | !is.null(rate)) {
      warning("Shape and/or rate parameters unused since simulations fitted
              from gaussian. Specify distribution = gamma if gamma simulations
              required")
    }
    for (i in 1:R) {
      # Iterate sample creation for given sample size
      dtNew[, xResponse := ifelse(xTest == "A", rnorm(N, 0, sd), 
                                  rnorm(N, diff.means, sd))]
      # Randomization test p-value
      P[i, 1] <- randomFun(data = dtNew, K = K, colTest = "xTest", 
                           colResponse = "xResponse")
      # Actual t-test p-value
      P[i, 2] <- t.test(H0[1:N, 2], H0[(N + 1):(2 * N), 2])$p.value
    }
  }else if (distribution == "gamma") {
    # If shape or rate not specified, create defaults
    if (is.null(shape) | is.null(rate)) {
      shape <- rep(1, 2)
      rate <- shape
      warning("gamma distribution specified but shape or rate parameters 
              incomplete.\nEnsure both are numeric vectors of length 2, 
              otherwise defaults of shape = 1, rate = 1 used")
    }
    for (i in 1:R) {
      # Potentially think about the efficiency of this- does it 
      # extract the rate and shape values for each observation 
      # assess this via profiling and use of system.time
      dtNew[, xResponse := ifelse(xTest == "A", 
                                  rgamma(N, shape = shape[1], rate = rate[2]), 
                                  rgamma(N, shape = shape[2], rate = rate[2]))]
      x <- pValueFun()
    }
  }else{
    stop("distribution must be either 'gaussian' or 'gamma'\n 
         default is gaussian if no distribution argument set")
  }

  # return size and power of each test
  return(as.data.frame(results(P, R, alpha)))
}

# question- is it quicker to store values as a matrix or a data.table in the above?
