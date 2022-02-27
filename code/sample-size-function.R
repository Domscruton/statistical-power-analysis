
# Initialize Environment --------------------------------------------------

pathProj <- paste0("C:/Users/User/Documents/Projects/Data Science Projects/", 
                   "statistical-power-analysis/")
pathCode <- paste0(pathProj, "code/")

# Source functions
cFunSource <- c("results", "randomization", "help")
for (i in cFunSource) {
  source(paste0(pathCode, i, "-function.R"))
}


# Simulation Function -----------------------------------------------------

# Function to assess performance of Two-Sample randomization and t-tests for 
# variations in the distributional properties of a dataset

# Input function for SimulationFun, used to calculate p-values for the 
# Randomization and t-test
pValueFun <- function(dtNew, K, N){
  test <- t.test(dtNew[1:N, 2], dtNew[(N + 1):(2 * N), 2])
  return(c(randomFun(data = dtNew, K = K, colTest = "xTest", 
                     colResponse = "xResponse", test.args = FALSE, 
                     levels = c("A", "B"))$p.value, 
           test$p.value, test$statistic))
}


SimulationFun <- function(N = 200, K = 1000, R = 100, alpha = 0.05, 
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
  PMatrix <- matrix(data = NA, nrow = R, ncol = 2)
  # Create data.table to store simulated datasets
  # (pre-defining size reduces computational burden)
  dtNew <- data.table(xTest = c(rep("A", N), rep("B", N)), 
                      xResponse = NA)
  # Create vector to store t-test statistics
  tTestValue <- c(rep(NA, R))
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
      cResults <- pValueFun(dtNew, K, N)
      PMatrix[i, ] <- cResults[1:2]
      tTestValue[i] <- cResults[3]
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
      dtNew[, xResponse := ifelse(xTest == "A", 
                                  rgamma(N, shape = shape[1], rate = rate[1]), 
                                  rgamma(N, shape = shape[2], rate = rate[2]))]
      cResults <- pValueFun(dtNew, K, N)
      PMatrix[i, ] <- cResults[1:2]
      tTestValue[i] <- cResults[3]
      PMatrix[i, ] <- pValueFun(dtNew, K, N)
    }
  }else{
    stop("distribution must be either 'gaussian' or 'gamma'\n 
         default is gaussian if no distribution argument set")
  }
  # return size and power of each test
  return(list("Results" = resultsFun(PMatrix, R, alpha), 
              "tTestStatistics" = tTestValue))
}

# question- is it quicker to store values as a matrix or a data.table in the above?
