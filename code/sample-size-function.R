sampSizeFun <- function(N, K, R, alpha = 0.05){
  #Function to calculate size and power varying sample sizes
  #Inputs:
  # N: sample size to simulate for each region
  # K: number of randomization samples to calculate
  # R: number of test replications
  # alpha: significance level
  #Output:
  # Results: (4x1)- dataframe containing the size and power of the two tests
  
  #Create a matrix to store p-values from the tests
  P <- matrix(data = NA, nrow = R, ncol = 4)
  for (i in 1:R) {
    #Iterate sample creation for given sample size
    set.seed(i)
    H0 <- rbind(data.frame(region = "South", rate = rnorm(N, 0.04417, 0.1)), 
                data.frame(region = "Northeast", rate = rnorm(N, 0.04417, 0.1)))
    H1 <- rbind(data.frame(region = "South", rate = rnorm(N, 0.04417, 0.1)), 
                data.frame(region = "Northeast", rate = rnorm(N, 0.01848, 0.1)))
    #Fill the matrix with 4 p-values for each iteration
    P[i, 1] <- randomFun(H0, K)
    P[i, 2] <- randomFun(H1, K)
    P[i, 3] <- t.test(H0[1:N, 2], H0[(N + 1):(2 * N), 2])$p.value
    P[i, 4] <- t.test(H1[1:N, 2], H1[(N + 1):(2 * N), 2])$p.value
  }
  #return the size and power for each test as a dataframe
  return(as.data.frame(results(P, R, alpha)))
}
