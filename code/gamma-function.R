gammaFun <- function(par1, par2, K, R, N, alpha = 0.05){
  #Function to calculate power and size using different gamma samples
  #Inputs:
  # par1: vector of parameters of the South region gamma simulation
  # par2: vector of parameters of the North East region gamma simulation
  # K: number of randomization samples to calculate
  # R: number of test replications
  # N: sample size to simulate for each region
  #Output:
  # Results: (4x1)-dataframe to store p-values from the tests
  #Create a matrix to store p-values from the tests
  P <- matrix(data = NA, nrow = R, ncol = 4)
  #Extract the parameters for the different distributions
  alpha1 <- par1[1]
  beta1 <- par1[2]
  alpha2 <- par2[1]
  beta2 <- par2[2]
  for (i in 1:R) {
    #Iterate sample creation for given gamma densities for samples of size 1000
    set.seed(i)
    H0 <- rbind(data.frame(region = "South", rate = rgamma(N, alpha1, beta1)), 
                data.frame(region = "Northeast", rate = rgamma(N, alpha1, beta1)))
    H1 <- rbind(data.frame(region = "South", rate = rgamma(N, alpha1, beta1)), 
                data.frame(region = "Northeast", rate = rgamma(N, alpha2, beta2)))
    #Fill the matrix with 4 p-values for each iteration
    P[i, 1] <- randomFun(H0, K)
    P[i, 2] <- randomFun(H1, K)
    P[i, 3] <- t.test(H0[1:N, 2], H0[(N + 1):(2 * N), 2])$p.value
    P[i, 4] <- t.test(H1[1:N, 2], H1[(N + 1):(2 * N), 2])$p.value
  }
  #return the size and power of the tests as a dataframe
  return(as.data.frame(results(P, R, alpha)))
}
