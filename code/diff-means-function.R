diffMeansFun <- function(diff, K, R, alpha = 0.05){
  #Function to calculate power and size for a changing sample mean and 
  #differences in the sample mean
  #Inputs:
  # diff: difference in the mean of each distribution
  # K: number of randomization samples to calculate
  # R: number of test replications
  # alpha: significance level
  #Output:
  # Results: (4x1)-dataframe containing the size and power of the two tests
  
  #Create a matrix to store p-values from the tests
  P <- matrix(data = NA, nrow = R, ncol = 4)
  for (i in 1:R) {
    #Iterate sample creation with different means for samples of size 1000
    set.seed(i)
    H0 <- rbind(data.frame(region = "South", rate = rnorm(1000, 0.01848, 0.1)), 
                data.frame(region = "Northeast", rate = rnorm(1000, 0.01848, 0.1)))
    H1 <- rbind(data.frame(region = "South", rate = rnorm(1000, 0.01848, 0.1)), 
                data.frame(region = "Northeast", rate = rnorm(1000, 0.01848 + diff, 0.1)))
    #Fill the matrix with 4 p-values for each iteration
    P[i, 1] <- randomFun(H0, K)
    P[i, 2] <- randomFun(H1, K)
    P[i, 3] <- t.test(H0[1:1000, 2], H0[(1000 + 1):(2 * 1000), 2])$p.value
    P[i, 4] <- t.test(H1[1:1000, 2], H1[(1000 + 1):(2 * 1000), 2])$p.value
  }
  #Return the size and power of each of the tests as a dataframe
  return(as.data.frame(results(P, R, alpha)))
}
