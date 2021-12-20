randomFun <- function(data, K){
  #Function to calculate randomized test statistics
  #Inputs:
  # data: combined dataset for the two samples
  # K: number of randomization samples to carry out
  #Output:
  # p-value from the empirical t-distribution
  
  #Calculate the number of observations within each region
  n1 <- length(which(data$region == "South"))
  n2 <- length(which(data$region == "Northeast"))
  #Initialize vector of test statistics
  Trand <- rep(NA, K)
  #set seed for reproducibility
  set.seed(160001695)
  for (k in 1:K) {
    #Create randomised samples by permuting the data
    indices <- sample(1:(n1 + n2))
    perm <- data$rate[indices]
    #Calculate a t-statistic for each permutation
    Trand[k] <- abs(mean(perm[1:n1]) - mean(perm[(n1 + 1):(n1 + n2)]))
  }
  #Calculate the t-statistic for the original data
  Tobs <- abs(mean(data$rate[1:n1]) - mean(data$rate[(n1 + 1):(n1 + n2)]))
  #Calculate the p-value
  p <- length(which(Trand > Tobs)) / K
  return(p)
}
