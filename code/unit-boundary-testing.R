# Script for Unit Testing of Functions


# Initialize Environment --------------------------------------------------

cFunSource <- c("results", "sample-size", "randomization", "help")
pathProj <- paste0("C:/Users/User/Documents/Projects/Data Science Projects/", 
                   "statistical-power-analysis/")
pathCode <- paste0(pathProj, "code/")
for (i in cFunSource) {
  source(paste0(pathCode, i, "-function.R"))
}

cRandSeed <- 345

library(data.table)


# Generate Sample Data ----------------------------------------------------

SimulationFun(N = 200, K = 20)


# Unit Testing ------------------------------------------------------------

# Code Coverage -----------------------------------------------------------

# execute every line of our code with representative data for which we know
# the true answer to check whether the code functions correctly

# 1) randomFun
# Simulate data from Normal distribution
set.seed(cRandSeed)
dtGaussian <- data.table(Test = c(rep("A", 100), rep("B", 100)), 
                             Response = c(rnorm(100), rnorm(100, mean = 0.2)))
# calculate Two-Sample t-test
t.test(dtGaussian[1:100, 2], dtGaussian[101:200, 2])$p.value # 0.1497
# Calculate Two-Sample Randomization and t-test using RandomizationFun
randomFun(dtGaussian, K = 100000, colTest = "Test", 
          colResponse = "Response") # 0.15028

# 2) SimulationFun
# Simulate data from gamma distribution
shape1 <- 1
shape2 <- 3
rate1 <- 2
rate2 <- 0.5
dtGamma <- data.table(Test = c(rep("A", 100), rep("B", 100)), 
                      Response = c(rgamma(100, shape1, rate1), 
                                   rgamma(100, shape2, rate2)))
t.test(dtGamma[1:100, 2], dtGamma[101:200, 2])$p.value
SimulationFun(K = 100, R = 100, distribution = "gamma", 
              shape = c(shape1, shape2), rate = c(rate1, rate2))


# Data Coverage -----------------------------------------------------------

# Ensure program handles data and data errors correctly. That is we test
# the Boundaries of the function and its arguments to ensure it is robust
# to misuse and any other potential issues

# 1) HelpFun
# Returns information for SimulationFun
HelpFun(SimulationFun)

# Returns error since input argument is not valid
HelpFun(Hello)
HelpFun("SimulationFun")

