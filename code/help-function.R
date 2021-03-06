
# HelpFun -----------------------------------------------------------------

# Helper function to provide information on the functions: 
# 1) RandomFun
# 2) RandomFunTest
# 3) SimulationFun
# 4) pValueFun
# 5) ResultsFun


# HelpFun -----------------------------------------------------------------

HelpFun <- function(f) {
  
  # If input function not one of above functions, stop execution
  if (!identical(f, randomFun) & !identical(f, randomFunTest) &
      !identical(f, SimulationFun) & !identical(f, pValueFun) & 
      !identical(f, resultsFun) ) {
    stop("invalid argument")
  }
  else if (identical(f, randomFun)){
    out <- writeLines("randomFun performs the randomization test by shuffling
          the response column K times to calculate the t-test
          statistic under the null hypothesis that the two
          levels in the Test column have equal response values")
  }
  else if (identical(f, randomFunTest)){
    out <- writeLines("randomFunTest is used within randomFun to test for 
          compliance of input arguments")
  }
  else if (identical(f, SimulationFun)){
    out <- writeLines("SimulationFun performs the randomization test but for
          simulated data where we can alter the arguments to change
          the distributional properties of the simulated data. This
          can be used to assess the robustness of the t-test and
          randomization-test for changes in the effect size,
          standard deviation and distributional properties of the
          data")
  }
  else if (identical(f, pValueFun)){
    out <- writeLines("pValueFun is used within SimulationFun to extract
          p-values for the t-test and randomization test")
  }
  else if (identical(f, resultsFun)){
    out <- writeLines("resultsFun is used to calculate and print the final
          results, namely the size and power of the randomization
          and t-tests")
  }
}
