
# randomFunTest -----------------------------------------------------------

# Function to carry out testing of randomFun arguments (improves readability)

randomFunTest <- function(data, K, colTest, colResponse, levels = NULL){
  # Check arguments
  # 1) data
  if (!is.data.frame(data) & !is.data.table(data)) {
    stop("data must be of class data.frame or data.table")
  }else if (length(sapply(data, is.numeric)) < 2) {
    stop("Randomization test requires at least 2 numeric columns in input 
         dataset")
  }else if (length(sapply(data, is.numeric)) > 2 & is.null(vars)) {
    warning("More than 2 columns provided and column names not specified. 
            First two numeric columns used. Please specify required columns if 
            these are incorrect")
  }
  # 2) K
  if (!is.numeric(K)) {
    stop("K (number of permutations) must be of type numeric or integer")
  }else if (K <= 0 | K %% 1 != 0) {
    stop("K must be a positive integer")
  }
  # 3) ColTest and ColVars
  for (i in c("colTest", "colResponse")) {
    if (!is.null(eval(parse(text = i)))) {
      if (is.character(eval(parse(text = i))) & 
          !(eval(parse(text = i)) %in% colnames(data))) {
        stop("input data.table does not contain specified columns")
      }
    }
  }
  # 4) Levels
  if (!is.null(levels)) {
    if (!(levels %in% data[, get(colTest)])) {
      stop("Provided levels not found in Test column")
    }
  }else{
    warning("Since no levels provided 1st 2 levels identified used in test")
  }
  if (!is.null(levels) & length(unique(levels)) != 2) {
    warning("Randomization test requires 2 levels in levels argument")
  }else if (length(unique(data[, get(colTest)])) < 2){
    stop("ColTest requires at least 2 unique levels")
  }
}
  
  
# randomFun ---------------------------------------------------------------

randomFun <- function(data, K = 1000, colTest, colResponse, 
                      levels = NULL){
  # Function to calculate randomized test statistics
  # Inputs:
    # data: combined dataset for the two samples
    # K: number of randomization samples to carry out
    # colTest: character string specifying testing column
    # colResponse: character string specifying response column
    # levels: optional character vector of 2 levels of colTest for comparison
            # if not specified performs two-sample t-test across all levels
  # Output:
    # p-value from empirical t-distribution

  # Check arguments
  randomFunTest()
  
  # Convert dataframe to data.table (data.table more efficient)
  data <- as.data.table(data)
  # specify variables and levels if unspecified
  for (i in c("colTest", "colResponse")) {
    if (is.null(eval(parse(i)))) {
      parse(i) <- 
    }
  }
  
  # Calculate number of observations within each level
  n1 <- length(which(data$region == "South"))
  n2 <- length(which(data$region == "Northeast"))
  # Initialize storage vector for test statistics
  Trand <- rep(NA, K)
  for (k in 1:K) {
    # Create randomised samples by permuting the data
    indices <- sample(1:(n1 + n2))
    dtperm <- data[indices, rate]
    # Calculate t-statistic for each permutation
    Trand[k] <- abs(mean(perm[1:n1]) - mean(perm[(n1 + 1):(n1 + n2)]))
  }
  # Calculate t-statistic for original data
  Tobs <- abs(mean(data$rate[1:n1]) - mean(data$rate[(n1 + 1):(n1 + n2)]))
  # p-value
  p <- length(which(Trand > Tobs)) / K
  return(p)
  
  # Adding MetaData explaining function
  attr(randomFun, "help") <- 
    "randomFun carries out a Two-Sample Randomization test, using one numeric 
    column and one categorical or numeric column consisting of two levels for 
    comparison"
}