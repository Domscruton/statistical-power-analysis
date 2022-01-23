
# randomFunTest -----------------------------------------------------------

# Function to carry out testing of randomFun arguments (improves readability)

randomFunTest <- function(data, K, colTest, colResponse, levels = NULL){
  # Check arguments
  # 1) data
  if (!is.data.table(data) & !is.data.frame(data)) {
    stop("data must be of class data.frame or data.table")
  }else if (sum(sapply(data, is.numeric)) < 1) {
    stop("Randomization test requires at least 1 numeric columns in input 
         dataset")
  }else if (dim(data)[2] > 2) {
    warning("More than 2 columns provided and column names not specified. 
            First two columns used as Test and Response columns. 
            \nPlease specify required columns if these are incorrect")
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
    if (any(!(levels %in% data[, get(colTest)]))) {
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
                      levels = NULL, test.args = TRUE){
  # Function to calculate randomized test statistics
  # Inputs:
    # data: combined dataset for the two samples
    # K: number of randomization samples to carry out
    # colTest: character string specifying testing column
    # colResponse: character string specifying response column
    # levels: optional character vector of 2 levels of colTest for comparison
            # if not specified performs two-sample t-test for first 2 levels
    # test.args: should arguments be tested for compatability?
  # Output:
    # p-value from empirical t-distribution

  # Check arguments
  if (test.args) {
    randomFunTest(data, K, colTest, colResponse, levels)
  }
  
  # Convert dataframe to data.table (data.table more efficient)
  dtdata <- as.data.table(x = data)
  # specify levels if unspecified
  if (is.null(levels)) {
    levels <- unique(dtdata[, get(colTest)])[1:2]
  }
  
  # Calculate number of observations within each level
  cn1 <- dim(dtdata[get(colTest) %in% levels[1], ])[1]
  cn2 <- dim(dtdata[get(colTest) %in% levels[2], ])[1]
  # Initialize storage vector for test statistics
  Trand <- rep(NA, K)
  for (k in 1:K) {
    # Create randomised samples by permuting the data
    indices <- sample(1:(cn1 + cn2))
    # get() collapses datatable to vector
    dtperm <- dtdata[indices, get(colResponse)]
    # Calculate t-statistic for each permutation
    Trand[k] <- abs(mean(dtperm[1:cn1]) - mean(dtperm[(cn1 + 1):(cn1 + cn2)]))
  }
  # Calculate t-statistic for original data
  Tobs <- abs(mean(dtdata[1:cn1, get(colResponse)]) - 
                mean(dtdata[(cn1 + 1):(cn1 + cn2), get(colResponse)]))
  # p-value
  p <- length(which(Trand > Tobs)) / K
  return(p)
  
  # Adding MetaData explaining function
  attr(randomFun, "help") <- 
    "randomFun carries out a Two-Sample Randomization test, using one numeric 
    column and one categorical or numeric column consisting of two levels for 
    comparison"
}
