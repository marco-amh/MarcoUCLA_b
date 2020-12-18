#' OLS with White Robust Standard Errors
#'
#' @param data_ols
#'
#' @return
#' @export
#'
#' @examples
ols_se_white <-function(data_ols){

  #Create matrices for variables
  Y <- matrix(0,nrow(data_ols),1)
  X <- matrix(0,nrow(data_ols),ncol(data_ols))

  #Define dependent variable
  Y <- data_ols[,1]

  #Vector of constants
  X[,1] <- 1

  #Fill the matrix with independent variables
  for (i in 2:ncol(data_ols)){
    X[,i] <- data_ols[,i]
  }
  colnames(X)[2:ncol(data_ols)] <- names(data_ols)[2:ncol(data_ols)]
  colnames(X)[1] <- c("intercept")

  #Number of observations
  n <- nrow(X)
  #Number of parameters
  k <- ncol(X)

  #####
  #OLS matrix form
  #####
  #\hat \y=(X'X)^{-1}X'Y

  #Solving the matrix
  coef <-t(solve(t(X)%*%X)%*%t(X)%*%Y)
  rownames(coef) <- c("Coefficients") #Rename

  #Vector to store y_hat
  Y_hat <- matrix(NA,n,k)

  #Loop for y_hat
  for (i in 1:k){
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
    Y_hat[,i] <- (X[,i]*coef[,i])
  }

  # Create time
  #t <- seq(from=start, to=end, by=factor)

  ######
  # Calculate Y_hat for time/individual
  #####
  Y_hat <- apply(Y_hat, MARGIN = 1, sum)

  #####
  # Plot y_obs, y_hat and residuals
  #####

  # Fitted values
  par(mfcol=c(1,2))
  plot(Y_hat, type="line", main="Fitted vs Observed values", col="red")
  lines(data_ols[,1], type="line", col="black")
  legend("topleft",c("Fitted","Observed"),fill=c("red","black"),bty='n')

  #Residuals
  res <- Y-Y_hat
  plot(res, type="line", main="Residuals")

  #####
  # Variance covariance matrix
  #####

  #Var(\hat \beta)|X)=1-(n-k) \hat \epsilon ' \hat \epsilon(X'X)^{-1}
  VCV <- (1/(n-k))*as.numeric(t(res)%*%res)*solve(t(X)%*%X)

  #Diagonal matrix variance-covariance
  se <- t(as.matrix(sqrt(diag(VCV))))
  rownames(se) <- c("s.e.") #rename

  #####
  # t-values
  #####

  #Robust Variance covariance matrix
  white_var <- (diag(VCV))*(n/(n-k-1))

  #Robust White S.E.
  white_se <-(white_var)

  ## t-value with White s.e.

  white_t_value <- coef/white_se
  rownames(white_t_value) <- c("t-value") #rename

  # Bind main results
  results <-rbind(coef, white_se, white_t_value)
  list_results <- list("Results" = results, "Residuals" = res, "Fitted" = Y_hat, "y"=Y)

  return(list_results)
}

#results_ols <- ols_se_white(data_ols=uclanomics::Money_demand[2:5])
#results_ols$Results

