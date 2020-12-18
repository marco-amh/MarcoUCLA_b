#' OLS Main Results
#'
#' @param data_sum
#'
#' @return
#' @export
#'
#' @examples
sum_ols <- function(data_sum){
  #Parameters
  k <- length(data_sum$Results)
  n <- length(data_sum$Fitted)
  res <-data_sum$Residuals
  fitted <- data_sum$Fitted
  Y <- data_sum$y


  # Standard error of the regression
  se_reg <- res^2

  #####
  # ANOVA table
  #####
  SSR <- sum((fitted-mean(Y))^2)
  SSE <- sum(res^2)
  SST <- sum((Y-mean(Y))^2)

  var_SSR <- SSR/(k-1)
  var_SSE <- SSE/(n-k)
  var_SST <- SST/(n-1)

  sd_SSR <- var_SSR^(1/2)
  sd_SSE <- var_SSE^(1/2)
  sd_SST <- var_SST^(1/2)

  #####
  #R^2
  #####
  R2 <- SSR/SST
  R2_adj <- 1-((1-R2)*(n-1)/(n-k))
  Corr_mult <- R2^(1/2)

  #Selection criterion
  log_lik <- (-n/2)*(1+log(2*pi)+log(sum(res^2)/n))
  AIC <- (-2*log_lik)/n+(2*k/n) #Akaike Information Criteria
  Schwarz <- (-2*log_lik)/n+((k*log(n))/n) #Bayesian
  HC <- (-2*log_lik)/n+((2*k*log(log(n))/n)) #Hanna Quinn

  #####
  #Confidence Interval for residuals variance
  #####

  #The variance follows a Chi-Square distribution
  #$X^2_(n-k+1)=frac\(n-k+1)s^2}{\sigma^2}$
  sd_SSE_inf <- (((n-k+1)*var_SSE)/qchisq(0.05,n-k+1))^(1/2)
  sd_SSE_up  <- (((n-k+1)*var_SSE)/qchisq(0.95,n-k+1))^(1/2)

  print(c(sd_SSE_inf,sd_SSE,sd_SSE_up))
  curve(dchisq(x, df = n-k+1), from = 0, to = 20, n = 5000, col= 'blue', lwd=2, add = F)
  p_value <- pchisq(sd_SSE,n-k+1)
  qchisq(p_value,n-k+1)

  ######
  # F test for variances
  #####
  F_calc_var <- var_SSR/var_SSE
  F_calc <- (R2/(k-1))/((1-R2)/(n-k))
  F_crit <- qf(0.95,n-k,k-1)
  F_value <- pf(F_crit,n-k,k-1)
  # Graph
  curve(df(x, df1 = n-k, df2 = k-1), from = 0, to = 10, n = 5000, col= 'blue', lwd=2, add = F,
        main="F-Test ANOVA",sub = "Ho: All parameters equal to zero", xlab="F-Distribution. Compare Two Variances")
  abline(v=F_crit, col="black",lwd=2)
  abline(v=F_calc, col="red",lwd=2)
  legend("topright", c("F critical","F calculated"),fill=c("black","red"),bty='n')

  return(list("F_value"=F_value, "R2"=R2,"R_adj"=R2_adj, "SSR"=SSR, "SSE"=SSE, "SST"=SST,"AIC"=AIC, "BIC"=Schwarz))

  print(data$Results)
  print("The F-test is to compare two variances: the variance of the SSRegression and the SSErrors. If we
        get a high F-calculated value is sign that the y variable is well explained by the model. Hence,
        we set the Null Hypothesis that all coefficients equal to zero, Ho: b1=b2...=bk=0. We aim to
        reject Ho")

}

#model_sum <- sum_ols(data=results_ols)
