#' Jarque Bera Test for Residuals
#'
#' @param model
#' @param cl
#'
#' @return
#' @export
#'
#' @examples
ols_residuals <- function(model, cl){
  res <- model$Residuals
  n <- length(model$Residuals)

  par(mfcol=c(1,1))
  #Plot residuals
  plot(res, type="line", main="Residuals")

  #Histogram
  xr <- rnorm(5000000,mean(res),sd(res))
  hist(res,breaks ="FD",col="skyblue2", freq = FALSE, ylab = "Density",
       main = "Histogram of the Residuals")
  lines(density(res),lwd = 2, col ="red")
  lines(density(xr),col="black",lwd=2)
  legend("topleft",c("Histogram","Density","Theoretical ND"),fill=c("skyblue2","red","black"),bty='n')

  #QQ Plot
  qqnorm(res,col="skyblue4", pch=20,lwd=1,main="QQ Normal Plot")

  #Jarque-Bera test for normality
  # Skewness
  skew_1_a <- (res-mean(res))^3
  skew_2_a <- (sum(skew_1_a))/n

  skew_1_b <- (res-mean(res))^2
  skew_2_b <- (sum(skew_1_b))/n
  skew_3_b <- skew_2_b^(3/2)

  skew_res <- skew_2_a/skew_3_b

  # Kurtosis
  kurt_1_a <- (res-mean(res))^4
  kurt_2_a <- (sum(kurt_1_a))/n

  kurt_1_b <- (res-mean(res))^2
  kurt_2_b <- (sum(kurt_1_b))/n
  kurt_3_b <- kurt_2_b^(2)

  kurt_res <- kurt_2_a/kurt_3_b

  JB_calc_quantile <- (n/6)*((skew_res^2)+(1/4)*(kurt_res-3)^(2))
  p_value_JB <- 1-pchisq(JB_calc_quantile,2)
  JB_chi_critical <- qchisq(cl,2)

  curve(dchisq(x, df = 2), from = 0, to = 15, n = 100000, col= 'blue', lwd=2, add = F,
        main="Jarque-Bera Test for Normality",sub = "Ho: Residuals are normally distributed",
        xlab="Chi Square Distribution")
  abline(v=qchisq(0.95,2), col="black",lwd=2)
  abline(v=JB_calc_quantile, col="red",lwd=2)
  legend("topright", c("Chi critical","Chi calculated"),fill=c("black","red"),bty='n')

  print("The Null hypothesis is that the residuals are normally distributed. The alternative hypothesis
  is that the residuals do not follow a normal distribution. We fail to reject Ho if p-value > 0.05")

  return(list("JB_calculated"=JB_calc_quantile, "JB_critical"= JB_chi_critical, "p_value"=p_value_JB))

}

#ols_residuals(results_ols, 0.95)
