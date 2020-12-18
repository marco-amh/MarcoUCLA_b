#' Variance Inflation Factor
#'
#' @param data_vif
#'
#' @return
#' @export
#'
#' @examples
vif_mc<- function(data_vif){

  t_y <- matrix(rep(seq(1,length(data_vif)),times=length(data_vif)),length(data_vif),1)
  t <- t(matrix(rep(seq(1,length(data_vif)),times=length(data_vif)),length(data_vif),length(data_vif)))

  t_x <- matrix(NA,length(data_vif),(length(data_vif))-1)
  for (i in 1:length(data_vif)){
    t_x[i,] <- t[i,-i]
  }
  order_var <- cbind(t_y,t_x)

  r2 <- matrix(NA,length(data_vif),1)

  for (i in 1:length(data_vif)){
    model_2 <- ols(data_ols = data_vif[,c(order_var[i,])])
    model_1 <- sum_ols(model_2)
    r2[i,] <- 1/(1-model_1$R2)
  }

  colnames(r2)<- c("VIF")
  rownames(r2)[1:nrow(r2)] <- names(data_vif)[1:ncol(data_vif)]

  print("VIF=1 Predictors No Correlated")
  print("1<VIF<5 Predictors Moderately Correlated")
  print("VIF>5-10 Predictors Correlated")

  return(r2)
}

#vif_mc(data_vif=uclanomics::Money_demand[3:6])



#model_test_vif <- lm(data_test[,1]~data_test[,2]+data_test[,3]+data_test[,4]+data_test[,5])
#car::vif(model_test_vif)
