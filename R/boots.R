#' Bootstrapping
#' @param B
#' @param data
#'
#' @return
#' @export
#'
#' @examples
boots <- function(B, data){

  set.seed(81)
  boot_coeff <- matrix(NA,B,1)

  for (i in 1:B){
    data_boot <- data[sample(nrow(data), nrow(data), replace=T),  ]

    boot_coeff[i,] <- cor(data_boot[,1],data_boot[,2])

  }

  hist(boot_coeff, xlab="Coefficient", main=paste0("Bootstrapped Correlation"),
       breaks="FD",col="gray")
  abline(v=quantile(boot_coeff, 0.5),col="red", lwd=2,lty=1)
  abline(v=quantile(boot_coeff, 0.025),col="red", lwd=2,lty=2)
  abline(v=quantile(boot_coeff, 0.975),col="red", lwd=2, lty=2)
  abline(v=cor(data[,1],data[,2]) ,col="blue", lwd=2, lty=2)

}


#boots(B=100000, data=data)
