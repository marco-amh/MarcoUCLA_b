#' Block Bootstrapping
#'
#' @param k
#' @param B
#' @param data_bb
#'
#' @return
#' @export
#'
#' @examples
block_boots <- function(k, B, data_bb){
  #k <- 12 #size of blocks
  #B <- 100000 #number of bootstraps
  set.seed(81)

  n <- nrow(data_bb) #sample size
  s <- ceiling(n/k) #number of blocks in each bootstraps

  # Matrix to store the results
  boot_coeff <- matrix(NA,B,1)
  # Loop for block bootstrapping
  for(i in 1:B) {
    tmp <- matrix(NA,s*k, ncol(data_bb))
    for(j in 1:s){
      t <- sample(k:n, size=1) #last point of time
      tmp[(j-1)*k+1:k,] <-  data_bb[t-(k:1)+1,] #fill the boots vector with observations in a block
    }
    tmp <- tmp[1:n,]
    boot_coeff[i,] <- cor(tmp[,1],tmp[,2])
    if(i%%1000==0) print(i)
  }

  #----
  # Histogram
  #----

  hist(boot_coeff, xlab="Coefficient", main=paste0("Block Bootstrapped Correlation"),
       breaks="FD",col="gray")
  abline(v=quantile(boot_coeff, 0.5),col="red", lwd=2,lty=1)
  abline(v=quantile(boot_coeff, 0.025),col="red", lwd=2,lty=2)
  abline(v=quantile(boot_coeff, 0.975),col="red", lwd=2, lty=2)
  abline(v=cor(data_bb[,1],data_bb[,2]) ,col="blue", lwd=2, lty=2)

  return(list(mean(boot_coeff),quantile(boot_coeff, 0.5),quantile(boot_coeff, 0.025),quantile(boot_coeff, 0.975)))

}

#data <- cbind(uclanomics::Money_demand[,2],uclanomics::Money_demand[,4])

#block_boots(k=2, B=10000, data_bb=data)
