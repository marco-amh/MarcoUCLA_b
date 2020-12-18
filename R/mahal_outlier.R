#' Mahalanobis Distance Outlier Detection
#'
#' @param data
#' @param p_val_chi
#'
#' @return
#' @export
#'
#' @examples
mahal_outlier <- function (data, p_val_chi=0.001){

  #Detecting Outliers Mahalanobis Distance framework
  data_mahal <-mahalanobis(data,colMeans(data),cov(data))
  data_mahal_p <- pchisq(data_mahal, df=length(data)-1, lower.tail=FALSE)
  p_values_id <- as.data.frame(cbind(seq(length(data_mahal_p)),data_mahal_p))
  data_mahal_ordered <- p_values_id[order(p_values_id$data_mahal_p, decreasing=FALSE),]
  data_outlier <- data_mahal_ordered[which(data_mahal_ordered$data_mahal_p<p_val_chi),]
  print(data_outlier)
  data_clean <- data[-data_outlier$V1, ]
  return(list(data_outlier, data_clean))
}

