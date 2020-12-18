#' Money Demand Model for Mexico
#' @autor Marco Martinez
#' @format  364 x 5 data frame
#'
#' \describe{
#' \item{index}{Monetary Aggregate}
#' \item{column}
#' }
"MoneyData"

MoneyData = read.csv("MoneyData.csv", header=TRUE)

usethis::use_data(MoneyData, overwrite = TRUE)

