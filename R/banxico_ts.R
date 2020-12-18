#' Banxico API
#'
#' @param series_id_banxico
#' @param initial_date
#' @param final_date
#'
#' @return
#' @export
#'
#' @examples
banxico_ts <- function(series_id_banxico, initial_date, final_date){
  API_key_banxico <- "bafc26d5c3c13e17b465d9550a5f38ca5f888a4dc8c6661ef3658720ba1702a9"
  URL <- "https://www.banxico.org.mx/SieAPIRest/service/v1/series/"
  parameters <- paste(series_id_banxico,"/datos/",initial_date,"/",final_date,"?token=",API_key_banxico,sep ="")
  PATH <- paste0(URL, parameters)
  query_banxico <- jsonlite::fromJSON(PATH)
  ts_mat <- data.frame(c(query_banxico$bmx$series$datos[[1]][,1]),as.numeric(gsub(",","",query_banxico$bmx$series$datos[[1]][,2])))
  ts_names <-paste0(query_banxico$bmx$series$titulo,", ",query_banxico$bmx$series$idSerie)
  names(ts_mat)[names(ts_mat)=="c.query_banxico.bmx.series.datos..1.....1.."]<-"Date"
  names(ts_mat)[names(ts_mat)=="as.numeric.gsub..........query_banxico.bmx.series.datos..1....."]<-ts_names
  return(ts_mat)
}



