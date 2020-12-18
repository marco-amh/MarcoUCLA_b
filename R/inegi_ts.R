#' INEGI API
#'
#' @param series_id_inegi
#'
#' @return
#' @export
#'
#' @examples
inegi_ts <- function(series_id_inegi){
  API_key_inegi <- "36c55a30-5b09-f211-164e-06da067a41af"
  URL <- "https://www.inegi.org.mx/app/api/indicadores/desarrolladores/jsonxml/INDICATOR/"
  parameters <- paste(series_id_inegi,"/es/0700/false/BIE/2.0/",API_key_inegi,"?type=jsonp",sep ="")
  PATH <- paste0(URL, parameters)
  query_inegi <- jsonlite::fromJSON(PATH)
  inegi_ts <- query_inegi$Series$OBSERVATIONS[[1]][1:2]
  inegi_ts <- inegi_ts[order(inegi_ts$TIME_PERIOD, decreasing=F),]
  inegi_ts <- data.frame(c(query_inegi$Series$OBSERVATIONS[[1]][,1]),as.numeric(gsub(",","",query_inegi$Series$OBSERVATIONS[[1]][,2])))
  names(inegi_ts)[names(inegi_ts)=="c.query_inegi.Series.OBSERVATIONS..1.....1.."]<-"Date"
  names(inegi_ts)[names(inegi_ts)=="as.numeric.gsub..........query_inegi.Series.OBSERVATIONS..1....."]<-paste("ts", series_id_inegi)
  return(inegi_ts)

}

#ts_inegi <- inegi_ts("444609")
#class(ts_inegi$`ts 444609`)
#plot(ts_inegi$`ts 444609`, type="line")
