library(jsonlite)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)

station<- function(hdr){
  time<-Sys.time() %>%
    as.POSIXct("Asia/Shanghai")%>%
    format(tz="UTC")%>%
    gsub(pattern=" ",replacement = "T",time)%>%
    paste("Z",sep = "")
  station_requst<- list(
    UTCString=time,
    regionName="全部区域"
  )
  ##Converting the Request body(Dataframe) to Request body(JSON)
  station_requst <- jsonlite::toJSON(station_requst,auto_unbox = TRUE)
  result<-POST("https://power.cnecloud.com/api/v3/pv/monitor/stations",
               body=station_requst,
               content_type_json(),
               add_headers(.headers = hdr))
  output<-content(result)
  
  #读电站列表
  stations <- output$data$stationDataList
  stations <- as.data.frame(do.call(rbind, lapply(stations, as.vector)))
  return(stations)
}
