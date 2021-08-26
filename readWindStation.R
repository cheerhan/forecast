library(rjson)
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
  output<-content(GET(url=paste("https://power.cnecloud.com/api/v3/wind/monitor/stations",time,sep = "/"),add_headers(hdr)))
  
  #读电站列表
  stations <- output$data$stationDataList
  stations <- as.data.frame(do.call(rbind, lapply(stations, as.vector)))
  return(stations)
}
