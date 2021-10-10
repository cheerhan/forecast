library(jsonlite)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)

devicedata<- function(hdr,stationcode,devicefullcode,startime,endtime,devicepoints,enterpriseid="316603493269504",timeInterval=10){
  #创建request电站某天的逆变器的历史数据。
  request_init <- list(
    stationCode=as.character(stationcode),
    deviceFullCodes=as.vector(unlist(devicefullcode, use.names = FALSE)),
    startTime=startime,
    endTime=endtime,
    devicePoints=as.vector(devicepoints),
    #5为5秒，2为1分钟，10为10分钟
    timeInterval=timeInterval,
    orderField="deviceName",
    orderType="1",
    pageNum="1",
    pageSize="1",
    enterpriseId=enterpriseid
  )
  
  ##Converting the Request body(Dataframe) to Request body(JSON)
  request_body_json <- jsonlite::toJSON(request_init,auto_unbox = TRUE)
  result<-POST("https://power.cnecloud.com/api/v3/wind/analysis/history/devicedata",
               body=request_body_json,
               content_type_json(),
               add_headers(.headers = hdr))
  output<-content(result)
  request_init$pageSize<-output[["data"]][["totalCount"]]
  
  request_body_json <- jsonlite::toJSON(request_init,auto_unbox = TRUE)
  result<-POST("https://power.cnecloud.com/api/v3/wind/analysis/history/devicedata",
               body=request_body_json,
               content_type_json(),
               add_headers(.headers = hdr))
  output<-content(result)
  data <- data.frame()
  
  for(i in 1:length(output$data$dataList)){
    do<- output$data$dataList[[i]]$pointData
    do <- as.data.frame(do.call(rbind, lapply(do, as.vector))) #list to dataframe
    do[do=="NULL"] <- NA
    do<-bind_cols(output$data$dataList[[i]][-1],do)
    data<-rbind(data,do)
  }
  data<-data%>% select("deviceModeName","deviceTypeName","stationName","time","deviceName","pointValue","devicePointCode") %>% spread(devicePointCode, pointValue)
  data<-unnest(data)
  data$time<-gsub("T|Z","",data$time)%>%
    as.POSIXct("UTC")%>%
    format(tz="Asia/Shanghai")%>%
    as.POSIXct("UTC")
  return(data)
}
