library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)

devicechart<- function(hdr,stationcode,devicefullcode,startime,endtime,devicepoints,enterpriseid="316603493269504"){
  #创建request电站某天的逆变器的历史数据。
  request_body <- list(
    stationCode=stationcode,
    deviceFullCodes=as.vector(devicefullcode),
    startTime=startime,
    endTime=endtime,
    devicePoints=as.vector(devicepoints),
    #5为5秒，2为1分钟，10为10分钟
    timeInterval="5",
    devicePoint="[]",
    enterpriseId=enterpriseid
  )
  
  ##Converting the Request body(Dataframe) to Request body(JSON)
  request_body_json <- toJSON(request_body)
  result<-POST("https://power.cnecloud.com/api/v3/wind/analysis/history/devicechart",
               body=request_body_json,
               content_type_json(),
               add_headers(.headers = hdr))
  output<-content(result)
  
  ##处理逆变器设备列表
  deviceInfo <- output$data$deviceInfo
  deviceInfo <- as.data.frame(do.call(rbind, lapply(deviceInfo, as.vector)))
  deviceInfo <- filter(deviceInfo,deviceInfo$deviceCode %in% device$deviceCode)
  deviceInfo <- select(deviceInfo,2:3)
  names(deviceInfo)<-c("deviceCode","deviceName")
  deviceInfo$deviceCode<-as.character(deviceInfo$deviceCode)
  if(length(output$data$pointTime)==0){
    pointData<-deviceInfo
    pointData<-unnest(pointData,cols = (deviceName))
    return(pointData)
  }
  #处理数据时间
  pointTime <- unlist(output$data$pointTime)
  pointTime <- gsub("T|Z","",pointTime)%>%
    as.POSIXct("UTC")%>%
    format(tz="Asia/Shanghai")%>%
    as.data.frame()
  # pointTime <- as.data.frame(pointTime[rep(seq_len(nrow(pointTime)), each = nrow(device)), ])
  pointTime<-do.call("rbind", replicate(nrow(device), pointTime, simplify = FALSE))
  names(pointTime)<- c("Time")
  
  #处理测点数据
  nbData<-data.frame()
  qxData<-data.frame()
  for(i in 1:length(output$data$pointData)){
    do<- output$data$pointData[[i]]$pointInfo
    do <- as.data.frame(do.call(rbind, lapply(do, as.vector)))
    do[do=="NULL"] <- NA
    
    #如果是气象数据需要复制设备数个
    if(grepl("QX",output$data$pointData[[i]]$pointCode)){
      do <- do[rep(seq_len(nrow(do)), each = nrow(device)), ]
      do <- as.data.frame(t(do))
      do <- gather(do)
      names(do)<-c("deviceCode",output$data$pointData[[i]]$pointCode)
      if(length(qxData)==0){qxData<-do}
      else{ qxData<-bind_cols(qxData,select(do,last_col()))}
    }
    #如果是逆变器数据
    else {
      do <- as.data.frame(t(do))
      do <- gather(do)
      names(do)<-c("deviceCode",output$data$pointData[[i]]$pointCode)
      if(length(nbData)==0){nbData<-do}
      else{ nbData<-bind_cols(nbData,select(do,last_col()))}
    }
  }
  if(ncol(qxData)>2){pointData<-bind_cols(nbData,select(qxData,2:ncol(qxData)))}
  else{pointData<-nbData}
  pointData<-bind_cols(pointData,pointTime)
  pointData<-left_join(deviceInfo,pointData)
  
  pointData<-unnest(pointData)
  pointData<-pointData%>%relocate(Time,.after = deviceName)
  
  pointData$deviceCode<-as.factor(pointData$deviceCode)
  pointData$deviceName<-as.factor(pointData$deviceName)
  pointData$Time<-as_datetime(pointData$Time)
  pointData[, 4:ncol(pointData)] <- sapply( pointData[, 4:ncol(pointData)], as.numeric)
  return(pointData)
}

