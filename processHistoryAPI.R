library(dplyr)
library(tidyr)

#处理测点数据
process <- function(output){
  #处理设备数据
  deviceInfo<- data.frame(matrix(unlist(output$data$deviceInfo), nrow=length(output$data$deviceInfo), byrow=TRUE)) 
  deviceInfo <- data.frame(lapply(deviceInfo, as.character), stringsAsFactors=FALSE)
  deviceInfo<- select(deviceInfo,last_col()-1,last_col())
  names(deviceInfo)<-c("devicefullCode","device")
  
  #处理数据时间
  pointTime <- unlist(output$data$pointTime)
  pointTime <- gsub("T|Z","",pointTime)%>%
    as.POSIXct("UTC")%>%
    format(tz="Asia/Shanghai")%>%
    data.frame()
  names(pointTime)<- c("Time")
  for(i in 1:length(deviceInfo)){
    if(i==1){tmp<-pointTime}
    else{  tmp<-rbind(pointTime,tmp)}
  }
  pointTime<-tmp
  
  pointData<-data.frame()
  for(i in 1:length(output$data$pointData)){
    do<- output$data$pointData[[i]]$pointInfo
    do <- as.data.frame(do.call(rbind, lapply(do, as.vector)))
    do[do == "NULL"] <- NA
    do <- as.data.frame(t(do))
    do <- gather(do)
    names(do)<-c("devicefullCode",output$data$pointData[[i]]$pointName)
    if(i==1){pointData<-do}
    else{ pointData<-bind_cols(pointData,select(do,last_col()))}
  }
  pointData<-bind_cols(pointData,pointTime)
  pointData<-left_join(pointData,deviceInfo,by="devicefullCode")
  return(pointData)
}


