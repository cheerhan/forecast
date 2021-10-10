library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(psych)
library(openxlsx)

##导入设备状态配置
stateConfig <- read.xlsx("./config/风电机组状态映射.xlsx")


##集团的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjbmVhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDEwLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJjbmVhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLljY_lkIjnrqHnkIblkZgiLCJwaG9uZU51bSI6IjE4NTE0NDY5MjQ0IiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjMxNjYwMzQwMDk5NDgxNiIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjMxNjYwMzQ5MzI2OTUwNCIsImV4cCI6MTkzNjg1MTQ0NCwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzL2RlNTViNzQyYjE0NjQ4MGI4MjQxODBhNjM3MzljYzljLlBORyIsImVudGVycHJpc2VOYW1lIjoi5Y2P5ZCI5paw6IO95rqQIiwianRpIjoiYmUyNmRjNmMtY2RkNC00ZjFkLTljZmQtYjAzNWEyOTUxZjgzIiwidXNlcm5hbWUiOiJjbmVhZG1pbiJ9.GCKqs6GYYCqKjREw9BZyPOtsAbEZ0c1hL75VEA11q5E",      
      'Content-Type'="application/json;charset=UTF-8")


##集团enterpriseid
enterpriseid<-"316603493269504"

startime<-"2021-08-01T15:59:59Z"
endtime<-"2021-08-02T15:59:59Z"
# endtime<-Sys.time() %>%
#   as.POSIXct("Asia/Shanghai")%>%
#   format(tz="UTC")%>%
#   gsub(pattern=" ",replacement = "T",time)%>%
#   paste("Z",sep = "") #当前时间

source('~/R/forecast/src/readWindStation.R')
all_station<-station(hdr)
# all_station<-all_station%>%arrange(as.numeric(stationUnitCount))
all_station<-filter(all_station,stationName%in%c("肥西")&stationStatus!='900')

stationCode<-all_station[1,]$stationCode

source('~/R/forecast/src/readDevice.R')
all_device<-device(hdr,station=stationCode)

state<-data.frame()
for(i in 1:length(all_device$deviceCode)){
  device<-all_device[i,]
  #使用readPoint获得设备列表的部分测点
  source('~/R/forecast/src/readPoint.R')
  point<-point(hdr,device$deviceId)
  #风机状态："TR001","TR015","TR028",有功功率"TR002"
  point<-filter(point,devicePointCode %in% c("TR001","TR002","YFTR100"))
  tryCatch({ 
    #使用readData获取测点数据
    source('~/R/forecast/src/readData.R')
    data<-devicedata(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval =5)
    data$TR001<-as.numeric(data$TR001)
    data<-merge(data,select(stateConfig,c("TR001","TR001_des")),by=c("TR001"),all.x = TRUE)

    g3<-ggplot(data=data,aes(x=time,y=as.numeric(TR002),colour=as.factor(TR001_des)))+
      geom_line(colour="gray")+
      geom_rug()+
      theme(text = element_text(family = "Noto Sans CJK SC"),
            legend.position="bottom", legend.box = "horizontal")
    
    path<-file.path("checkgen28",paste(stationCode))
    if(!dir.exists(path))
      dir.create(path,recursive = TRUE)
    jpeg(filename = paste0(path,"/",device$deviceName,'.jpeg'),width = 1800,height = 600,units = "px")
    plot(g3)
    dev.off()
    print(paste(i,all_device[i,]$deviceName))
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  # tmp<-data %>%
  #   group_by(deviceName) %>%
  #   mutate(diffTR001 = as.numeric(TR001)- lag(as.numeric(TR001), default = 0))%>%
  #   filter(diffTR001!=0)%>%
  #   arrange(time)%>%
  #   mutate(end_time = c(time[-1]-1,as.POSIXct(gsub(pattern="T",replacement = " ",endtime),tz="UTC")+hours(8)))%>%
  #   mutate(period=difftime(end_time,time,units = "hours"))%>%
  #   select(c("deviceName","time","end_time","period","TR001"))
  # tmp$TR001<-as.numeric(tmp$TR001)
  # tmp<-merge(tmp,select(stateConfig,c("TR001","TR001_des")),by=c("TR001"))
  # state<-bind_rows(state,tmp)
}
# ##############draft##########

##合并状态与状态配置
# state<-merge(state,stateConfig,by=c("TR001"))
# qplot(ymin = time,ymax = end_time,x = deviceName,colour = TR001_des,geom = "linerange",data = state,size = I(5)) +
#   coord_flip() +
#   theme_bw() +
#   theme(panel.grid = element_blank()) +
#   theme(text = element_text(family = "Noto Sans CJK SC"),
#         legend.position="bottom", legend.box = "horizontal")

# ##查看设备状态数据情况
# data$TR001<-as.factor(data$TR001)
# summary(data)
# tapply(data$TR001, data$deviceCode, summary)
# apply(data[,4:7], 2, function(x) tapply(x, data$deviceCode, summary))
# 

# 
# ##查看设备状态数据
# state$TR001<-as.factor(state$TR001)
# tapply(state$TR001, state$deviceCode, summary)
# 
# ##查看设备状态数据
# data$TR001<-as.factor(data$TR001)
# tapply(data$TR001, data$deviceCode, summary)
# 
# 
# ##查看设备状态数据
# state$TR015_des<-as.factor(state$TR015_des)
# tapply(state$TR015_des, state$deviceCode, summary)
# 
# length(data$TR015_des)
# 
