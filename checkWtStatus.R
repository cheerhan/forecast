library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(psych)

##集团的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjbmVhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDEwLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJjbmVhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLljY_lkIjnrqHnkIblkZgiLCJwaG9uZU51bSI6IjE4NTE0NDY5MjQ0IiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjMxNjYwMzQwMDk5NDgxNiIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjMxNjYwMzQ5MzI2OTUwNCIsImV4cCI6MTkzNjg1MTQ0NCwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzL2RlNTViNzQyYjE0NjQ4MGI4MjQxODBhNjM3MzljYzljLlBORyIsImVudGVycHJpc2VOYW1lIjoi5Y2P5ZCI5paw6IO95rqQIiwianRpIjoiYmUyNmRjNmMtY2RkNC00ZjFkLTljZmQtYjAzNWEyOTUxZjgzIiwidXNlcm5hbWUiOiJjbmVhZG1pbiJ9.GCKqs6GYYCqKjREw9BZyPOtsAbEZ0c1hL75VEA11q5E",      
      'Content-Type'="application/json;charset=UTF-8")

# 使用readStation获得所有风电站
source('~/R/forecast/readWindStation.R')
station<-station(hdr)

startime<-"2021-08-11T16:00:00Z"
endtime<-"2021-08-13T15:59:59Z"
##集团enterpriseid
enterpriseid<-"316603493269504"
#依兰
stationCode<-"334413350"

source('~/R/forecast/readDevice.R')
device<-device(hdr,station=stationCode)
device<-device[22:22,]
#使用readPoint获得设备列表的部分测点
source('~/R/forecast/readPoint.R')
point<-point(hdr,device$deviceId)
#风机状态："TR001","TR015","TR028"
point<-filter(point,devicePointCode %in% c("TR001","FB002","NC001","NC003","NC005"))

#使用readHistory获取测点数据
source('~/R/forecast/readHistory.R')
data<-devicechart(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid)

##查看设备状态数据情况
data$TR001<-as.factor(data$TR001)
summary(data)
tapply(data$TR001, data$deviceCode, summary)
apply(data[,4:7], 2, function(x) tapply(x, data$deviceCode, summary))

# 聚合,基于状态合并时间
state<-data %>%
  group_by(deviceCode) %>%
  mutate(diffTR001 = as.numeric(TR001)- lag(as.numeric(TR001), default = 0))%>%
  filter(diffTR001!=0)%>%
  mutate(end_time = c(Time[-1]-1, NA))%>%
  mutate(period=difftime(end_time,Time,units = "hours"))


View(filter(state,is.na(diffTR001)))
##############draft##########

##导入设备状态配置
library(xlsx)
stateConfig <- read.xlsx("./config/风电机组状态映射.xlsx",sheetIndex=1,header=TRUE)
##合并状态与状态配置
state<-merge(state,stateConfig,by=c("TR001"))

##查看设备状态数据
state$TR001<-as.factor(state$TR001)
tapply(state$TR001, state$deviceCode, summary)

##查看设备状态数据
data$TR001<-as.factor(data$TR001)
tapply(data$TR001, data$deviceCode, summary)


##查看设备状态数据
state$TR015_des<-as.factor(state$TR015_des)
tapply(state$TR015_des, state$deviceCode, summary)

length(data$TR015_des)

