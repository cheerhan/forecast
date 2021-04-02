library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(psych)

##普洛斯的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJwbHNhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDIyLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJwbHNhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLmma7mtJvmlq_nrqHnkIblkZgiLCJwaG9uZU51bSI6IjE1NjExNDA5NTYyIiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjQ1MTQzNjQ2MzY5MjI4OCIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjQ1MTQzNjQ2Nzg4NjU5MiIsImV4cCI6MTkzMTY1NDIzNSwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzLzBjMTM3OTczMDI1MDQ2ODY4OGM5M2ZmZmEyZTg3ODU1LmpwZyIsImVudGVycHJpc2VOYW1lIjoi5pmu5p6r5paw6IO95rqQIiwianRpIjoiY2IyMGVlM2YtMmI3NS00MjE0LWE4ZjYtNjdkODIzYWE0M2VkIiwidXNlcm5hbWUiOiJwbHNhZG1pbiJ9.qLK9ocTltrvrd8ameGhd8Sc_hLvAvhbjy8vKmmKdtVA",      'Content-Type'="application/json;charset=UTF-8")
startime<-"2021-02-28T16:00:00Z"
endtime<-"2021-03-31T15:59:59Z"
enterpriseid<-"451436467886592"
points<-c("NB031","NB034")

# 使用readStation获得所有电站
source('~/R/forecast/readStation.R')
station<-station(hdr)

data<-data.frame()
for(i in 1:length(station$stationCode)){
  stationCode<-station$stationCode[i]
  print(station$stationName[i])
  
  source('~/R/forecast/readDevice.R')
  device<-device(hdr,station=stationCode)
  
  #使用readPoint获得设备列表的部分测点
  source('~/R/forecast/readPoint.R')
  point<-point(hdr,device$deviceId)
  point<-filter(point,devicePointCode %in% points)
  #使用readHistory获取测点数据
  source('~/R/forecast/readHistory.R')
  tmp<-devicechart(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId)
  data<-bind_rows(data,tmp)
}

#求所有设备一天中nb031的最大值，NB034的最小值，NB034的最大值-最小值
daily_device<-data%>% group_by(deviceCode,time=as.Date(Time))%>%summarise(deviceName=first(deviceName),max_31=max(NB031),min_34=min(NB034,na.rm = TRUE),sub_34=max(NB034,na.rm = TRUE)-min(NB034,na.rm = TRUE))
daily_device[sapply(daily_device, is.infinite)] <- NA
daily_device<-daily_device%>%separate(deviceCode,into=c("stationCode","deviceType","deviceMode","deviceNo"),sep="M")
daily_device<-merge(daily_device,station,by="stationCode")
#设备日发电量
daily_device<-select(daily_device,c("stationCode","stationName","stationCapacity","deviceName","time","max_31","min_34","sub_34"))
#设备总发电量
sum_device<-daily_device%>%group_by(stationCode,deviceName)%>%summarise(stationCode=first(stationCode),
                                                                       stationName=first(stationName),
                                                                       stationCapacity=first(stationCapacity),
                                                                       max_31=sum(max_31,na.rm=TRUE),
                                                                       sub_34=sum(sub_34,na.rm=TRUE))
sum_device<-mutate(sum_device,diff3134=abs(sub_34-max_31))
sum_device<-as.data.frame(sum_device)
#电站总发电量
sum_station<-sum_device%>%group_by(stationCode)%>%summarise(stationCode=first(stationCode),
                                                            stationName=first(stationName),
                                                            stationCapacity=first(stationCapacity),
                                                            max_31=sum(max_31,na.rm=TRUE),
                                                            sub_34=sum(sub_34,na.rm=TRUE))
sum_station<-mutate(sum_station,diff3134=abs(sub_34-max_31))
sum_station<-as.data.frame(sum_station)
                                                          

library(xlsx)
options(scipen = 999)
write.xlsx(sum_station, "普洛斯所有电站3月电量.xlsx", sheetName = "Sheet1",  col.names = TRUE, row.names = FALSE)
write.xlsx(sum_device, "普洛斯所有设备3月电量.xlsx", sheetName = "Sheet2",  col.names = TRUE, row.names = FALSE, append = FALSE)
