library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(psych)

#确定一个电站某一天发电量

##普洛斯的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJwbHNhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDIyLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJwbHNhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLmma7mtJvmlq_nrqHnkIblkZgiLCJwaG9uZU51bSI6IjE1NjExNDA5NTYyIiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjQ1MTQzNjQ2MzY5MjI4OCIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjQ1MTQzNjQ2Nzg4NjU5MiIsImV4cCI6MTkzMTY1NDIzNSwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzLzBjMTM3OTczMDI1MDQ2ODY4OGM5M2ZmZmEyZTg3ODU1LmpwZyIsImVudGVycHJpc2VOYW1lIjoi5pmu5p6r5paw6IO95rqQIiwianRpIjoiY2IyMGVlM2YtMmI3NS00MjE0LWE4ZjYtNjdkODIzYWE0M2VkIiwidXNlcm5hbWUiOiJwbHNhZG1pbiJ9.qLK9ocTltrvrd8ameGhd8Sc_hLvAvhbjy8vKmmKdtVA",      'Content-Type'="application/json;charset=UTF-8")
startime<-"2021-02-28T16:00:00Z"
endtime<-"2021-03-01T15:59:59Z"
enterpriseid<-"451436467886592"
points<-c("NB018","NB031","NB034","QX002","QX012")

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

#求所有设备nb031的一天最大值
sum_31<- data%>% group_by(devicefullCode)%>%summarise(sum_31=max(NB031,na.rm = TRUE))
#根据电站聚合电站一天发电量。
sum_station<- sum_31%>%separate(devicefullCode,into=c("stationCode","deviceType","deviceMode","deviceNo"),sep="M")
sum_station<-sum_station%>%group_by(stationCode)%>%summarise(sum_31=sum(sum_31))
