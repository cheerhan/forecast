library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(psych)

library(ggplot2)
library(grid)
library(gridExtra)
##普洛斯的config：header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJwbHNhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDIyLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJwbHNhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLmma7mtJvmlq_nrqHnkIblkZgiLCJwaG9uZU51bSI6IjE1NjExNDA5NTYyIiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjQ1MTQzNjQ2MzY5MjI4OCIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjQ1MTQzNjQ2Nzg4NjU5MiIsImV4cCI6MTkzMTY1NDIzNSwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzLzBjMTM3OTczMDI1MDQ2ODY4OGM5M2ZmZmEyZTg3ODU1LmpwZyIsImVudGVycHJpc2VOYW1lIjoi5pmu5p6r5paw6IO95rqQIiwianRpIjoiY2IyMGVlM2YtMmI3NS00MjE0LWE4ZjYtNjdkODIzYWE0M2VkIiwidXNlcm5hbWUiOiJwbHNhZG1pbiJ9.qLK9ocTltrvrd8ameGhd8Sc_hLvAvhbjy8vKmmKdtVA",      
      'Content-Type'="application/json;charset=UTF-8")

##普洛斯enterpriseid
enterpriseid<-"451436467886592"

source('~/R/forecast/readPvStation.R')
all_station<-station(hdr)
all_station<-filter(all_station,stationName%in%c("佛山顺德二期"))

startime<-"2021-09-05T16:00:00Z"
endtime<-"2021-09-06T15:59:59Z"

# source('~/R/forecast/readDevice.R')
# device1<-device(hdr,station=all_station$stationCode)

source('~/R/forecast/readDevCap.R')
device<-deviceCap(hdr,station=all_station$stationCode)

source('~/R/forecast/readPoint.R')
point<-point(hdr,device$deviceId)
#直流功率："NB003",交流功率："NB018","NB031","NB034","NB001","NB002"
point<-filter(point,devicePointCode %in% c("NB028","NB029"))

#使用readHistory获取测点数据
source('~/R/forecast/readHistory.R')
tmp<-devicechart(hdr,stationcode=all_station$stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval = 2)
tmp1<-tmp%>%group_by(deviceName)%>%summarize(NB029=last(na.omit(NB029))-first(na.omit(NB029)))
merge(tmp,select(device,c("deviceCode","deviceCapacity")),by=c("deviceCode"))



