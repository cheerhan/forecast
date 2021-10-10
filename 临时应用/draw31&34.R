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
error_sn<-"3344125"
# 使用readStation获得所有电站
source('~/R/forecast/readPvStation.R')
station<-station(hdr)
error_station<-filter(station,stationCode %in% error_sn)
#时间段
startime<-"2021-07-31T16:00:00Z"
endtime<-"2021-08-31T15:59:59Z"
dateRanges <- data.frame(
  start = seq(as.POSIXct(as.Date(startime)+1), as.POSIXct(as.Date(endtime)), "2 day"),
  end = seq(as.POSIXct(as.Date(startime)+2), as.POSIXct(as.Date(endtime)+1), "2 day")
)

source('~/R/forecast/readDevice.R')
all_device<-device(hdr,station=error_station$stationCode)
for(i in 1:length(all_device$deviceCode)){
    device<-all_device[i,]
    #使用readPoint获得设备列表的部分测点
    source('~/R/forecast/readPoint.R')
    point<-point(hdr,device$deviceId)
    #直流功率："NB003",交流功率："NB018","NB031","NB034"
    point<-filter(point,devicePointCode %in% c("NB031","NB034"))
    #使用readHistory获取测点数据
    source('~/R/forecast/readHistory.R')
    data_10<-devicechart(hdr,stationcode=station$stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval = 10)
    data_1<-devicechart(hdr,stationcode=station$stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval =2)
    rm(data_1)
  }

