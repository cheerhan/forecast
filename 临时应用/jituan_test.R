library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(psych)

library(ggplot2)
library(grid)
library(gridExtra)
##集团的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjbmVhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDEwLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJjbmVhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLljY_lkIjnrqHnkIblkZgiLCJwaG9uZU51bSI6IjE4NTE0NDY5MjQ0IiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjMxNjYwMzQwMDk5NDgxNiIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjMxNjYwMzQ5MzI2OTUwNCIsImV4cCI6MTkzNjg1MTQ0NCwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzL2RlNTViNzQyYjE0NjQ4MGI4MjQxODBhNjM3MzljYzljLlBORyIsImVudGVycHJpc2VOYW1lIjoi5Y2P5ZCI5paw6IO95rqQIiwianRpIjoiYmUyNmRjNmMtY2RkNC00ZjFkLTljZmQtYjAzNWEyOTUxZjgzIiwidXNlcm5hbWUiOiJjbmVhZG1pbiJ9.GCKqs6GYYCqKjREw9BZyPOtsAbEZ0c1hL75VEA11q5E",
      'Content-Type'="application/json;charset=UTF-8")

##集团enterpriseid
enterpriseid<-"316603493269504"

# 使用readStation获得所有电站
source('~/R/forecast/readPvStation.R')
all_station<-station(hdr)
all_station<-filter(all_station,regionName!="美国"&stationName!="乃东储能"&stationStatue!='900')

#时间段
startime<-"2021-08-31T16:00:00Z"
endtime<-"2021-09-06T15:59:59Z"

power_10<-data.frame()
  
source('~/R/forecast/readDevice.R')
all_device<-device(hdr,station=station$stationCode)
device<-all_device
source('~/R/forecast/readPoint.R')
point<-point(hdr,device$deviceId)
#直流功率："NB003",交流功率："NB018","NB031","NB034","NB001","NB002"
point<-filter(point,devicePointCode %in% c("NB018","NB031"))

#使用readHistory获取测点数据
source('~/R/forecast/readHistory.R')
data_10<-devicechart(hdr,stationcode=station$stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval = 10)




