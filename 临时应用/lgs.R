library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(psych)
library(ggplot2)

##普洛斯的config：header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJwbHNhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDIyLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJwbHNhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLmma7mtJvmlq_nrqHnkIblkZgiLCJwaG9uZU51bSI6IjE1NjExNDA5NTYyIiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjQ1MTQzNjQ2MzY5MjI4OCIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjQ1MTQzNjQ2Nzg4NjU5MiIsImV4cCI6MTkzMTY1NDIzNSwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzLzBjMTM3OTczMDI1MDQ2ODY4OGM5M2ZmZmEyZTg3ODU1LmpwZyIsImVudGVycHJpc2VOYW1lIjoi5pmu5p6r5paw6IO95rqQIiwianRpIjoiY2IyMGVlM2YtMmI3NS00MjE0LWE4ZjYtNjdkODIzYWE0M2VkIiwidXNlcm5hbWUiOiJwbHNhZG1pbiJ9.qLK9ocTltrvrd8ameGhd8Sc_hLvAvhbjy8vKmmKdtVA",      
      'Content-Type'="application/json;charset=UTF-8")

##普洛斯enterpriseid
enterpriseid<-"451436467886592"

#德州齐河
stationCode<-"3344194"

source('~/R/forecast/readDevice.R')
device<-device(hdr,station=stationCode)
device<-filter(device,deviceName %in% c("1N2逆变器"))

#使用readPoint获得设备列表的部分测点
source('~/R/forecast/readPoint.R')
point<-point(hdr,device$deviceId)
#直流功率："NB003",交流功率："NB018","NB031","NB034"
point<-filter(point,devicePointCode %in% c("NB018","NB031","NB034"))

startime<-"2020-07-31T16:00:00Z"
endtime<-"2021-08-31T15:59:59Z"
#使用readHistory获取测点数据
source('~/R/forecast/readHistory.R')
data_10<-devicechart(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval = 10)


tmp<-filter(data_10,format(Time, format="%H:%M") %in% c("00:00","08:30","10:00","11:00","12:00","13:00","14:30","23:50"))

# 聚合
tmp2<-tmp %>%
  group_by(as.Date(Time)) %>%  
  mutate(end_time = c(Time[-1], NA))%>%
  mutate(new_NB031 = as.numeric(NB031[-1])-as.numeric(NB031))%>%
  filter(new_NB031>=0)%>%
  select(c("deviceName","Time","end_time","NB031","new_NB031"))

power<- function(){
  
  }

library("openxlsx")
wb <- createWorkbook()
addWorksheet(wb, "1N2逆变器发电量")

writeData(wb, "1N2逆变器发电量", tmp2, startRow = 1, startCol = 1)

saveWorkbook(wb, file = "result/德州齐河1N2逆变器发电量.xlsx", overwrite = TRUE)
