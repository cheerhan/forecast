library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(psych)
library(ggplot2)
library(gghighlight)
##普洛斯的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJwbHNhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDIyLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJwbHNhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLmma7mtJvmlq_nrqHnkIblkZgiLCJwaG9uZU51bSI6IjE1NjExNDA5NTYyIiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjQ1MTQzNjQ2MzY5MjI4OCIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjQ1MTQzNjQ2Nzg4NjU5MiIsImV4cCI6MTkzMTY1NDIzNSwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzLzBjMTM3OTczMDI1MDQ2ODY4OGM5M2ZmZmEyZTg3ODU1LmpwZyIsImVudGVycHJpc2VOYW1lIjoi5pmu5p6r5paw6IO95rqQIiwianRpIjoiY2IyMGVlM2YtMmI3NS00MjE0LWE4ZjYtNjdkODIzYWE0M2VkIiwidXNlcm5hbWUiOiJwbHNhZG1pbiJ9.qLK9ocTltrvrd8ameGhd8Sc_hLvAvhbjy8vKmmKdtVA",      'Content-Type'="application/json;charset=UTF-8")
startime<-"2021-09-12T16:00:00Z"
endtime<-"2021-09-18T15:59:59Z"
enterpriseid<-"451436467886592"

source('~/R/forecast/src/readPvStation.R')
all_station<-station(hdr)
station<-filter(all_station,stationName%in%c("唐山迁钢")&stationStatus!='900')
stationCode<-station$stationCode

source('~/R/forecast/src/readDevCap.R')
device<-deviceCap(hdr,station=stationCode)

#使用readPoint获得设备列表的部分测点
source('~/R/forecast/src/readPoint.R')
point<-point(hdr,device$deviceId)
point<-filter(point,devicePointCode %in% c("QX002","NB018","NB003"))

#使用readHistory获取测点数据
source('~/R/forecast/src/readHistory.R')
data<-devicechart(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId,timeInterval =10)

data<-merge(data,device,by=c("deviceCode","deviceName"))
data<-select(data,c("deviceCode","deviceName","Time","deviceCapacity","QX002","NB003","NB018"))
data<-mutate(data,load=as.numeric(NB018)/as.numeric(deviceCapacity),convert=NB018/NB003)

library(nlme)
fm<-lmList(standard_18~QX002|deviceName, data=data, na.action=na.omit)
fm<-coef(fm)
# fm<-as.data.frame(fm)

#负载率&资源
ggplot(data,aes(x=QX002, y=load,color=deviceName)) +
  geom_smooth(method='loess')+   
  theme(legend.position = "none")+
  ylim(0,1)+
  gghighlight()+
  theme(text = element_text(family = "Noto Sans CJK SC")) +
  facet_wrap(~deviceName)

#负载率& 转换效率
ggplot(data,aes(x=load, y=convert,color=deviceName)) +
  geom_smooth(method='loess')+ 
  theme(legend.position = "none")+
  ylim(0.4,2)+
  gghighlight()+
  theme(text = element_text(family = "Noto Sans CJK SC")) +
  facet_wrap(~deviceName)


xyplot(convert~load|deviceName,data=data,ylim = c(0,2))
pairs(select(data,c("NB030","NB018","convert","load")))

             