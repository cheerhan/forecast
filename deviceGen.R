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
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJwbHNhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDIyLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJwbHNhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLmma7mtJvmlq_nrqHnkIblkZgiLCJwaG9uZU51bSI6IjE1NjExNDA5NTYyIiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjQ1MTQzNjQ2MzY5MjI4OCIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjQ1MTQzNjQ2Nzg4NjU5MiIsImV4cCI6MTkzMTY1NDIzNSwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzLzBjMTM3OTczMDI1MDQ2ODY4OGM5M2ZmZmEyZTg3ODU1LmpwZyIsImVudGVycHJpc2VOYW1lIjoi5pmu5p6r5paw6IO95rqQIiwianRpIjoiY2IyMGVlM2YtMmI3NS00MjE0LWE4ZjYtNjdkODIzYWE0M2VkIiwidXNlcm5hbWUiOiJwbHNhZG1pbiJ9.qLK9ocTltrvrd8ameGhd8Sc_hLvAvhbjy8vKmmKdtVA",      
      'Content-Type'="application/json;charset=UTF-8")
##集团的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjbmVhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDEwLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJjbmVhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLljY_lkIjnrqHnkIblkZgiLCJwaG9uZU51bSI6IjE4NTE0NDY5MjQ0IiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjMxNjYwMzQwMDk5NDgxNiIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjMxNjYwMzQ5MzI2OTUwNCIsImV4cCI6MTkzNjg1MTQ0NCwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzL2RlNTViNzQyYjE0NjQ4MGI4MjQxODBhNjM3MzljYzljLlBORyIsImVudGVycHJpc2VOYW1lIjoi5Y2P5ZCI5paw6IO95rqQIiwianRpIjoiYmUyNmRjNmMtY2RkNC00ZjFkLTljZmQtYjAzNWEyOTUxZjgzIiwidXNlcm5hbWUiOiJjbmVhZG1pbiJ9.GCKqs6GYYCqKjREw9BZyPOtsAbEZ0c1hL75VEA11q5E",      
      'Content-Type'="application/json;charset=UTF-8")


startime<-"2021-08-14T16:00:00Z"
endtime<-"2021-08-15T15:59:59Z"
##集团enterpriseid
enterpriseid<-"316603493269504"
#江孜
stationCode<-"504"

source('~/R/forecast/readDevice.R')
device<-device(hdr,station=stationCode)
device<-device[11,]
#使用readPoint获得设备列表的部分测点
source('~/R/forecast/readPoint.R')
point<-point(hdr,device$deviceId)
#直流功率："NB003",交流功率："NB018","NB031","NB034"
point<-filter(point,devicePointCode %in% c("NB031","NB034","NB003"))

#使用readHistory获取测点数据
source('~/R/forecast/readHistory.R')
data<-devicechart(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId)

#计算所有逆变器的电量
sum_31<- data%>% group_by(deviceCode)%>%summarise(max_31=max(NB031,na.rm = TRUE),sub_34=max(NB034,na.rm = TRUE)-min(NB034,na.rm = TRUE))
sum_31[sapply(sum_31, is.infinite)] <- NA
sum_31<- data%>% group_by(deviceCode,date=as.Date(as.POSIXct(Time, 'GMT')))%>%summarise(max_31=max(NB031,na.rm = TRUE),sub_34=max(NB034,na.rm = TRUE)-min(NB034,na.rm = TRUE))

source('~/R/forecast/readDevCap.R')
device<-deviceCap(hdr,stationCode)

gen<-merge(device,sum_31,by="deviceCode")
gen<-mutate(gen,hour_31=max_31/as.numeric(deviceCapacity))
gen<-mutate(gen,is.fun=0)


#lossDevice是按日标记的故障设备，需要从别的地方来。o(╯□╰)o

#标记故障设备
con<-which(outer(gen$deviceName,lossDevice$deviceName,"==") & outer(gen$date,lossDevice$date,"=="),arr.ind=TRUE)
gen<-within(gen,is.fun[con[,1]]<-1)

#计算等效小时数
mean_hour<-gen %>% group_by(date) %>% filter(is.fun==0)%>%summarise(mean_hour=mean(hour_31))
gen<-merge(gen,mean_hour,by="date")

#计算损失电量
gen<-mutate(gen,loss_gen=case_when(is.fun==1~mean_hour*as.numeric(deviceCapacity)-max_31,TRUE~0))



