library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(psych)
library(lattice)
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

# #集团的header
# hdr=c('Accept'="application/json, text/plain, */*",
#       'Accept-Encoding'="gzip, deflate, br",
#       'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
#       'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjbmVhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDEwLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJjbmVhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLljY_lkIjnrqHnkIblkZgiLCJwaG9uZU51bSI6IjE4NTE0NDY5MjQ0IiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjMxNjYwMzQwMDk5NDgxNiIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjMxNjYwMzQ5MzI2OTUwNCIsImV4cCI6MTkzNjg1MTQ0NCwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzL2RlNTViNzQyYjE0NjQ4MGI4MjQxODBhNjM3MzljYzljLlBORyIsImVudGVycHJpc2VOYW1lIjoi5Y2P5ZCI5paw6IO95rqQIiwianRpIjoiYmUyNmRjNmMtY2RkNC00ZjFkLTljZmQtYjAzNWEyOTUxZjgzIiwidXNlcm5hbWUiOiJjbmVhZG1pbiJ9.GCKqs6GYYCqKjREw9BZyPOtsAbEZ0c1hL75VEA11q5E",
#       'Content-Type'="application/json;charset=UTF-8")
# ##集团enterpriseid
# enterpriseid<-"316603493269504"

# 使用readStation获得所有电站
source('~/R/forecast/src/readPvStation.R')
all_station<-station(hdr)
all_station<-filter(all_station,stationName%in%c("唐山迁钢")&stationStatus!='900')
# all_station<-filter(all_station,regionName!="美国"&stationName!="乃东储能"&stationStatus!='900')

#时间段
startime<-"2021-08-31T16:00:00Z"
endtime<-"2021-09-14T15:59:59Z"

power_1<-data.frame()
power_10<-data.frame()
for(j in 1:length(all_station$stationCode)){
  station<-all_station[j,]
  print(paste(j,station$stationName))
  source('~/R/forecast/src/readDevCap.R')
  device<-deviceCap(hdr,station=station$stationCode)
  tryCatch(
    { 
        source('~/R/forecast/src/readPoint.R')
        point<-point(hdr,device$deviceId)
        #直流功率："NB003",交流功率："NB018","NB031","NB034","NB001","NB002"
        point<-filter(point,devicePointCode %in% c("NB031","NB034"))
        #使用readHistory获取测点数据
        source('~/R/forecast/src/readHistory.R')
        data_1<-devicechart(hdr,stationcode=station$stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval =2)
        data_10<-devicechart(hdr,stationcode=station$stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval =10)
        
        tmp1<-merge(data_1,device,by=c("deviceCode","deviceName"))
        tmp1<-tmp1%>%group_by(deviceCode,as.Date(Time))%>%
          mutate(diff31= as.numeric(NB031)- lag(as.numeric(NB031)))%>%
          mutate(diff34= as.numeric(NB034)- lag(as.numeric(NB034)))
        
        tmp2<-tmp1%>%filter(diff34>0)%>%
          mutate(period=difftime(Time,lag(Time),units = "hours"))%>%
          mutate(valid31=ifelse(diff31<as.numeric(period)*as.numeric(deviceCapacity)&diff31>0,NB031,NA))%>%
          mutate(valid34=ifelse(diff34<as.numeric(period)*as.numeric(deviceCapacity)&diff34>0,NB034,NA))
        
        tmp1<-merge(tmp1,select(tmp2,c("deviceCode","deviceName","Time","valid31","valid34")),by=c("deviceCode","deviceName","Time"), all.x=TRUE)
                    
        tmp<-tmp1%>%group_by(deviceCode,as.Date(Time))%>%summarise(deviceName=first(deviceName),
                                                    valid31=last(na.omit(valid31)),
                                                    valid34_mm=max(na.omit(valid34))-min(na.omit(valid34)),
                                                    valid34_fl=last(na.omit(valid34))-first(na.omit(valid34)))
        power_1<-bind_rows(power_1,tmp)
        
        # g1<-xyplot(valid34~Time|deviceName,data=tmp1,type='l')
        # g2<-xyplot(NB034~Time|deviceName,data=tmp1,type='l')
        # jpeg(filename = paste0("result/",station$stationName,'.jpeg'),width = 1920,height = 1080,units = "px")
        # grid.arrange(g1, g2, nrow = 2)
        # dev.off()

        tmp_10<-data_10%>%group_by(deviceCode,as.Date(Time))%>%
          summarise(deviceName=first(deviceName),
                    NB031_l=max(na.omit(NB031)),
                    NB034_mm=max(na.omit(NB034))-min(na.omit(NB034)),
                    NB034_fl=last(na.omit(NB034))-first(na.omit(NB034)))
        power_10<-bind_rows(power_10,tmp_10)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
  )
}


#对比1分钟数据与10分钟数据
tmp<-merge(power_1,power_10,by=c("deviceCode","deviceName","as.Date(Time)"),all=TRUE)
error<-filter(tmp,abs(NB031_l-valid31)>2|valid34_mm!=valid34_fl|abs(valid34_mm-NB034)>10)
error<-separate(error,col=deviceCode,into=c("stationCode","1","2","3"),sep = "M")
error<-merge(error,select(all_station,c("stationCode","stationName")),by="stationCode")

View(error)
