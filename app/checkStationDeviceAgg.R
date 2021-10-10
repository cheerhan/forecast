library(jsonlite)
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

##集团的header
# hdr=c('Accept'="application/json, text/plain, */*",
#       'Accept-Encoding'="gzip, deflate, br",
#       'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
#       'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjbmVhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDEwLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJjbmVhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLljY_lkIjnrqHnkIblkZgiLCJwaG9uZU51bSI6IjE4NTE0NDY5MjQ0IiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjMxNjYwMzQwMDk5NDgxNiIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjMxNjYwMzQ5MzI2OTUwNCIsImV4cCI6MTkzNjg1MTQ0NCwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzL2RlNTViNzQyYjE0NjQ4MGI4MjQxODBhNjM3MzljYzljLlBORyIsImVudGVycHJpc2VOYW1lIjoi5Y2P5ZCI5paw6IO95rqQIiwianRpIjoiYmUyNmRjNmMtY2RkNC00ZjFkLTljZmQtYjAzNWEyOTUxZjgzIiwidXNlcm5hbWUiOiJjbmVhZG1pbiJ9.GCKqs6GYYCqKjREw9BZyPOtsAbEZ0c1hL75VEA11q5E",
#       'Content-Type'="application/json;charset=UTF-8")
# 
# ##集团enterpriseid
# enterpriseid<-"316603493269504"

# 使用readStation获得所有电站
source('~/R/forecast/src/readPvStation.R')
all_station<-station(hdr)
# all_station<-filter(all_station,stationName%in%s&stationStatus!='900')
all_station<-filter(all_station,stationName%in%c("佛山百威啤酒一期","佛山百威啤酒二期")&stationStatus!='900')
# all_station<-filter(all_station,regionName!="美国"&stationName!="乃东储能"&stationStatus!='900')

#时间段
startime<-"2021-09-25T16:00:00Z"
endtime<-"2021-09-26T15:59:59Z"
dateRanges <- data.frame(
   start = seq(as.POSIXct(as.Date(startime)+1), as.POSIXct(as.Date(endtime)), "2 day"),
   end = seq(as.POSIXct(as.Date(startime)+2), as.POSIXct(as.Date(endtime)+1), "2 day")
)

#对比电站测点与设备求和后测点
for(j in 1:length(all_station$stationCode)){
   station<-all_station[j,]
   print(paste(j,station$stationName))
   
   #全站数据
   source('~/R/forecast/src/readStationID.R')
   stationId<-stationID(hdr,station=station$stationCode)
   
   source('~/R/forecast/src/readPoint.R')
   point<-point(hdr,stationId$deviceId)
   #直流功率："NB003",交流功率："NB018","NB031","NB034","NB001","NB002"
   point<-filter(point,devicePointCode %in% c("QZ003","QZ001"))
   
   source('~/R/forecast/src/readHistory.R')
   power_10<-devicechart(hdr,stationcode=station$stationCode,stationId$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval = 10)
   
   #设备数据
   source('~/R/forecast/src/readDevice.R')
   all_device<-device(hdr,station=station$stationCode)
   
   power_1<-data.frame()
   for(i in 1:length(all_device$deviceCode)){
      device<-all_device[i,]
      tryCatch(
         { 
            source('~/R/forecast/src/readPoint.R')
            point<-point(hdr,device$deviceId)
            #直流功率："NB003",交流功率："NB018","NB031","NB034","NB001","NB002"
            point<-filter(point,devicePointCode %in% c("NB018"))
            #使用readHistory获取测点数据
            source('~/R/forecast/src/readHistory.R')
            data_1<-devicechart(hdr,stationcode=station$stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval =10)
            power_1<-bind_rows(power_1,data_1)

         }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
      )
   }
   tmp<-power_1%>%group_by(Time)%>%summarise(NB018=sum(NB018))
   
   g1<-ggplot() +
      geom_line(data=power_10, aes(x=Time, y=QZ001), color='orange')+
      geom_line(data=tmp, aes(x=Time, y=NB018), color='red') +
      geom_line(data=power_10, aes(x=Time, y=QZ003), color='blue')+
      geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
                inherit.aes=FALSE, alpha = 0.4, fill = c("lightblue"))
   path<-file.path("checkgen17")
   if(!dir.exists(path))
      dir.create(path,recursive = TRUE)
   
   jpeg(filename = paste0(path,"/",station$stationName,'.jpeg'),width = 1800,height = 600,units = "px")
   plot(g1)
   dev.off()
}




