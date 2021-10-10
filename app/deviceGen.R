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

# ##集团的header
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
all_station<-filter(all_station,stationName%in%c("唐山迁钢")&stationStatus!='900')
# all_station<-filter(all_station,regionName!="美国"&stationName!="乃东储能"&stationStatus!='900')

#时间段
startime<-"2021-09-16T16:00:00Z"
endtime<-"2021-09-17T15:59:59Z"

power_10<-data.frame()
power_1<-data.frame()
for(j in 1:length(all_station$stationCode)){
   station<-all_station[j,]
   print(paste(j,station$stationName))

   source('~/R/forecast/src/readDevice.R')
   all_device<-device(hdr,station=station$stationCode)
   
   for(i in 1:length(all_device$deviceCode)){
      device<-all_device[i,]
      #使用readPoint获得设备列表的部分测点
      tryCatch(
         { 
            source('~/R/forecast/src/readPoint.R')
            point<-point(hdr,device$deviceId)
            #直流功率："NB003",交流功率："NB018","NB031","NB034","NB001","NB002"
            point<-filter(point,devicePointCode %in% c("QX002","NB018","NB031","NB034"))
             #使用readHistory获取测点数据
            source('~/R/forecast/src/readHistory.R')
            data_10<-devicechart(hdr,stationcode=station$stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval = 10)
            data_1<-devicechart(hdr,stationcode=station$stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval =2)
            power_10<-bind_rows(power_10,data_10)
            power_1<-bind_rows(power_1,data_1)
            #校对功率
            # tmp<-data_10%>%
            #    group_by(as.Date(Time))%>%summarise(deviceName=first(deviceName),
            #                                        NB003=sum(na.omit(NB003)),
            #                                        NB018=sum(na.omit(NB018)))%>%
            #    mutate(stationName=station$stationName)
            # power_10<-bind_rows(power_10,tmp) 
            # #校对发电量
            # tmp<-data_10%>%
            #    group_by(as.Date(Time))%>%summarise(deviceName=first(deviceName),
            #                                        NB031_10=last(na.omit(NB031)),
            #                                        NB034_10=last(na.omit(NB034))-first(na.omit(NB034)))%>%
            #    mutate(stationName=station$stationName)
            # power_10<-bind_rows(power_10,tmp)
            
            # 校对逆变器报表
            # tmp<-data_10%>%
            #    group_by(as.Date(Time))%>%summarise(deviceName=first(deviceName),
            #                                        NB003=sum(na.omit(NB003)),
            #                                        NB018=sum(na.omit(NB018)), 
            #                                        NB031=last(na.omit(NB031)),
            #                                        NB034=last(na.omit(NB034))-first(na.omit(NB034)),
            #                                        QX012=last(na.omit(QX012)),
            #                                        QX002=sum(na.omit(QX002)))%>%
            #    mutate(stationName=station$stationName)
            # power_10<-bind_rows(power_10,tmp)
            # 
            }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
      )
   }
}


tmp_10<-power_10%>%
   group_by(as.Date(Time),deviceCode)%>%summarise(deviceName=first(deviceName),
                                       NB031_10=last(na.omit(NB031)),
                                       NB034_10=last(na.omit(NB034))-first(na.omit(NB034)),
                                       NB034_sd=sd(na.omit(NB034)))

tmp_1<-power_1%>%
   group_by(as.Date(Time),deviceCode)%>%summarise(deviceName=first(deviceName),
                                       NB031_1=last(na.omit(NB031)),
                                       NB034_1=last(na.omit(NB034))-first(na.omit(NB034)),
                                       NB034_sd=sd(na.omit(NB034)))
# tmp<-power_10%>%
#    group_by(as.Date(Time),deviceCode)%>%summarise(deviceName=first(deviceName),
#                                        NB003=sum(na.omit(NB003))/6,
#                                        NB018=sum(na.omit(NB018))/6)


library("openxlsx")
wb <- createWorkbook()
addWorksheet(wb, "问题设备")
writeData(wb, "问题设备", filter(power_10,NB018>NB003), startRow = 1, startCol = 1)

saveWorkbook(wb, file = "普洛斯电量问题整理/功率问题/设备列表.xlsx", overwrite = TRUE)



