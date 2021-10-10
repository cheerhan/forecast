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

# s<-unlist(distinct(error,stationName))
# 使用readStation获得所有电站
source('~/R/forecast/src/readPvStation.R')
all_station<-station(hdr)
all_station<-filter(all_station,stationName%in%c("滁州盼盼")&stationStatus!='900')
# all_station<-filter(all_station,stationName%in%s&stationStatus!='900')
# all_station<-filter(all_station,stationName%in%c("南京江宁南","北京空港","成都新都","大连环普国际","沈阳福耀","苏州百得电动工具","苏州环普A区","唐山迁钢","无锡环普","武汉黄陂","武汉江夏")&stationStatus!='900')
# all_station<-filter(all_station,stationName%in%c("佛山百威啤酒一期","佛山百威啤酒二期")&stationStatus!='900')
# all_station<-filter(all_station,regionName!="美国"&stationName!="乃东储能"&stationStatus!='900')

#时间段
day<-"2021-09-30"
startime<-day%>%as.POSIXct("Asia/Shanghai")%>%
   format(tz="UTC")%>%
   gsub(pattern=" ",replacement = "T",time)%>%
   paste("Z",sep = "")
endtime<-day%>%as.POSIXct("Asia/Shanghai")%>%+days(1)%>%-seconds(1)%>%format(tz="UTC")%>%
   gsub(pattern=" ",replacement = "T",time)%>%
   paste("Z",sep = "")

#对比逆变器测点
for(j in 1:length(all_station$stationCode)){
   station<-all_station[j,]
   print(paste(j,station$stationName))

   source('~/R/forecast/src/readDevice.R')
   all_device<-device(hdr,station=station$stationCode)
   power_10<-data.frame()
   power_1<-data.frame()
   for(i in 1:length(all_device$deviceCode)){
      device<-all_device[i,]
      #使用readPoint获得设备列表的部分测点
      tryCatch(
         { 
            source('~/R/forecast/src/readPoint.R')
            point<-point(hdr,device$deviceId)
            #直流功率："NB003",交流功率："NB018","NB031","NB034","NB001","NB002"
            point<-filter(point,devicePointCode %in% c("NB034"))
             #使用readHistory获取测点数据
            source('~/R/forecast/src/readHistory.R')
            data_10<-devicechart(hdr,stationcode=station$stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval = 10)
            data_1<-devicechart(hdr,stationcode=station$stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval =2)
            power_10<-bind_rows(power_10,data_10)
            power_1<-bind_rows(power_1,data_1)

            # g1<-ggplot() +
            #    geom_line(data=data_1, aes(x=Time, y=NB031), color='red')+
            #    geom_line(data=data_10, aes(x=Time, y=NB031), color='blue') +
            #    geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            #              inherit.aes=FALSE, alpha = 0.4, fill = c("lightblue"))+facet_grid()

            # g2<-ggplot() +
            #    geom_line(data=data_1, aes(x=Time, y=NB034), color='red')+
            #    geom_line(data=data_10, aes(x=Time, y=NB034), color='blue') +
            #    geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            #              inherit.aes=FALSE, alpha = 0.4, fill = c("lightblue"))+facet_grid()

            # g3<-ggplot() +
            #    geom_line(data=data_1, aes(x=Time, y=NB018), color='red')+
            #    geom_line(data=data_10, aes(x=Time, y=NB018), color='blue') +
            #    geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
            #              inherit.aes=FALSE, alpha = 0.4, fill = c("lightblue"))

          # path<-file.path("checkgen32",paste(station$stationName,station$stationUnitCount,sep = '_'))
            # if(!dir.exists(path))
            #    dir.create(path,recursive = TRUE)
            # 
            # jpeg(filename = paste0(path,"/",device$deviceCode,'.jpeg'),width = 1800,height = 600,units = "px")
            # # grid.arrange(g1, g2, nrow = 2)
            # plot(g3)
            # dev.off()
         }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}
      )
   }
   tmp<-merge(power_1,power_10,by=c("deviceCode","deviceName","Time"),all.x=TRUE)
   g3<-ggplot(data=tmp)+
      geom_path(aes(x=Time, y=NB034.x), color='red')+
      geom_line(aes(x=Time, y=NB034.y), color='blue')+
      geom_point(aes(x=Time, y=NB034.y), color='blue')+
      theme(text = element_text(family = "Noto Sans CJK SC")) +
      facet_wrap(~deviceName)

   path<-file.path("checkgen34")
   if(!dir.exists(path))
      dir.create(path,recursive = TRUE)

   jpeg(filename = paste0(path,"/",station$stationName,'.jpeg'),width = 1800,height = 1000,units = "px")
   plot(g3)
   dev.off()
}



# tmp<-power_10%>%
#    group_by(as.Date(Time),deviceCode)%>%summarise(deviceName=first(deviceName),
#                                        NB003=sum(na.omit(NB003)),
#                                        NB018=sum(na.omit(NB018)),
#                                        NB031=last(na.omit(NB031)),
#                                        NB034=last(na.omit(NB034))-first(na.omit(NB034)))
# 
# library("openxlsx")
# wb <- createWorkbook()
# addWorksheet(wb, "问题设备")
# writeData(wb, "问题设备", filter(power_10,NB018>NB003), startRow = 1, startCol = 1)
# 
# saveWorkbook(wb, file = "普洛斯电量问题整理/功率问题/设备列表.xlsx", overwrite = TRUE)





