# 用于统计普洛斯月发电量

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
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJwbHNhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDIyLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJwbHNhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLmma7mtJvmlq_nrqHnkIblkZgiLCJwaG9uZU51bSI6IjE1NjExNDA5NTYyIiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjQ1MTQzNjQ2MzY5MjI4OCIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjQ1MTQzNjQ2Nzg4NjU5MiIsImV4cCI6MTkzMTY1NDIzNSwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzLzBjMTM3OTczMDI1MDQ2ODY4OGM5M2ZmZmEyZTg3ODU1LmpwZyIsImVudGVycHJpc2VOYW1lIjoi5pmu5p6r5paw6IO95rqQIiwianRpIjoiY2IyMGVlM2YtMmI3NS00MjE0LWE4ZjYtNjdkODIzYWE0M2VkIiwidXNlcm5hbWUiOiJwbHNhZG1pbiJ9.qLK9ocTltrvrd8ameGhd8Sc_hLvAvhbjy8vKmmKdtVA",      'Content-Type'="application/json;charset=UTF-8")

#普洛斯
enterpriseid<-"451436467886592"

startday<-"2021-09-01"
days<-30


# 使用readStation获得所有电站
source('~/R/forecast/src/readPVStation.R')
all_station<-station(hdr)
all_station<-filter(all_station,stationName%in%c("滁州盼盼")&stationStatus!='900')


data<-data.frame()
data_34<-data.frame()
for(j in 1:nrow(all_station)){
  stationCode<-all_station[j,]$stationCode
  print(paste(j,all_station[j,]$stationName))
  
  source('~/R/forecast/src/readDevice.R')
  all_device<-device(hdr,station=stationCode)
  for(i in 1:nrow(all_device)){
    device<-all_device[i,]
    #使用readPoint获得设备列表的部分测点
    source('~/R/forecast/src/readPoint.R')
    point<-point(hdr,device$deviceId)
    point_31<-filter(point,devicePointCode %in% c("NB031","NB034"))
    point_34<-filter(point,devicePointCode %in% c("NB034"))
    for(k in 1:days){
      if(k==1|k==days)
        tryCatch({
          #使用readHistory获取测点数据
          startime<-startday%>%as.POSIXct("Asia/Shanghai")%>%+days(k-1)%>%format(tz="UTC")%>%
            gsub(pattern=" ",replacement = "T",time)%>%
            paste("Z",sep = "")
          endtime<-startday%>%as.POSIXct("Asia/Shanghai")%>%+days(k)%>%-seconds(1)%>%format(tz="UTC")%>%
            gsub(pattern=" ",replacement = "T",time)%>%
            paste("Z",sep = "")
          source('~/R/forecast/src/readData.R')
          tmp<-devicedata(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point_34$devicePointId,enterpriseid,timeInterval = 2)
          tmp_34<-filter(tmp,as.numeric(NB034)!=0)%>%group_by(time=as.Date(time))%>%summarise(stationName=first(stationName),
                                                                                              deviceModeName=first(deviceModeName),
                                                                                              deviceName=first(deviceName),
                                                                                              NB034_f=first(as.numeric(na.omit(NB034))),
                                                                                              NB034_l=last(as.numeric(na.omit(NB034))))
          data<-bind_rows(data,tmp)
          data_34<-bind_rows(data_34,tmp_34)
        }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    }
    print(paste(nrow(all_device)-i,all_device[i,]$deviceName,Sys.time()))
  }
}

# sum_31<-data_31%>%group_by(stationName,deviceName)%>%summarise(power_31=sum(NB031))
sum_34<-data_34%>%group_by(stationName,deviceName)%>%
  summarise(power_34=last(NB034_l)-first(NB034_f))
sum_device<-merge(sum_31,sum_34,by=c("stationName","deviceName"),all.x = TRUE)
# 
# 
# sum_31<-data_31%>%group_by(stationName)%>%summarise(power_31=sum(NB031))
# sum_34<-data_34%>%group_by(stationName,deviceName)%>%
#   summarise(power_34=last(NB034_l)-first(NB034_f))%>%
#   group_by(stationName)%>%
#   summarise(power_34=sum(power_34))
# sum_station<-merge(sum_31,sum_34,by="stationName",all.x = TRUE)
# 
# View(filter(sum_station,abs(power_34-power_31)>power_31*0.03))
# View(filter(sum_device,abs(power_34-power_31)>power_31*0.03))

# 
# library("openxlsx")
# wb <- createWorkbook()
# addWorksheet(wb, "电站电量")
# addWorksheet(wb, "设备电量")
# addWorksheet(wb, "问题设备")
# 
# writeData(wb, "电站电量", sum_station, startRow = 1, startCol = 1)
# writeData(wb, "设备电量", sum_device, startRow = 1, startCol = 1)
# writeData(wb, "问题设备", filter(sum_station,abs(power_34-power_31)>power_31*0.03), startRow = 1, startCol = 1)
# 
# 
# saveWorkbook(wb, file = "result/普洛斯10月导出.xlsx", overwrite = TRUE)


# tryCatch({ 
#   startime<-day%>%as.POSIXct("Asia/Shanghai")%>%+hours(17)%>%format(tz="UTC")%>%
#     gsub(pattern=" ",replacement = "T",time)%>%
#     paste("Z",sep = "")
#   endtime<-day%>%as.POSIXct("Asia/Shanghai")%>%+days(30)%>%-seconds(1)%>%format(tz="UTC")%>%
#     gsub(pattern=" ",replacement = "T",time)%>%
#     paste("Z",sep = "")
#   #使用readHistory获取测点数据
#   source('~/R/forecast/src/readData.R')
#   tmp<-devicedata(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point_31$devicePointId,enterpriseid,timeInterval = 10)
#   tmp_31<-filter(tmp,as.numeric(NB031)!=0)%>%group_by(time=as.Date(time))%>%summarise(stationName=first(stationName),
#                                                                                       deviceModeName=first(deviceModeName),
#                                                                                       deviceName=first(deviceName),
#                                                                                       NB031=last(as.numeric(na.omit(NB031))),
#                                                                                       NB034=last(as.numeric(na.omit(NB034)))-first(as.numeric(na.omit(NB034))))
#   data_31<-bind_rows(data_31,tmp_31)
# }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")}) 
# 


# dailyPower<-function(data){
#   p<-0
#   for(i in 1:nrow(data){
# 
#   }
#   return(p)
# }
# 
# dailyPower(tmp)


########
# #求所有设备一天中nb031的最大值，NB034的最小值，NB034的最大值-最小值
# daily_device<-data%>% group_by(deviceCode,time=as.Date(Time))%>%summarise(deviceName=first(deviceName),
#                                                                           max_31=max(NB031,na.rm = TRUE),
#                                                                           min_34=min(NB034,na.rm = TRUE),
#                                                                           max_34=max(NB034,na.rm=TRUE),  
#                                                                           first_34=first(na.omit(NB034)),
#                                                                           last_34=last(na.omit(NB034)))
# daily_device[sapply(daily_device, is.infinite)] <- NA
# daily_device<-daily_device%>%separate(deviceCode,into=c("stationCode","deviceType","deviceMode","deviceNo"),sep="M")
# daily_device<-merge(daily_device,station,by="stationCode")
# #设备日发电量
# daily_device<-select(daily_device,c("stationCode","stationName","stationCapacity","deviceName","time","max_31","min_34","max_34","first_34","last_34"))
# daily_device<-mutate(daily_device,mm_34=max_34-min_34,fl_34=last_34-first_34)
# daily_device<-mutate(daily_device,diff_mm=abs(mm_34-max_31),diff_fl=abs(fl_34-max_31))
# error_daily_device<-filter(daily_device,diff_mm>max_31*0.03|diff_fl>max_31*0.03)
# error_daily_device<-as.data.frame(error_daily_device)
# #设备总发电量
# sum_device<-daily_device%>%group_by(stationCode,deviceName)%>%summarise(stationCode=first(stationCode),
#                                                                        stationName=first(stationName),
#                                                                        stationCapacity=first(stationCapacity),
#                                                                        max_31=sum(max_31,na.rm = TRUE),
#                                                                        mm_34=sum(mm_34,na.rm = TRUE),
#                                                                        fl_34=sum(fl_34,na.rm=TRUE))
# 
# sum_device<-mutate(sum_device,diff_mm=abs(mm_34-max_31),diff_fl=abs(fl_34-max_31))
# sum_device<-as.data.frame(sum_device)
# 
# #问题设备
# error_sum_device<-filter(sum_device,diff_mm>max_31*0.03|diff_fl>max_31*0.03)
# 
# #电站总发电量
# sum_station<-sum_device%>%group_by(stationCode)%>%summarise(stationCode=first(stationCode),
#                                                             stationName=first(stationName),
#                                                             stationCapacity=first(stationCapacity),
#                                                             max_31=sum(max_31,na.rm=TRUE),
#                                                             mm_34=sum(mm_34,na.rm=TRUE),
#                                                             fl_34=sum(fl_34,na.rm=TRUE))
# sum_station<-mutate(sum_station,diff_mm=abs(mm_34-max_31),diff_fl=abs(fl_34-max_31))
# sum_station<-as.data.frame(sum_station)
#                                                           
# 
# library(openxlsx)
# options(scipen = 999)
# write.xlsx(sum_station, "result/普洛斯8月电量.xlsx", sheetName = "Sheet1",append=TRUE)
# xlsx::write.xlsx(sum_device, "result/普洛斯8月电量.xlsx", sheetName = "设备电量",append=TRUE)
# 
# 

# 
# 
