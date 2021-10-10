library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(psych)

##集团的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjbmVhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDEwLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJjbmVhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLljY_lkIjnrqHnkIblkZgiLCJwaG9uZU51bSI6IjE4NTE0NDY5MjQ0IiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjMxNjYwMzQwMDk5NDgxNiIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjMxNjYwMzQ5MzI2OTUwNCIsImV4cCI6MTkzNjg1MTQ0NCwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzL2RlNTViNzQyYjE0NjQ4MGI4MjQxODBhNjM3MzljYzljLlBORyIsImVudGVycHJpc2VOYW1lIjoi5Y2P5ZCI5paw6IO95rqQIiwianRpIjoiYmUyNmRjNmMtY2RkNC00ZjFkLTljZmQtYjAzNWEyOTUxZjgzIiwidXNlcm5hbWUiOiJjbmVhZG1pbiJ9.GCKqs6GYYCqKjREw9BZyPOtsAbEZ0c1hL75VEA11q5E",      
      'Content-Type'="application/json;charset=UTF-8")

##集团enterpriseid
enterpriseid<-"316603493269504"

startday<-"2021-09-01"
days<-1
startime<-startday%>%as.POSIXct("Asia/Shanghai")%>%format(tz="UTC")%>%
  gsub(pattern=" ",replacement = "T",time)%>%
  paste("Z",sep = "")
endtime<-startday%>%as.POSIXct("Asia/Shanghai")%>%+days(days)%>%-seconds(1)%>%format(tz="UTC")%>%
  gsub(pattern=" ",replacement = "T",time)%>%
  paste("Z",sep = "")

source('~/R/forecast/src/readWindStation.R')
all_station<-station(hdr)
# all_station<-all_station%>%arrange(as.numeric(stationUnitCount))
all_station<-filter(all_station,stationName%in%c("亳州谯东")&stationStatus!='900')
stationCode<-all_station[1,]$stationCode

source('~/R/forecast/src/readDevice.R')
all_device<-device(hdr,station=stationCode)
all_device<-all_device[1:1,]

data_10<-data.frame()
data_5<-data.frame()
for(i in 1:length(all_device$deviceCode)){
  device<-all_device[i,]
  #使用readPoint获得设备列表的部分测点
  source('~/R/forecast/src/readPoint.R')
  point<-point(hdr,device$deviceId)
  #风机总发："TR008",日发："TR012","TR028"
  point<-filter(point,devicePointCode %in% c("TR008","TR002","TM102"))
  #使用readHistory获取测点数据
  source('~/R/forecast/src/readData.R')
  tmp<-devicedata(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval = 10)
  data_10<-bind_rows(data_10,tmp)
  tmp<-devicedata(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval = 5)
  data_5<-bind_rows(data_5,tmp)
  print(paste(i,all_device[i,]$deviceName))
}

#验证YC，YM用的啥数
tmp<-data_5 %>%group_by(time = floor_date(ymd_hms(time), "10 mins")) %>%summarise(avg_TR008 = mean(na.omit(as.numeric(TR008))),
                                                                                  last_TR008 = last(as.numeric(TR008)),
                                                                                  avg_TM102 = mean(na.omit(as.numeric(TM102))),
                                                                                  last_TM102 = last(as.numeric(TM102))) 
View(merge(data_10,tmp,by="time"))

# daily_device<-data_10%>%
#   group_by(as.Date(time),deviceName)%>%
#   summarise(deviceName=first(deviceName),
#          d_power=last(as.numeric(na.omit(TR008)))-first(as.numeric(na.omit(TR008))),
#          p_power=sum(as.numeric(na.omit(TR002)))/6,
#          yf_power=sum(as.numeric(na.omit(YFTR002)))/6)
# 
# amount_device<-data_10%>%
#   group_by(deviceName)%>%
#   summarise(deviceName=first(deviceName),
#             d_power=last(as.numeric(na.omit(TR008)))-first(as.numeric(na.omit(TR008))),
#             p_power=sum(as.numeric(na.omit(TR002)))/6,
#             yf_power=sum(as.numeric(na.omit(YFTR002)))/6)
# 

############draft#######
# data_1<-devicechart(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval =2)
# 
# 
# dateRanges <- data.frame(
#   start = seq(as.POSIXct(as.Date(startime)+1), as.POSIXct(as.Date(endtime)), "2 day"),
#   end = seq(as.POSIXct(as.Date(startime)+2), as.POSIXct(as.Date(endtime)+1), "2 day")
# )
# 
# ggplot() + 
#   geom_line(data=data_1, aes(x=Time, y=TR012), color='red')+
#   geom_line(data=data_10, aes(x=Time, y=TR012), color='blue') +
#   geom_rect(data = dateRanges, aes(xmin = start , xmax = end, ymin = -Inf, ymax = Inf),
#             inherit.aes=FALSE, alpha = 0.4, fill = c("lightblue"))+facet_grid()
