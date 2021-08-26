library(dplyr)
library(tidyr)

##普洛斯的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJwbHNhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDIyLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJwbHNhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLmma7mtJvmlq_nrqHnkIblkZgiLCJwaG9uZU51bSI6IjE1NjExNDA5NTYyIiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjQ1MTQzNjQ2MzY5MjI4OCIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjQ1MTQzNjQ2Nzg4NjU5MiIsImV4cCI6MTkzMTY1NDIzNSwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzLzBjMTM3OTczMDI1MDQ2ODY4OGM5M2ZmZmEyZTg3ODU1LmpwZyIsImVudGVycHJpc2VOYW1lIjoi5pmu5p6r5paw6IO95rqQIiwianRpIjoiY2IyMGVlM2YtMmI3NS00MjE0LWE4ZjYtNjdkODIzYWE0M2VkIiwidXNlcm5hbWUiOiJwbHNhZG1pbiJ9.qLK9ocTltrvrd8ameGhd8Sc_hLvAvhbjy8vKmmKdtVA",      'Content-Type'="application/json;charset=UTF-8")
##集团的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjbmVhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDEwLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJjbmVhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLljY_lkIjnrqHnkIblkZgiLCJwaG9uZU51bSI6IjE4NTE0NDY5MjQ0IiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjMxNjYwMzQwMDk5NDgxNiIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjMxNjYwMzQ5MzI2OTUwNCIsImV4cCI6MTkzNjg1MTQ0NCwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzL2RlNTViNzQyYjE0NjQ4MGI4MjQxODBhNjM3MzljYzljLlBORyIsImVudGVycHJpc2VOYW1lIjoi5Y2P5ZCI5paw6IO95rqQIiwianRpIjoiYmUyNmRjNmMtY2RkNC00ZjFkLTljZmQtYjAzNWEyOTUxZjgzIiwidXNlcm5hbWUiOiJjbmVhZG1pbiJ9.GCKqs6GYYCqKjREw9BZyPOtsAbEZ0c1hL75VEA11q5E",      
      'Content-Type'="application/json;charset=UTF-8")
# 使用readStation获得所有电站
source('~/R/forecast/readStation.R')
station<-station(hdr)

startime<-"2021-8-11T16:00:00Z"
endtime<-"2021-08-12T15:59:59Z"
##集团enterpriseid
enterpriseid<-"316603493269504"
#乃东
stationCode<-"504"

# 使用readDevice获得某个电站所有206、201
source('~/R/forecast/readDevice.R')
device<-device(hdr,station=stationCode)

#使用readPoint获得设备列表的部分测点。
source('~/R/forecast/readPoint.R')
point<-point(hdr,device$deviceId)

#创建单电站所有设备某几个测点request。直流功率（NB003），并网功率（NB018），日发电量（NB031)，总累计发发电量（NB034）
points<-c("NB035","NB031","NB034")
point<-filter(point,devicePointCode %in% points)

#使用readHistory获取测点数据
source('~/R/forecast/readHistory.R')
data<-devicechart(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid)
data<-gather(data,key = "point",value="value",-deviceCode,-deviceName,-Time)

quality<-function(value){
  # 仅需要最后一列测试NB018
  diff<-abs(diff(value))
  diff<-c(diff[1],diff)
  jump <-boxplot.stats(diff,coef = 6)$out
  value<- case_when(is.na(diff)~"Missing",
                  diff==0~"Dead",
                  diff%in%jump~"Jump",
                  TRUE~"Valid")
}

data<-data%>%group_by(deviceName,as.Date(Time),point)%>%mutate(quality=quality(value))

library(ggplot2)
library(manipulate)
manipulate(ggplot(data=filter(data,deviceName==devicename.val),aes(x=Time,y=value))+
             geom_line()+
             geom_point(aes(colour=quality))+
             facet_wrap(vars(point), ncol = 1,scales = "free_y"),
           devicename.val=picker(as.list(unique(data$deviceName))))


# 仅需要最后一列测试NB018
diff<-abs(diff(data$NB018))
diff<-c(diff[1],diff)
jump <-boxplot.stats(diff,coef = 6)$out
value<- case_when(is.na(diff)~"M",
                  diff==0~"D",
                  diff%in%jump~"J",
                  TRUE~"V")
data<-bind_cols(data,value=as.factor(value))
         
with(data,{plot(NB018~Time,type="l")
  points(NB018~Time,col=value)})


# 仅需要最后一列测试NB034
diff<-abs(diff(data$NB034))
diff<-c(diff[1],diff)
jump <-boxplot.stats(diff,coef = 10)$out
value<- case_when(is.na(diff)~"blue",
                  diff==0~"black",
                  diff%in%jump~"red",
                  TRUE~"green")
data<-bind_cols(data,value=as.factor(value))
         
with(data,{plot(NB034~Time,type="l")
  points(NB034~Time,col=value)})



# ##画图测试
# lattice::xyplot(NB031~Time|deviceName,data=data)
# psych::describeBy(data[],group=data$deviceName)

#寻找跳变值
# 1. 计算diff
diff_18<-c(NA,diff(nb018))
# 2.去掉diff中的0，然后寻找outlier
diff_18[diff_18==0]<-NA
boxplot.stats(diff_18,coef = 6)$out
# 3.定位跳变点
jump_18<-nb018
diff_18[!(diff_18 %in% boxplot.stats(diff_18,coef = 6)$out)]<-NA
jump_18[is.na(diff_18)]<-NA

par(family="SourceHanSans-Regular")
with(data,{plot(nb018~Time,type="l") 
  points(jump_18~data$Time,col="red")})

