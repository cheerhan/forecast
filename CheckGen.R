library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)
library(psych)

# 使用readStation获得所有电站
source('~/R/forecast/readStation.R')
station<-station(hdr)

##普洛斯的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJwbHNhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDIyLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJwbHNhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLmma7mtJvmlq_nrqHnkIblkZgiLCJwaG9uZU51bSI6IjE1NjExNDA5NTYyIiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjQ1MTQzNjQ2MzY5MjI4OCIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjQ1MTQzNjQ2Nzg4NjU5MiIsImV4cCI6MTkzMTY1NDIzNSwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzLzBjMTM3OTczMDI1MDQ2ODY4OGM5M2ZmZmEyZTg3ODU1LmpwZyIsImVudGVycHJpc2VOYW1lIjoi5pmu5p6r5paw6IO95rqQIiwianRpIjoiY2IyMGVlM2YtMmI3NS00MjE0LWE4ZjYtNjdkODIzYWE0M2VkIiwidXNlcm5hbWUiOiJwbHNhZG1pbiJ9.qLK9ocTltrvrd8ameGhd8Sc_hLvAvhbjy8vKmmKdtVA",      'Content-Type'="application/json;charset=UTF-8")

startime<-"2021-02-28T16:00:00Z"
endtime<-"2021-03-01T15:59:59Z"
enterpriseid<-"451436467886592"
stationCode<-"3344212"

# 使用readDevice获得某个电站所有206、201，上海宝山，滁州盼盼
source('~/R/forecast/readDevice.R')
device<-device(hdr,station=stationCode)
device<-device[1,]
#使用readPoint获得设备列表的部分测点。
source('~/R/forecast/readPoint.R')
point<-point(hdr,device$deviceId)

#创建单电站所有设备某几个测点request。直流功率（NB003），并网功率（NB018），日发电量（NB031)，总累计发发电量（NB034）
points<-c("NB018","NB031","NB034","QX002","QX012")
point<-filter(point,devicePointCode %in% points)

#使用readHistory获取测点数据
source('~/R/forecast/readHistory.R')
data<-devicechart(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId)




##画图测试
lattice::xyplot(NB031~Time|deviceName,data=data)
psych::describeBy(data[],group=data$deviceName)

with(data,{plot(nb018~Time,type="l") 
  points(jump_18~data$Time,col="red")})

#寻找跳变值
# 1. 计算diff
diff_18<-c(NA,diff(nb018))
# 2.去掉diff中的0，然后寻找outlier
diff_18[diff_18==0]<-NA
boxplot.stats(diff_18,coef = 10)$out
# 3.定位跳变点
jump_18<-nb018
diff_18[!(diff_18 %in% boxplot.stats(diff_18,coef = 6)$out)]<-NA
jump_18[is.na(diff_18)]<-NA
