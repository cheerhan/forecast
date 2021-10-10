library(dplyr)
library(ggplot2)
library(lubridate)

##集团的header
hdr=c('Accept'="application/json, text/plain, */*",
      'Accept-Encoding'="gzip, deflate, br",
      'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
      'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJjbmVhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDEwLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJjbmVhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLljY_lkIjnrqHnkIblkZgiLCJwaG9uZU51bSI6IjE4NTE0NDY5MjQ0IiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjMxNjYwMzQwMDk5NDgxNiIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjMxNjYwMzQ5MzI2OTUwNCIsImV4cCI6MTkzNjg1MTQ0NCwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzL2RlNTViNzQyYjE0NjQ4MGI4MjQxODBhNjM3MzljYzljLlBORyIsImVudGVycHJpc2VOYW1lIjoi5Y2P5ZCI5paw6IO95rqQIiwianRpIjoiYmUyNmRjNmMtY2RkNC00ZjFkLTljZmQtYjAzNWEyOTUxZjgzIiwidXNlcm5hbWUiOiJjbmVhZG1pbiJ9.GCKqs6GYYCqKjREw9BZyPOtsAbEZ0c1hL75VEA11q5E",
      'Content-Type'="application/json;charset=UTF-8")
startime<-"2021-09-14T16:00:00Z"
endtime<-"2021-09-15T15:59:59Z"

enterpriseid<-"316603493269504" ##集团enterpriseid

# ##普洛斯的config：header
# hdr=c('Accept'="application/json, text/plain, */*",
#       'Accept-Encoding'="gzip, deflate, br",
#       'Accept-Language'="en,zh-CN;q=0.9,zh;q=0.8,la;q=0.7,ru;q=0.6" ,
#       'Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJwbHNhZG1pbiIsImVudGVycHJpc2VDb2RlIjoxMDIyLCJ1c2VyRW50ZXJwcmlzZVN0YXR1cyI6MywiYXV0byI6IjAiLCJ1c2VyU3RhdHVzIjoxLCJ1c2VyX25hbWUiOiJwbHNhZG1pbiIsImNvbXBhbnlOYW1lIjpudWxsLCJ1c2VyRnVsbE5hbWUiOiLmma7mtJvmlq_nrqHnkIblkZgiLCJwaG9uZU51bSI6IjE1NjExNDA5NTYyIiwidXNlckxvZ28iOm51bGwsInVzZXJJZCI6IjQ1MTQzNjQ2MzY5MjI4OCIsImF1dGhvcml0aWVzIjpbImFkbWluIl0sImNsaWVudF9pZCI6ImNuZWdyb3VwIiwic2NvcGUiOlsiYWxsIiwicmVhZCIsIndyaXRlIl0sImVudGVycHJpc2VJZCI6IjQ1MTQzNjQ2Nzg4NjU5MiIsImV4cCI6MTkzMTY1NDIzNSwiZW50ZXJwcmlzZUxvZ28iOiJodHRwczovL3Bvd2VyLmNuZWNsb3VkLmNvbS9hcGkvdjMvaW1hZ2VzLzBjMTM3OTczMDI1MDQ2ODY4OGM5M2ZmZmEyZTg3ODU1LmpwZyIsImVudGVycHJpc2VOYW1lIjoi5pmu5p6r5paw6IO95rqQIiwianRpIjoiY2IyMGVlM2YtMmI3NS00MjE0LWE4ZjYtNjdkODIzYWE0M2VkIiwidXNlcm5hbWUiOiJwbHNhZG1pbiJ9.qLK9ocTltrvrd8ameGhd8Sc_hLvAvhbjy8vKmmKdtVA",
#       'Content-Type'="application/json;charset=UTF-8")
# enterpriseid<-"451436467886592" ##普洛斯enterpriseid

#时间段
startime<-"2021-08-13T16:00:00Z"
endtime<-"2021-08-14T15:59:59Z"
endtime<-Sys.time() %>%
  as.POSIXct("Asia/Shanghai")%>%
  format(tz="UTC")%>%
  gsub(pattern=" ",replacement = "T",time)%>%
  paste("Z",sep = "") #当前时间

source('~/R/forecast/src/readPvStation.R')
all_station<-station(hdr)
# all_station<-all_station%>%arrange(as.numeric(stationUnitCount))
all_station<-filter(all_station,stationName%in%c("乃东光伏电站"))
# all_station<-filter(all_station,stationName%in%c("乃东光伏电站","措美","江孜","哈尔滨聚合")&stationStatus!='500')


pro_device<-data.frame()
for(j in 1:length(all_station$stationCode)){
  stationCode<-all_station[j,]$stationCode
  print(paste(j,all_station[j,]$stationName))
  
  source('~/R/forecast/src/readDevice.R')
  all_device<-device(hdr,station=stationCode)
  
  data<-data.frame()
  for(i in 1:length(all_device$deviceCode)){
    device<-all_device[i,]
    
    #使用readPoint获得设备列表的部分测点
    source('~/R/forecast/src/readPoint.R')
    point<-point(hdr,device$deviceId)
    #直流功率："NB003",交流功率："NB018","NB031","NB034"
    point<-filter(point,devicePointCode %in% c("NB018","NB037"))
    tryCatch({ 
      #使用readHistory获取测点数据
      source('~/R/forecast/src/readData.R')
      tmp<-devicedata(hdr,stationcode=stationCode,device$deviceCode,startime,endtime,point$devicePointId,enterpriseid,timeInterval = 2)
      tmp<-tmp%>%
        mutate(device_des=paste(deviceName,deviceModeName,sep="_"))%>%
        mutate(status=case_when(
          NB037=="1"~"正常发电",NB037=="2"~"外部限功率",NB037=="3"~"内部限功率",NB037=="4"~"技术待机",
          NB037=="5"~"环境停机",NB037=="6"~"命令停机",NB037=="7"~"限功率停机",NB037=="8"~"计划运维",
          NB037=="9"~"技术改造",NB037=="10"~"故障停机",NB037=="11"~"故障停机",NB037=="12"~"信息中断",
          NB037=="13"~"工作停滞",NB037=="14"~"不可抗力",TRUE~NB037))
    
      ##查看设备状态数据
      tmp$NB018<-as.numeric(tmp$NB018)
      data<-bind_rows(data,tmp)
      tmp<-select(filter(data,!status%in%c(1:14,NA)),c("stationName","deviceModeName","deviceName"))
      tmp<-tmp[!duplicated(tmp),]
      pro_device<-bind_rows(pro_device,tmp)
      }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
    print(paste(i,all_device[i,]$deviceName))
  }
  g3<-ggplot(data=data,aes(x=time,y=NB018,colour=status))+
    geom_line()+
    geom_rug()+
    theme(text = element_text(family = "Noto Sans CJK SC"),
          legend.position="bottom", legend.box = "horizontal") +
    facet_wrap(~device_des)
###########
  # state<-data %>%
  #   group_by(deviceName) %>%
  #   mutate(diffNB037 = as.numeric(NB037)- lag(as.numeric(NB037), default = 0))%>%
  #   filter(diffNB037!=0)%>%
  #   arrange(time)%>%
  #   mutate(end_time = c(time[-1]-1,as.POSIXct(gsub(pattern="T",replacement = " ",endtime),tz="UTC")+hours(8)))%>%
  #   mutate(period=difftime(end_time,time,units = "hours"))%>%
  #   select(c("deviceName","time","end_time"  , "period","status"))

  # g3<-qplot(ymin = time,ymax = end_time,x = deviceName,colour = status,geom = "linerange",data = state,size = I(5)) +
  #   coord_flip() +
  #   theme_bw() +
  #   theme(panel.grid = element_blank()) +
  #   theme(text = element_text(family = "Noto Sans CJK SC"),
  #         legend.position="bottom", legend.box = "horizontal")####
  
  path<-file.path("checkgen31")
  if(!dir.exists(path))
     dir.create(path,recursive = TRUE)

  jpeg(filename = paste0(path,"/",all_station[j,]$stationName,'.jpeg'),width = 1800,height = 1000,units = "px")
  plot(g3)
  dev.off()
}


