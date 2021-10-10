library(httr)

##根据stationCode获得deviceId
stationID<-function(hdr,stationCode){
  output<-content(GET(url = "https://power.cnecloud.com/api/v3/base/device/devices/prev",
                add_headers(hdr),
                query = list("stationCode"=stationCode, "deviceTypeCode" = "801")))
  device <- as.data.frame(do.call(rbind, lapply(output$data, as.vector)))
  return(device)
}


##获得所有电站的设备
# device<-function(hdr){
#   device<-data.frame()
#   stationcode<- as.vector(station$stationCode)
#   
#   for (i in 1:length(stationcode)) {
#     output<-content(GET(url = "https://power.cnecloud.com/api/v3/base/device/devices/prev", 
#                 add_headers(hdr),
#                 query = list("stationCode"=stationcode[i], "deviceTypeCode" = "206")))
#     tmp <- as.data.frame(do.call(rbind, lapply(output$data, as.vector)))
#     if(i==1){device<-tmp}
#     else{device<-rbind(device,tmp)}
#   }
#   return(device)
# }

  
  
  