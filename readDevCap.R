library(httr)
library(dplyr)
##根据stationCode获得deviceId
deviceCap<-function(hdr,stationCode){
  output<-content(GET(url = paste("https://power.cnecloud.com/api/v3/monitor/seriesinverter/datalist",stationCode,"206",sep = "/"),
                      add_headers(hdr)))
  device <- as.data.frame(do.call(rbind, lapply(output$data$deviceList, as.vector)))
  output<-content(GET(url = paste("https://power.cnecloud.com/api/v3/monitor/seriesinverter/datalist",stationCode,"201",sep = "/"),
                      add_headers(hdr)))
  device <- rbind(device,as.data.frame(do.call(rbind, lapply(output$data$deviceList, as.vector))))
  device <- device%>%select(5:ncol(device))%>%relocate(deviceCapacity,.after = deviceName)
  return(device)
}
