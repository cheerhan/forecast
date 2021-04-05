library(rjson)
library(dplyr)
library(tidyr)
library(httr)

point <- function(hdr,deviceId){
    request<-list(
      deviceIds=deviceId,
      devicePointTypes=c("YM","YC"),
      showWeather=1
    )
    request <- toJSON(request)
    result<-POST("https://power.cnecloud.com/api/v3/wind/analysis/devicepoint",
                 body=request,
                 content_type_json(),
                 add_headers(.headers = hdr))
    output<-content(result)
    points<-as.data.frame(do.call(rbind, lapply(output$data, as.vector)))
    points<-unnest(points,cols = c(devicePointId, devicePointName, devicePointIecCode, devicePointUnit, 
                                   devicePointCode, isChecked, devicePointIecName))
    return(points)
}
