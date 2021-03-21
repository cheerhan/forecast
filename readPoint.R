library(rjson)
library(dplyr)
library(tidyr)
library(httr)
library(lubridate)

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
    return(points)
}
point(hdr,out$deviceId)
