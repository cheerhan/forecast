library(jsonlite)
library(dplyr)
library(tidyr)
library(httr)

point <- function(hdr,deviceId){
    request<-list(
      deviceIds=as.vector(unlist(deviceId, use.names = FALSE)),
      devicePointTypes=vector(),
      showWeather=1
    )
    request<- jsonlite::toJSON(request)
    result<-POST("https://power.cnecloud.com/api/v3/wind/analysis/devicepoint",
                 body=request,
                 content_type_json(),
                 add_headers(.headers = hdr))
    output<-content(result)
    points<-as.data.frame(do.call(rbind, lapply(output$data, as.vector)))
    # points<-unnest(points,cols = c(devicePointId, devicePointName, devicePointIecCode, devicePointUnit, 
                                   # devicePointCode, isChecked, devicePointIecName), .preserve = NULL)
    return(points)
}
