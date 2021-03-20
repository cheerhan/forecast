request_body <- data.frame(
  stationCode="3344112",
  deviceFullCodes="3344112M206M1M1",
  startTime="2021-03-16T16:00:00Z",
  endTime="2021-03-17T06:52:39Z",
  devicePoints="[489615]",
  timeInterval="10",
  devicePoint="[]",
  enterpriseId="451436467886592"
)
my_subscrition_key<-c("bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJ0ZXN0YWRtaW4iLCJlbnRlcnByaXNlQ29kZSI6MTAxMCwidXNlckVudGVycHJpc2VTdGF0dXMiOjMsImF1dG8iOiIwIiwidXNlclN0YXR1cyI6MSwidXNlcl9uYW1lIjoidGVzdGFkbWluIiwiY29tcGFueU5hbWUiOm51bGwsInVzZXJGdWxsTmFtZSI6InRlc3RhZG1pbiIsInBob25lTnVtIjoiMTk4MTAwMDY1MzYiLCJ1c2VyTG9nbyI6Ii9pbWcvbm9waWMucG5nIiwidXNlcklkIjoiNDMyOTczNjA3MTUwMDgwIiwiYXV0aG9yaXRpZXMiOlsiYWRtaW4iXSwiY2xpZW50X2lkIjoiY25lZ3JvdXAiLCJzY29wZSI6WyJhbGwiLCJyZWFkIiwid3JpdGUiXSwiZW50ZXJwcmlzZUlkIjoiMzE2NjAzNDkzMjY5NTA0IiwiZXhwIjoxOTMxMzM2NDcwLCJlbnRlcnByaXNlTG9nbyI6Imh0dHBzOi8vcG93ZXIuY25lY2xvdWQuY29tL2FwaS92My9pbWFnZXMvZGU1NWI3NDJiMTQ2NDgwYjgyNDE4MGE2MzczOWNjOWMuUE5HIiwiZW50ZXJwcmlzZU5hbWUiOiLljY_lkIjmlrDog73mupAiLCJqdGkiOiIyY2E0NWZlYi1iYjM4LTQ2OWUtODFhYS00ZGIzMDhmOWNmZmUiLCJ1c2VybmFtZSI6InRlc3RhZG1pbiJ9.Oxx7UYWezJyv10mlE6WrHBvgYnJ3G4rIHQxwfi9u-9Q")

##Converting the Request body(Dataframe) to Request body(JSON)
require(jsonlite)
request_body_json <- toJSON(list(documents = request_body), auto_unbox = TRUE)

hdr=c('Authorization'="bearer eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJ0ZXN0YWRtaW4iLCJlbnRlcnByaXNlQ29kZSI6MTAxMCwidXNlckVudGVycHJpc2VTdGF0dXMiOjMsImF1dG8iOiIwIiwidXNlclN0YXR1cyI6MSwidXNlcl9uYW1lIjoidGVzdGFkbWluIiwiY29tcGFueU5hbWUiOm51bGwsInVzZXJGdWxsTmFtZSI6InRlc3RhZG1pbiIsInBob25lTnVtIjoiMTk4MTAwMDY1MzYiLCJ1c2VyTG9nbyI6Ii9pbWcvbm9waWMucG5nIiwidXNlcklkIjoiNDMyOTczNjA3MTUwMDgwIiwiYXV0aG9yaXRpZXMiOlsiYWRtaW4iXSwiY2xpZW50X2lkIjoiY25lZ3JvdXAiLCJzY29wZSI6WyJhbGwiLCJyZWFkIiwid3JpdGUiXSwiZW50ZXJwcmlzZUlkIjoiMzE2NjAzNDkzMjY5NTA0IiwiZXhwIjoxOTMxMzM2NDcwLCJlbnRlcnByaXNlTG9nbyI6Imh0dHBzOi8vcG93ZXIuY25lY2xvdWQuY29tL2FwaS92My9pbWFnZXMvZGU1NWI3NDJiMTQ2NDgwYjgyNDE4MGE2MzczOWNjOWMuUE5HIiwiZW50ZXJwcmlzZU5hbWUiOiLljY_lkIjmlrDog73mupAiLCJqdGkiOiIyY2E0NWZlYi1iYjM4LTQ2OWUtODFhYS00ZGIzMDhmOWNmZmUiLCJ1c2VybmFtZSI6InRlc3RhZG1pbiJ9.Oxx7UYWezJyv10mlE6WrHBvgYnJ3G4rIHQxwfi9u-9Q",
      'Content-Type'="application/json;charset=UTF-8",
      'Connection'='keep-alive',
      'X-Application-Context'= "cne-power-web-service:pro:8093",
      'Cache-Control'="no-cache, no-store, max-age=0, must-revalidate",
      'Content-Length'="2048")

require(httr)
result<-POST("https://power.cnecloud.com/api/v3/wind/analysis/history/devicechart",
               body=request_body_json,
               content_type_json(),
               add_headers(.headers = hdr)) 
output<-content(result)
postForm("https://power.cnecloud.com/api/v3/wind/analysis/history/devicechart",
         .opts=list(httpheader=hdr, postfields=request_body_json))

