

install.packages("devtools")
library(devtools)
install_github("ieconomics/open-api/R/tradingeconomics")
library(tradingeconomics)



#####     Local - Trading Economics     #####
library(httr)
library(jsonlite)
library(xts)
library(ggplot2)

endpoint <- "http://markets.prod.services.amana.vpn/api/app/markets/tradingeconomics/historical/country/"
country <- "switzerland"
indicator <- "consumer price index cpi"
url <- paste0(endpoint, URLencode(country), "/indicator/" ,URLencode(indicator))

request <- GET(url)
if (request$status_code != 200) return(NULL)

json_data <- jsonlite::fromJSON(content(request, "text"))
json_data <- jsonlite::flatten(json_data)
indicator_df <- as.data.frame(json_data)

indicator_xts <- xts(indicator_df$value, order.by = as.POSIXct(indicator_df$dateTime, format = "%Y-%m-%dt"))
autoplot(indicator_xts, geom = "line") + labs(x = "", y = "", title = indicator)
##################################################

#####     Remote - Trading Economics     #####

# let's use the trading economics api
user <- "jezgowxfm638ojq"
passwd <- "f58grkulnor830k"

request <- paste0("?client=",user,":",passwd,"&format=json")

country <- "switzerland"
indicator <- "consumer price index cpi"

endpoint <- paste0("https://api.tradingeconomics.com/historical/country/")
url <- paste0(endpoint, URLencode(country), "/indicator/" ,URLencode(indicator), "/", request)

request <- GET(url)
if (request$status_code != 200) return(NULL)

json_data <- jsonlite::fromJSON(content(request, "text"))
json_data <- jsonlite::flatten(json_data)
indicator_df <- as.data.frame(json_data)

# field name  "DateTime" is different from "dateTime"
# field name "Value" is different from "value"
indicator_xts <- xts(indicator_df$Value, order.by = as.POSIXct(indicator_df$DateTime, format = "%Y-%m-%dT00:00:00"))
autoplot(indicator_xts, geom = "line") + labs(x = "", y = "", title = indicator)
##################################################
request <- paste0("?client=",user,":",passwd,"&format=json")
country <- "switzerland"
indicator <- "producer prices"

endpoint <- paste0("https://api.tradingeconomics.com/historical/country/")
url <- paste0(endpoint, URLencode(country), "/indicator/" ,URLencode(indicator), "/", request)

request <- GET(url)
if (request$status_code != 200) return(NULL)

json_data <- jsonlite::fromJSON(content(request, "text"))
json_data <- jsonlite::flatten(json_data)
indicator_df <- as.data.frame(json_data)

# field name  "DateTime" is different from "dateTime"
# field name "Value" is different from "value"
indicator_xts2 <- xts(indicator_df$Value, order.by = as.POSIXct(indicator_df$DateTime, format = "%Y-%m-%dT00:00:00"))
autoplot(indicator_xts2, geom = "line") + labs(x = "", y = "", title = indicator)

##################################################
request <- paste0("?client=",user,":",passwd,"&format=json")
country <- "switzerland"
indicator <- "GDP Deflator"

endpoint <- paste0("https://api.tradingeconomics.com/historical/country/")
url <- paste0(endpoint, URLencode(country), "/indicator/" ,URLencode(indicator), "/", request)

request <- GET(url)
if (request$status_code != 200) return(NULL)

json_data <- jsonlite::fromJSON(content(request, "text"))
json_data <- jsonlite::flatten(json_data)
indicator_df <- as.data.frame(json_data)

# field name  "DateTime" is different from "dateTime"
# field name "Value" is different from "value"
indicator_xts3 <- xts(indicator_df$Value, order.by = as.POSIXct(indicator_df$DateTime, format = "%Y-%m-%dT00:00:00"))
autoplot(indicator_xts3, geom = "line") + labs(x = "", y = "", title = indicator)


### Swiss data
inflation.suisse=merge(indicator_xts,indicator_xts2,indicator_xts3)

begin_date="1980-03-31"
end_date="2017-12-31"
# Cutoff for data
bpoint=which(time(inflation.suisse)==begin_date)
epoint=which(time(inflation.suisse)==end_date)
inflation.suisse.cut=inflation.suisse[bpoint:epoint,]
cpippi.suisse=inflation.suisse.cut[,-3]
colnames(cpippi.suisse)=c("CPI","PPI")

## QTM and MTQ change
library(TTR)
inflation.mom.suisse=ROC(cpippi.suisse)
inflation.mom.suisse=inflation.mom.suisse[!is.na(inflation.mom.suisse[,1]),]
inflation.qoq.suisse=apply.quarterly(inflation.mom.suisse,colSums)
GDPDEF.qoq.suisse=ROC(indicator_xts3)
colnames(GDPDEF.qoq.suisse)="GDPDEF"
GDPDEF.qoq.suisse=GDPDEF.qoq.suisse[!is.na(GDPDEF.qoq.suisse)]

mom.suisse=merge(inflation.mom.suisse,GDPDEF.qoq.suisse)
GDPDEF.mom.suisse=mom.suisse$GDPDEF
View(GDPDEF.mom.suisse)

CPI.qoq.suisse=inflation.qoq.suisse$CPI
PPI.qoq.suisse=inflation.qoq.suisse$PPI