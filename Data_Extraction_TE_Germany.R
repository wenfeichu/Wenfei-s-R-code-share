#######  Replication data retrieve

install.packages("rjson")
## 1
## data importing
# library using rjson to import
library(rjson)
jsonCPI = fromJSON(file = "http://markets.prod.services.amana.vpn/api/app/markets/tradingeconomics/historical/country/united%20states/indicator/consumer%20price%20index%20cpi")

# generate empty data set
CPIdata = data.frame(rep(0, length(jsonCPI)))
CPIdata$value = c(0)
colnames(CPIdata) = c("dateTime", "value")

# extract CPI              
for(i in seq(from=1, to=length(jsonCPI))){
  item = jsonCPI[[i]]  
  CPIdata$dateTime[i] = item$dateTime
  CPIdata$value[i] = item$value
}
write.csv(CPIdata,"Documents/CPIdata.csv")
##2
## GDP deflator
jsonGDP = fromJSON(file = "http://markets.prod.services.amana.vpn/api/app/markets/tradingeconomics/historical/country/united%20states/indicator/gdp%20growth%20rate")

# generate empty data set
GDPdata = data.frame(rep(0, length(jsonGDP)))
GDPdata$value = c(0)
colnames(GDPdata) = c("dateTime", "value")

for(i in seq(from=1, to=length(jsonGDP))){
  item = jsonGDP[[i]]  
  GDPdata$dateTime[i] = item$dateTime
  GDPdata$value[i] = item$value
}

## 3
## Core Consumer Price
jsonCCPI = fromJSON(file = "http://markets.prod.services.amana.vpn/api/app/markets/tradingeconomics/historical/country/united%20states/indicator/core%20consumer%20prices")

# generate empty data set
CCPIdata = data.frame(rep(0, length(jsonCCPI)))
CCPIdata$value = c(0)
colnames(CCPIdata) = c("dateTime", "value")


for(i in seq(from=1, to=length(jsonCCPI))){
  item = jsonCCPI[[i]]  
  CCPIdata$dateTime[i] = item$dateTime
  CCPIdata$value[i] = item$value
}

write.csv(CCPIdata,"Documents/CCPIdata.csv")
## 4
## 
jsonPPI = fromJSON(file = "http://markets.prod.services.amana.vpn/api/app/markets/tradingeconomics/historical/country/united%20states/indicator/producer%20prices")

# generate empty data set
PPIdata = data.frame(rep(0, length(jsonPPI)))
PPIdata$value = c(0)
colnames(PPIdata) = c("dateTime", "value")


for(i in seq(from=1, to=length(jsonPPI))){
  item = jsonPPI[[i]]  
  PPIdata$dateTime[i] = item$dateTime
  PPIdata$value[i] = item$value
}
write.csv(PPIdata,"Documents/PPIdata.csv")
## 5
## IPI Import Price
jsonIPI = fromJSON(file = "http://markets.prod.services.amana.vpn/api/app/markets/tradingeconomics/historical/country/united%20states/indicator/import%20prices")

# generate empty data set
IPIdata = data.frame(rep(0, length(jsonIPI)))
IPIdata$value = c(0)
colnames(IPIdata) = c("dateTime", "value")


for(i in seq(from=1, to=length(jsonIPI))){
  item = jsonIPI[[i]]  
  IPIdata$dateTime[i] = item$dateTime
  IPIdata$value[i] = item$value
}
write.csv(IPIdata,"Documents/IPIdata.csv")

## 6
## EPI Export Price
jsonEPI = fromJSON(file = "http://markets.prod.services.amana.vpn/api/app/markets/tradingeconomics/historical/country/united%20states/indicator/export%20prices")

# generate empty data set
EPIdata = data.frame(rep(0, length(jsonEPI)))
EPIdata$value = c(0)
colnames(EPIdata) = c("dateTime", "value")


for(i in seq(from=1, to=length(jsonEPI))){
  item = jsonEPI[[i]]  
  EPIdata$dateTime[i] = item$dateTime
  EPIdata$value[i] = item$value
}
write.csv(EPIdata,"Documents/EPIdata.csv")

## 7
## GDP Deflator

jsonDef = fromJSON(file = "http://markets.prod.services.amana.vpn/api/app/markets/tradingeconomics/historical/country/united%20states/indicator/gdp%20deflator")

# generate empty data set
Defdata = data.frame(rep(0, length(jsonDef)))
Defdata$value = c(0)
colnames(Defdata) = c("dateTime", "value")


for(i in seq(from=1, to=length(jsonDef))){
  item = jsonDef[[i]]  
  Defdata$dateTime[i] = item$dateTime
  Defdata$value[i] = item$value
}
write.csv(CPIdata,"Documents/GDPdefdata.csv")

## 8
## Inflation MoM

jsonINFMOM = fromJSON(file = "http://markets.prod.services.amana.vpn/api/app/markets/tradingeconomics/historical/country/united%20states/indicator/inflation%20rate%20mom")

# generate empty data set
INFMOMdata = data.frame(rep(0, length(jsonINFMOM)))
INFMOMdata$value = c(0)
colnames(INFMOMdata) = c("dateTime", "value")


for(i in seq(from=1, to=length(jsonINFMOM))){
  item = jsonINFMOM[[i]]  
  INFMOMdata$dateTime[i] = item$dateTime
  INFMOMdata$value[i] = item$value
}
write.csv(INFMOMdata,"Documents/Inflationmom.csv")
## 9  
## Inflation Expectation

jsonINFEXP = fromJSON(file = "http://markets.prod.services.amana.vpn/api/app/markets/tradingeconomics/historical/country/united%20states/indicator/inflation%20expectations")

# generate empty data set
INFEXPdata = data.frame(rep(0, length(jsonINFEXP)))
INFEXPdata$value = c(0)
colnames(INFEXPdata) = c("dateTime", "value")


for(i in seq(from=1, to=length(jsonINFEXP))){
  item = jsonINFEXP[[i]]  
  INFEXPdata$dateTime[i] = item$dateTime
  INFEXPdata$value[i] = item$value
}


set<-data.frame(CPIdata$value,CCPIdata$value)
