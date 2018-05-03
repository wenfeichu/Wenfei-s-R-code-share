# we need quantmod for the getSymbols function
# install it if we don't have it yet
if (!require("quantmod")) install.packages("quantmod")
library(quantmod)


######################################################
###### Data Extraction from online database
######################################################

# GDP
# https://fred.stlouisfed.org/series/GDP
getSymbols("GDP", src = "FRED")
head(GDP, 2)
#              GDP
# 1947-01-01 243.080
# 1947-04-01 246.267

# Real Gross Domestic Product GDP/GNP
# https://fred.stlouisfed.org/series/A191RL1Q225SBEA
getSymbols("A191RL1Q225SBEA", src = "FRED")
head(A191RL1Q225SBEA, 2)
#           A191RL1Q225SBEA
# 1947-04-01            -0.4
# 1947-07-01            -0.4

# Gross Domestic Product: Implicit Price Deflator
# https://fred.stlouisfed.org/series/GDPDEF
getSymbols("GDPDEF", src = "FRED")
head(GDPDEF, 2)
#         GDPDEF
# 1947-01-01 12.566
# 1947-04-01 12.745

# Consumer Price Index for All Urban Consumers: All Items
# https://fred.stlouisfed.org/series/CPIAUCSL
getSymbols("CPIAUCSL", src = "FRED")
head(CPIAUCSL, 2)
#             CPIAUCSL
# 1947-01-01    21.48
# 1947-02-01    21.62

# Consumer Price Index for All Urban Consumers: All Items Less Food and Energy
getSymbols("CPILFESL", src = "FRED")
head(CPILFESL, 2)
#             CPILFESL
# 1957-01-01     28.5
# 1957-02-01     28.6

# Producer Price Index for All Commodities
# https://fred.stlouisfed.org/series/PPIACO
getSymbols("PPIACO", src = "FRED")
head(PPIACO, 2)
#             PPIACO
# 1913-01-01   12.1
# 1913-02-01   12.0

# Producer Price Index by Commodity for Final Demand: Finished Goods Less Foods and Energy
# https://fred.stlouisfed.org/series/WPSFD4131
getSymbols("WPSFD4131", src = "FRED")
head(WPSFD4131, 2)
#             WPSFD4131
# 1974-01-01      49.7
# 1974-02-01      50.0

#  Import Price Index (End Use): All commodities
#  https://fred.stlouisfed.org/series/IR
getSymbols("IR", src = "FRED")
head(IR, 5)
#           IR
# 1982-09-01 80.0
# 1982-10-01   NA
# 1982-11-01   NA
# 1982-12-01 79.9
# 1983-01-01   NA

# Export Price Index (End Use): All commodities (IQ)
# https://fred.stlouisfed.org/series/IQ
getSymbols("IQ", src = "FRED")
head(IQ, 5)
#             IQ
# 1983-09-01 85.7
# 1983-10-01   NA
# 1983-11-01   NA
# 1983-12-01 85.4
# 1984-01-01   NA