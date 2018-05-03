######################################################
###### Statistical Modelling
######################################################
### Fitted Model
## Model1 Full model

model1<- lm(realinf~GDPDEF+CPIAUCSL+CPILFESL+PPIACO+WPSFD4131+IR+IQ,data=mergedata)
summary(model1)


## Stepwise Model 
stepwise(model1, direction="backward/forward", criterion=c("AIC"))

stepwise1=step(model1, direction = "both")
summary(stepwise1)

## Model 2 (Reduced with lag term)
model2<-lm(realinf~lag(realinf)+CPIAUCSL+CPILFESL+IQ,data=mergedata)
summary(model2)

## Model 3 Equally Weighted Average with realinf
model3<-lm(realinf~ewmean,data = mergedata)
summary(model3)

## Translate from R into Latex
stargazer(model1,stepwise1,model2)

### Time Series Model 

## ARIMA Model
acf(mergedata$ewmean, lag.max=20, plot=FALSE) # get the values of the autocorrelations
acf(mergedata$ewmean, lag.max=20)

## Partial Auto Correlation
pacf(mergedata$ewmean, lag.max=20)
pacf(mergedata$ewmean, lag.max=20, plot=FALSE)

## Auto Correlation Plot
acf(mergedata$mommean, lag.max=20, plot=FALSE) # get the values of the autocorrelations
acf(mergedata$mommean, lag.max=20)

## Partial Auto Correlation
pacf(mergedata$mommean, lag.max=20)
pacf(mergedata$mommean, lag.max=20, plot=FALSE)

## Auto ARIMA
library(forecast)
auto.arima(mergedata$ewmean,ic="bic")

## Arima Forecast
forecast.Arima(mergedata$ewmean, h=5, level=c(99.5))
auto.arima(mergedata$mommean,ic="bic")

## Box Test
Box.test(kingstimeseriesforecasts$residuals, lag=20, type="Ljung-Box")

## In ARIMA (2,0,0)
forcastarima <- arima(mergedata$ewmean, order=c(2,0,0))

## MIDAS Model
originmom=momroc[,2:7]
cpimom=originmom$CPIAUCSL
mommatrix=as.matrix(originmom)
mommean=as.vector(mergedata$mommean)

## as.vector to change ts in vec
cpi=mommatrix[,1]
ccpi=mommatrix[,2]
ppi=mommatrix[,3]
cppi=mommatrix[,4]
IR=mommatrix[,5]
IQ=mommatrix[,6]
Inflation=as.vector(mergedata$realinf)

## MIDAS regression model
# Full model
library(midasr)
GDPdefqoq=qoqroc
midasmodel1<-lm(GDPdefqoq~mls(cpi, k = 0:1 , m = 3)+mls(ccpi, k = 0:1, m = 3)+mls(ppi, k = 0:1 , m = 3)+
                  mls(cppi, k = 0:1 , m = 3)+mls(IR, k = 0:1 , m = 3)+mls(IQ, k = 0:1 , m = 3))

summary(midasmodel1)

# Restricted model with equally weighted mean
lagqoq=as.vector(lag(GDPdefqoq))
qoqrocvec=as.vector(GDPdefqoq)
midasmodel2<-lm(GDPdefqoq~lagqoq+mls(cpi, k = 0:1 , m = 3)+mls(ccpi, k = 0:1, m = 3)+mls(ppi, k = 0:1 , m = 3)+
                  mls(cppi, k = 0:1 , m = 3)+mls(IR, k = 0:1 , m = 3)+mls(IQ, k = 0:1 , m = 3))


summary(midasmodel2)

### Restricted model

## Midas Model 
midasmodel3<-lm(GDPdefqoq~lagqoq+mls(mommean,k=0:1,m=3))
midasmodel3<-midas_r(qoqrocvec~lagqoq+mls(mommean, 0:2, 3)+mls(Inflation, 0:2, 3),start = NULL)

## Translate into Latex
# summary(midasmodel3)
# stargazer(midasmodel1)
# stargazer(midasmodel1,midasmodel2)
# stargazer(midasmodel3)
# stargazer(midasmodel1,midasmodel2,midasmodel3)


## mom data mean
mergedata$mommean=rowMeans(mergedata[,2:7])
acf(mergedata$mommean, lag.max=20, plot=FALSE) # get the values of the autocorrelations
acf(mergedata$mommean, lag.max=20)

## Partial Auto Correlation
pacf(mergedata$mommean, lag.max=20)
pacf(mergedata$mommean, lag.max=20, plot=FALSE)


## Rename 
CCPI=originmom$CPILFESL
PPI=originmom$PPIACO
CPPI=originmom$WPSFD4131
IMPI=originmom$IR
EXPI=originmom$IQ

## Subplot
par(mfrow=c(3,2))
plot(CPI)
plot(CCPI)
plot(PPI)
plot(CPPI)
plot(IMPI)
plot(EXPI)
dev.off()
par(mfrow=c(1,1))

## QOQ data plot

DEFroc=ROC(GDPDEF)
GDPset=merge(GDPDEF,DEFroc)
plot(GDPset$DEFroc)

## Regression Result to Latex
stargazer(model1)


## HP Filter

# Trend and level for GDP Deflator
require(MASS)
GDPdef.hpfilter=mFilter::hpfilter(GDPdefqoq, freq = 4, type="frequency",drift = TRUE)
library(ggplot2)
plot(time(GDPdefqoq),GDPdef.hpfilter$trend,type = "l",xlab="Time",ylab = "GDP Deflator Trend",main="US inflation trend")
plot(time(GDPdefqoq),GDPdef.hpfilter$cycle,type = "l",xlab="Time",ylab = "GDP Deflator Trend",main="US inflation trend")
lines(x,col="red")


## starting value for Kalman filter
VaRresult2=VARmodel2$varresult
coefmatrix=rbind(VaRresult2$CPIAUCSL$coefficients,VaRresult2$CPILFESL$coefficients,
                 VaRresult2$PPIACO$coefficients,VaRresult2$WPSFD4131$coefficients,
                 VaRresult2$IR$coefficients,VaRresult2$IQ$coefficients)

## Variable and regression generation
GDPDEF=qoqroc
CPI=mtoq$CPIAUCSL
CCPI=mtoq$CPILFESL
PPI=mtoq$PPIACO
CPPI=mtoq$WPSFD4131
Import=mtoq$IR
Export=mtoq$IQ

## No constant linear model (estimation for initial value)
modelfull<-lm(GDPDEF~CPI+CCPI+PPI+CPPI+Import+Export-1)
summary(modelfull)
