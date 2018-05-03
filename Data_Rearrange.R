######################################################
###### Data Process and structured
######################################################

### Data Input
## General merge dataset of inflation data
data=merge(GDPDEF, CPIAUCSL,CPILFESL, PPIACO, WPSFD4131, IR, IQ)
drop<-c("datetime")

inflation=data.frame(data)
inflation$datetime=time(data)
library(zoo)

# Cut off date
cut_off_date="1966-01-01"
indice=inflation$datetime==cut_off_date

#Cut off Position in the time series
cutoff=which(inflation$datetime ==cut_off_date)

# End time              
endpoint=length(inflation$datetime)
inf_cut=inflation[cutoff:endpoint,]

# View(inf_cut)

# Inflation data components
inflation=data.frame(data)
data=data[,!(names(data) %in% drop)]
inf_rate=matrix(, nrow = dim(inf_cut)[1]-1, ncol = dim(inf_cut)[2]-1)
inf_cut=inf_cut[,!(names(inf_cut) %in% drop)]


## drop varible for inflation rate
install.packages("TTR")
library(TTR)
GDPDEFQOQ=ROC(GDPDEF)
drops<-c("GDP","GDPDEF")
GDPDEFQOQ=GDPDEFQOQ[which(time(GDPDEFQOQ)==cut_off_date):length(GDPDEFQOQ)]
mergedata=merge(GDPDEFQOQ,Infrate)

### Generate data with longer time period
# begin end 

begin_date="1947-01-01"
end_date="2017-01-01"

# time=time(as.timeSeries(data))
bpoint=which(time(PPIACO)==begin_date)
epoint=which(time(PPIACO)==end_date)
PPIACO=PPIACO[bpoint:epoint,]

point=which(time(CPIAUCSL)==end_date)
CPI=CPIAUCSL[1:epoint,]
epoint=which(time(GDPDEF)==end_date)
GDPDEF=GDPDEF[1:endpoint,]
GDPDEF=GDPDEF[1:epoint,]

## Merge data from 1947-01-01 to 2017-01-01 with GDPDE, CPI, PPI
mom.long.merge=merge(GDPDEF,CPI,PPIACO)
mom.CPI=ROC(CPI)[2:length(CPI)]
mom.PPI=ROC(PPIACO)[2:length(PPIACO)]
mom.cpippi=merge(mom.CPI,mom.PPI)
period=3
qoq.GDPDEF=ROC(GDPDEF)[2:length(GDPDEF),]
library(lubridate)
index(qoq.GDPDEF)=rollback(index(qoq.GDPDEF))
index(mom.CPI)=rollback(index(mom.CPI))
index(mom.PPI)=rollback(index(mom.PPI))

library(xts)
qoq.CPI=apply.quarterly(mom.CPI,colSums)
qoq.PPI=apply.quarterly(mom.PPI,colSums)

model.qoq.long1<-lm(qoq.GDPDEF~qoq.cpi+qoq.ppi-1)

# GDP QOQ and Others MOM
roc=ROC(Infdex[,3:8])
deflator=Infdex[,2]

# different frequency merge
diffreq=merge(qoqroc,momroc)

# change different frequency data in monthly data
qrs=floor((length(diffreq$GDPDEF)-1)/3)+1
for (i in 1:qrs)
{ diffreq$GDPDEF[3*i-2]=diffreq$GDPDEF[3*i]
diffreq$GDPDEF[3*i-1]=diffreq$GDPDEF[3*i]
}
diffreq$GDPDEF=(1+diffreq$GDPDEF)^(1/3)-1
momroc=diffreq

