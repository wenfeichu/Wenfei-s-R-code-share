############# MLE for Kalman filter parameters using Newton Iteration method
### 3 inflation variables input: CPI, CCPI, PPI
# Setup

## Data input and parameters define
# Data input
### 3 data CPI CCPI PPI merge
merge.3d<-merge(CPIAUCSL,CPILFESL,PPIACO)


cut_off_date="1959-12-01"
end_date="2016-12-01"
cut_off_date_GDPDEF="1959-10-01"
end_date_GDPDEF="2016-10-01"

#Cut off Position in the time series
cutoff=which(index(merge.3d) ==cut_off_date)
cutoff_GDPDEF=which(index(GDPDEF)==cut_off_date_GDPDEF)

# End time              
endpoint=which(index(merge.3d)==end_date)
endpoint_GDPDEF=which(index(GDPDEF)==end_date_GDPDEF)

## Change into rate and rename
inflation.3d=ROC(merge.3d[cutoff:endpoint,])[-1,]
colnames(inflation.3d)=c("CPI","CCPI","PPI")
GDPDEF_ROC=ROC(GDPDEF[cutoff_GDPDEF:endpoint_GDPDEF,])[-1]

## change date
inflation.3d.date<- seq(as.Date(index(inflation.3d[2])), length=length(inflation.3d$CPIAUCSL), by="months")-1
index(inflation.3d)=inflation.3d.date
index(GDPDEF_ROC)=timeLastDayInQuarter(as.Date(index(GDPDEF_ROC)), format = "%Y-%m-%d", zone = "", FinCenter = "")

mom.3.gd=merge(inflation.3d,GDPDEF_ROC)
GDPDEF.60to16.qtom=mom.3.gd$GDPDEF
inflation.3d.qoq=apply.quarterly(x=inflation.3d,FUN = colSums)

## Parameters
period=3
num=dim(gdpdef.mom.3vars)[3]

yt=gdpdef.mom.3vars[1,,]

reg.ts=xts(yt,time(inflation.3d))
reg.data=merge(reg.ts,x)
y =reg.ts; x=inflation.3d
colnames(reg.data)[1]="GDPDEF"

model.mom.60to16<-lm(GDPDEF~CPI+CCPI+PPI-1,data = mom.3.gd)

# View(gdpdef.mom)

# Parameter generate
num = nrow(y); pdim=ncol(y); qdim=ncol(x)  # dimension

input = 0; input.obs.mat=0;

drift=0 #rep(1,num) #y = cbind(globtemp, globtempl)
A = array(t(x), dim=c(qdim,pdim,num)) ###????

## Initial Value
mu0 = (summary(model.mom.60to16))$coef[,1]; #repeat(1,qdim); 

Sigma0 = diag((summary(model.mom.60to16))$coef[,2]);

Phi = diag(1,qdim)

# Function to Calculate Likelihood
Linn = function(para){
  cR = para[1] 
  
  s=0; cQ=matrix(0,qdim,qdim)
  
  for (i in 1:qdim)
  { for (j in i:qdim){
    s=s+1
    nam <- paste("cQ",i,j,sep="",collapse = NULL)
    assign(nam,para[s+1])
    cQ[i,j]=para[s+1]
  }
  }
  
  kf = Kfilter1(num,y,A,mu0,Sigma0,Phi,drift,0,cQ,cR,input)
  return(kf$like) }

# Estimation
R0=sd(model.mom.60to16$residuals)
init.par=c(R0,0.001,0,0,0.001,0,0.001)
#init.par=c(R0,0.001,0.0001,0.0001,0.0001,0.0001,0.0001,0.001,0.0001,0.0001,0.0001,0.0001,0.001,0.0001,0.0001,0.0001,0.001,0.0001,0.0001,0.001,0.0001,0.001)
#init.par=cbind(R0,rep(0,(qdim^2+qdim)/2+1))
#init.par = c(.1,.1,.1,0,.05) # initial values of parameters

(est = optim(init.par, Linn, NULL,method='BFGS', hessian=TRUE, 
             control=list(trace=1,REPORT=1))) # output not shown


SE = sqrt(diag(solve(est$hessian)))


# Display estimates
u = cbind(estimate=est$par, SE)


# Name estimated parameters
s=0;
cQ.names <- rep(NA,(qdim^2+qdim)/2)
for (i in 1:qdim){
  for(j in i:qdim){
    s=s+1
    cQ.names[s] <- paste("cQ",i, j, seq="", collapse = NULL)
  }
}
u.names<- c('cR',cQ.names)
rownames(u)<- u.names
# rownames(u)=c('sigw','cR11', 'cR22', 'cR12', 'drift'); u


## Smooth (first set parameters to their final estimates)
cR = est$par[1]

# cR1 = est$par[2]
# cR2 = est$par[3]
# cR12 = est$par[4]
# cR = matrix(c(cR1,0,cR12,cR2), 2)

s=0; cQ=matrix(0,qdim,qdim)

for (i in 1:qdim)
{ for (j in 1:i){
  s=s+1
  nam <- paste("cQ",i,j,sep="",collapse = NULL)
  assign(nam,est$par[s+1])
  cQ[i,j]=est$par[s+1]
}
}

(Q = t(cQ)%*%cQ) # to view the estimated Q matrix

# drift = est$par[(qdim^2+qdim)/2+2]
ks = Ksmooth1(num,y,A,mu0,Sigma0,Phi,drift,0,cQ,cR,input)
kf= Kfilter1(num,y,A,mu0,Sigma0,Phi,drift,0,cQ,cR,input)

## Fit and forecast
a=as.vector(ks$xs)
xs=matrix(a,dim(ks$xs)[1],dim(ks$xs)[3])
fitted.matrix<- x*t(xs)
fitted.value <- rowSums(fitted.matrix)
fitted.xts<-xts(fitted.value,time(y))


plot(y,type="l",col="red")
lines(fitted.xts,col="green")
title(main= "GDP Deflator versus Kalman Filter fitted values (Q, R optimized)")


## GDP Deflator versus
plot(time(y),y,type="b", pch=19, col="red", xlab="x", ylab="y")
# Add a line
lines(time(y),fitted.xts, pch=18, col="blue", type="b", lty=2)
# Add a title
title(main= "GDP Deflator versus Kalman Filter fitted values (Q, R optimized)")
# legend(legends)
legend(as.Date('1995-01-31'), 0.010, legend=c("GDP Deflator","Kalman Filter fitted"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
