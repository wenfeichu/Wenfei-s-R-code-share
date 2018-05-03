## Using Kalman filter to interpolate GDP deflator qoq to mom 

## Dataset

qoq.CPI.60to16=inflation.3d.qoq$CPI
qoq.CCPI.60to16=inflation.3d.qoq$CCPI
qoq.PPI.60to16=inflation.3d.qoq$PPI

# mom.merge=merge(mom,Yt)
GDPlagmat=removeNA(merge(GDPDEF_ROC,lag(GDPDEF_ROC),lag(GDPDEF_ROC,2)))

## Regression for initial value
model.gdp<-lm(GDPDEF_ROC~lag(GDPDEF_ROC)+lag(GDPDEF_ROC,2)-1)
AR.GDPDEF<-ar(GDPDEF_ROC)
model.resi.full<-lm(AR.GDPDEF$resid~qoq.CPI.60to16+qoq.PPI.60to16+qoq.CCPI.60to16-1)

# modelmom1<-lm(GDPDEF~CPI+CCPI+PPI+CPPI+Import+Export-1, data = mom.merge)
mom.model2<-lm(GDPDEF_ROC~lag(GDPDEF_ROC)+lag(GDPDEF_ROC,2)+qoq.CPI.60to16+qoq.PPI.60to16+qoq.CCPI.60to16-1)
mom.model2.res=mom.model2$residuals
qoq.sigma0=var(mom.model2.res)

## Data input and parameters define

# Data input
y =GDPDEF.60to16.qtom; x=inflation.3d

num=length(y)

# Replace NA with 0
for (i in 1:num){
  if (is.na(y[i])){
    y[i]=0
  }
  
}

# Period change ratio
ratio=1/3; 

# Parameter generate
num = nrow(y); pdim=ncol(y); qdim=3  # dimension
period=3

#input = x; 
input=0

# Observation input matrix 
input.obs.mat=0;

idim = ncol(x);

## Generate time varying A
A0=array(0,dim=c(qdim,pdim,num))

# Loop to replace
A=A0
for (i in 1:num){
  if (i %% 3==0){
    A[,,i]=c(1,1,1)
  }
}


## Initial  Value

# Initial State Mean
mu0 = ratio*rep(y[3,],3)

# Initial State covariance matrix
Sigma0 = ratio^2*cov(GDPlagmat) #ratio^2*qoq.sigma0

## Observation error covariance matrix

R=0
cR=0

## Initial Value for Parameters need to be optimized
# cQ initial
cQ0=matrix(0,qdim,qdim)

# Phi matrix define
Phi0=rbind(c(0,0,0),c(1,0,0),c(0,1,0))


# State Input matrix
drift=0

## Function to Calculate Likelihood
Linn = function(para){
  ut.sd = para[1] 
  
  cQ.row1=c(ut.sd,0,0)
  phi=para[2]
  cQ=rbind(cQ.row1,cQ0[-1,])
  Phi0=rbind(c(0,0,0),c(1,0,0),c(0,1,0))
  Phi=rbind(c(1-phi,phi,0), Phi0[-1,])
  
  kf = Kfilter.QTM2(num,y,A,mu0,Sigma0,Phi,0,0,cQ,cR,0)
  return(kf$like) }

# Estimation
R0=sd(modelfull$residuals)
initial.par.1 = ratio^2*qoq.sigma0
initial.par.2 = 0.5

initial.par = c(initial.par.1,initial.par.2)  #,initial.par.3)

# output not shown
(est.data.momlong = optim(initial.par, Linn, hessian = TRUE)) #,NULL,method='BFGS', hessian=TRUE, control=list(trace=1,REPORT=1)))

SE = sqrt(diag(solve(est.data.momlong$hessian)))

# Display estimates
u = cbind(estimate=est.data.momlong$par, SE)
rownames(u)<- c('cR',"cQ")



## Smooth (first set parameters to their final estimates)
# Matrix cQ
ut.sd = est.data.mom$par[1]
cQ0=matrix(0,qdim,qdim)
cQ=cQ0
cQ[1,1]=ut.sd

# Matrix Phi
Phi=rbind(c(1-est.data.mom$par[1],est.data.mom$par[1],0),Phi0[-1,])

(Q = t(cQ)%*%cQ) # to view the estimated Q matrix

# drift = est$par[(qdim^2+qdim)/2+2]
ks.3vars = Ksmooth.QTM2(num,y,A,mu0,Sigma0,Phi,drift,0,cQ,cR,input)
gdpdef.mom.3vars=ks.3vars$xs
dim(gdpdef.mom.3vars)


## Fit and forecast
a=as.vector(ks.3vars$xs)
xs.3vars=matrix(a,dim(ks.3vars$xs)[1],dim(ks.3vars$xs)[3])
fitted.matrix<- x*t(xs.3vars)
fitted.value <- rowSums(fitted.matrix)
fitted.xts<-xts(fitted.value,time(y))


## Kalman Filter fitted plot
legends=c("GDP Deflator","Kalman Filter fitted")
plot(y,type="l",col="red")
lines(fitted.xts,col="green")
title(main= "GDP Deflator versus Kalman Filter fitted values (Q, R optimized)")

