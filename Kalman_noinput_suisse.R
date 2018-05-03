## MLE Newton
# Setup

## Dataset

# mom.merge=merge(mom,Yt)
GDPlagmat.suisse=removeNA(merge(GDPDEF.qoq.suisse,lag(GDPDEF.qoq.suisse),lag(GDPDEF.qoq.suisse,2)))

## Regression for initial value
model.gdp.suisse<-lm(GDPDEF.qoq.suisse~lag(GDPDEF.qoq.suisse)+lag(GDPDEF.qoq.suisse,2)-1)
AR.GDPDEF.suisse<-ar(GDPDEF.qoq.suisse)
model.suisse.resi<-lm(AR.GDPDEF.suisse$resid~CPI.qoq.suisse+PPI.qoq.suisse-1)

# modelmom1<-lm(GDPDEF~CPI+CCPI+PPI+CPPI+Import+Export-1, data = mom.merge)
mom.model.suisse<-lm(GDPDEF.qoq.suisse~lag(GDPDEF.qoq.suisse)+lag(GDPDEF.qoq.suisse,2)+CPI.qoq.suisse+PPI.qoq.suisse-1)
mom.model.suisse.resi=mom.model.suisse$residuals
qoq.sigma0=var(mom.model.suisse.resi)

## Data input and parameters define

# Data input
### Generate monthly interval quarter GDP Deflator with missing data

y=GDPDEF.mom.suisse
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

# mu0 = (summary(modelfull))$coef[,1]; #repeat(1,qdim); 

# Initial State covariance matrix
Sigma0 = ratio^2*cov(GDPlagmat.suisse) #ratio^2*qoq.sigma0

## Observation error covariance matrix

R=0
cR=0

## Initial Value for Parameters need to be optimized
# cQ initial
cQ0=matrix(0,qdim,qdim)

# Phi matrix define
Phi0=rbind(c(0,0,0),c(1,0,0),c(0,1,0))


# State Input matrix
# drift0=matrix(0,qdim,idim)
drift=0

## Function to Calculate Likelihood
Linn = function(para){
  ut.sd = para[1] 
  #phi.para = para[2]
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
initial.par.2 = 0.35

initial.par = c(initial.par.1,initial.par.2)  #,initial.par.3)

#init.par=cbind(R0,rep(0,(qdim^2+qdim)/2+1))
#init.par = c(.1,.1,.1,0,.05) # initial values of parameters

# output not shown
(est.data.mom = optim(initial.par, Linn, hessian = TRUE)) #,NULL,method='BFGS', hessian=TRUE, control=list(trace=1,REPORT=1)))



SE = sqrt(diag(solve(est.data.momlong$hessian)))


# Display estimates
u = cbind(estimate=est.data.momlong$par, SE)

rownames(u)<- c('cR',"cQ")

# Matrix cQ

cQ0=matrix(0,qdim,qdim)
cQ=cQ0
cQ[1,1]=est.data.mom$par[1]

# Matrix Phi
Phi=rbind(c(1-est.data.mom$par[1],est.data.mom$par[1],0),Phi0[-1,])
# 
(Q = t(cQ)%*%cQ) # to view the estimated Q matrix

# drift = est$par[(qdim^2+qdim)/2+2]
ks.suisse = Ksmooth.QTM2(num,y,A,mu0,Sigma0,Phi,drift,0,cQ,cR,input)
GDPDEF.mom.kalman.suisse=ks.suisse$xs
dim(GDPDEF.mom.kalman.suisse)
View(GDPDEF.mom.kalman.suisse)
GDPDEF.mom.kalman.suisse=GDPDEF.mom.kalman.suisse[1,1,]

