# Setup

## Data input and parameters define
# Data input
library(astsa)
period=3
(num=dim(ks.suisse$xs)[3])

yt=GDPDEF.mom.kalman.suisse

reg.ts=xts(yt,time(GDPDEF.mom.suisse))

y =reg.ts; x=inflation.mom.suisse


model.mom.suisse<-lm(GDPDEF.qoq.suisse~CPI.qoq.suisse+PPI.qoq.suisse-1)




# model.mom1<-lm(GDPDEF~CPI+CCPI+PPI+CPPI+Import+Export-1,data = reg.data)

# mom.model2<-lm(qoq.GDPDEF~qoq.CPI+qoq.PPI-1)
model.mom.suisse.res=model.mom.suisse$residuals
qoq.sigma0=var(model.mom.suisse.res)

# View(gdpdef.mom)

# Parameter generate
num = nrow(y); pdim=ncol(y); qdim=ncol(x)  # dimension

input = 0; input.obs.mat=0;

drift=0 #rep(1,num) #y = cbind(globtemp, globtempl)
A = array(t(x), dim=c(qdim,pdim,num)) ###????

## Initial Value
mu0 = (summary(model.mom.suisse))$coef[,1]; #repeat(1,qdim); 

Sigma0 = diag((summary(model.mom.suisse))$coef[,2]);

Phi = diag(1,qdim)

# Function to Calculate Likelihood
Linn = function(para){
  cR = para[1] 
  # cQ1 = para[2]    # sigma_w
  # cQ2 = para[3]
  # cQ3 = para[4]
  # cQ4 = para[5]
  # cQ5 = para[6]
  # cQ6 = para[7]
  # cR1 = para[2] # 11 element of chol(R)
  # cR2 = para[3] # 22 element of chol(R)
  # cR12 = para[4] # 12 element of chol(R)
  # cQ define 
  s=0; cQ=matrix(0,qdim,qdim)
  
  for (i in 1:qdim)
  { for (j in i:qdim){
    s=s+1
    nam <- paste("cQ",i,j,sep="",collapse = NULL)
    assign(nam,para[s+1])
    cQ[i,j]=para[s+1]
  }
  }
  
  # cR = #matrix(c(cR1,0,cR12,cR2),2) # put the matrix together
  # drift = para[(qdim^2+qdim)/2+2]
  
  kf = Kfilter1(num,y,A,mu0,Sigma0,Phi,drift,0,cQ,cR,input)
  return(kf$like) }

# Estimation
R0=sd(model.mom.suisse$residuals)
init.par=c(R0,0.0001,0,0.0001)
#init.par=c(R0,0.001,0.0001,0.0001,0.0001,0.0001,0.0001,0.001,0.0001,0.0001,0.0001,0.0001,0.001,0.0001,0.0001,0.0001,0.001,0.0001,0.0001,0.001,0.0001,0.001)
#init.par=cbind(R0,rep(0,(qdim^2+qdim)/2+1))
#init.par = c(.1,.1,.1,0,.05) # initial values of parameters

(est = optim(init.par, Linn, NULL,method='BFGS', hessian=TRUE, 
             control=list(trace=1,REPORT=1))) # output not shown


SE = sqrt(diag(solve(est$hessian)))


# Display estimates
u = cbind(estimate=est$par, SE)

# rownames(u)[1] <- 'cR'
# rownames(u)[dim(u)[1]] <- 'drift'


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
## Plot
# xsm = ts(as.vector(ks$xs))#, start=1880)
# rmse = ts(sqrt(as.vector(ks$Ps)))#, start=1880)
# plot(xsm, ylim=c(-.6, 1), ylab='Temperature Deviations')
# xx = c(time(xsm), rev(time(xsm)))
# yy = c(xsm-2*rmse, rev(xsm+2*rmse))
# polygon(xx, yy, border=NA, col=gray(.6, alpha=.25))
# lines(qoqroc, type='o', pch=2, col=4, lty=6)
# lines(globtemp, type='o', pch=2, col=4, lty=6)
# lines(globtempl, type='o', pch=3, col=3, lty=6)

## Fit and forecast
a=as.vector(ks$xs)
xs=matrix(a,dim(ks$xs)[1],dim(ks$xs)[3])
fitted.matrix<- x*t(xs)
fitted.value <- rowSums(fitted.matrix)
fitted.xts<-xts(fitted.value,time(y))
# lines(qoqroc, type='o', pch=2, col=4, lty=6)
# lines(fitted.ts, type='o', pch=3, col=3, lty=6)

## one plot per page
# par(mfrow = c(1,1))
# dev.off()

## Kalman Filter fitted plot
legends=c("GDP Deflator","Kalman Filter fitted")
plot(y,type="l",col="red")
lines(fitted.xts,col="green")
title(main= "GDP Deflator versus Kalman Filter fitted values (Q, R optimized)")


## GDP Deflator versus
plot(time(y),y,type="b", pch=19, col="red", xlab="x", ylab="y")
# Add a line
lines(time(y),fitted.xts, pch=18, col="blue", type="b", lty=2)
# Add a legend
legend("1995-01-31", 0.06, legend=c("GDP Deflator","Kalman Filter fitted"),
       col=c("red", "blue"), lty=1:2, cex=0.8)
title(main= "Switzerland GDP Deflator versus Kalman Filter fitted  (Q, R optimized)")
# legend(legends)

plot(time(a),a,type="b",pch=19,col='green',xlab="time", ylab="Rate")
title(main = "Kalman Filter residuals")