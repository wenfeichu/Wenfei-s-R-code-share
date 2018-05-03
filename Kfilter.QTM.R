########## Quarterly to monthly kalman filter

### Compared with normal kalman filter function, allows missing values in the observation variable

Kfilter.QTM=function (num, y, A, mu0, Sigma0, Phi, Ups, Gam, cQ, cR, input,period) 
{
  Q = t(cQ) %*% cQ
  R = t(cR) %*% cR
  Phi = as.matrix(Phi)
  pdim = nrow(Phi)
  y = as.matrix(y)
  qdim = ncol(y)
  rdim = ncol(as.matrix(input))
  if (max(abs(Ups)) == 0) 
    Ups = matrix(0, pdim, rdim)
  if (max(abs(Gam)) == 0) 
    Gam = matrix(0, qdim, rdim)
  Ups = as.matrix(Ups)
  Gam = as.matrix(Gam)
  ut = matrix(input, num, rdim)
  xp = array(NA, dim = c(pdim, 1, num))
  Pp = array(NA, dim = c(pdim, pdim, num))
  xf = array(NA, dim = c(pdim, 1, num))
  Pf = array(NA, dim = c(pdim, pdim, num))
  innov = array(NA, dim = c(qdim, 1, num))
  sig = array(NA, dim = c(qdim, qdim, num))
  x00 = as.matrix(mu0, nrow = pdim, ncol = 1)
  P00 = as.matrix(Sigma0, nrow = pdim, ncol = pdim)
  xp[, , 1] = Phi %*% x00 + Ups %*% ut[1, ]
  Pp[, , 1] = Phi %*% P00 %*% t(Phi) + Q
  B = matrix(A[, , 1], nrow = qdim, ncol = pdim)
  sigtemp = B %*% Pp[, , 1] %*% t(B) + R
  if (1%%period==0){
  sig[, , 1] = (t(sigtemp) + sigtemp)/2
  siginv = solve(sig[, , 1])
  }
  else{
    sig[,,1]=0
    siginv=0
  }
  K = Pp[, , 1] %*% t(B) %*% siginv
  innov[, , 1] = y[1, ] - B %*% xp[, , 1] - Gam %*% ut[1, ]
  xf[, , 1] = xp[, , 1] + K %*% innov[, , 1]
  Pf[, , 1] = Pp[, , 1] - K %*% B %*% Pp[, , 1]
  sigmat = as.matrix(sig[, , 1], nrow = qdim, ncol = qdim)
  if (1%%period==0){
    like = log(sigmat) + t(innov[, , 1]) %*% siginv %*% innov[, , 1]
  }
  else{
    like=0
  }
  for (i in 2:num) {
    if (num < 2) 
      break
    xp[, , i] = Phi %*% xf[, , i - 1] + Ups %*% ut[i, ]
    Pp[, , i] = Phi %*% Pf[, , i - 1] %*% t(Phi) + Q
    B = matrix(A[, , i], nrow = qdim, ncol = pdim)
    siginv = B %*% Pp[, , i] %*% t(B) + R
    if (i%%period==0){
      sig[, , i] = (t(siginv) + siginv)/2
      siginv = solve(sig[, , i]) 
    } else{
      sig[, , i] = 0
      siginv = 0
      
    }
   
    
    K = Pp[, , i] %*% t(B) %*% siginv
    innov[, , i] = y[i, ] - B %*% xp[, , i] - Gam %*% ut[i, 
                                                         ]
    xf[, , i] = xp[, , i] + K %*% innov[, , i]
    Pf[, , i] = Pp[, , i] - K %*% B %*% Pp[, , i]
    sigmat = matrix(sig[, , i], nrow = qdim, ncol = qdim)
    
    if (i%%period==0){
      like = like + log(sigmat) + t(innov[, , i]) %*% siginv %*% innov[, , i] 
    }
    
  }
  like = 0.5 * like
  list(xp = xp, Pp = Pp, xf = xf, Pf = Pf, like = like, innov = innov, 
       sig = sig, Kn = K)
}