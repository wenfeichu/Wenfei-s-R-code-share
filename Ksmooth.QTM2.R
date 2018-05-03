Ksmooth.QTM2<-
function (num, y, A, mu0, Sigma0, Phi, Ups, Gam, cQ, cR, input) 
{
  kf = Kfilter.QTM2(num, y, A, mu0, Sigma0, Phi, Ups, Gam, cQ, cR, input)
  pdim = nrow(as.matrix(Phi))
  xs = array(NA, dim = c(pdim, 1, num))
  Ps = array(NA, dim = c(pdim, pdim, num))
  J = array(NA, dim = c(pdim, pdim, num))
  xs[, , num] = kf$xf[, , num]
  Ps[, , num] = kf$Pf[, , num]
  for (k in num:2) {
    J[, , k - 1] = (kf$Pf[, , k - 1] %*% t(Phi)) %*% solve(kf$Pp[, , k])
    xs[, , k - 1] = kf$xf[, , k - 1] + J[, , k - 1] %*% (xs[,  , k] - kf$xp[, , k])
    Ps[, , k - 1] = kf$Pf[, , k - 1] + J[, , k - 1] %*% (Ps[, , k] - kf$Pp[, , k]) %*% t(J[, , k - 1])
  }
  x00 = mu0
  P00 = Sigma0
  J0 = as.matrix((P00 %*% t(Phi)) %*% solve(kf$Pp[, , 1]), 
                 nrow = pdim, ncol = pdim)
  x0n = as.matrix(x00 + J0 %*% (xs[, , 1] - kf$xp[, , 1]), 
                  nrow = pdim, ncol = 1)
  P0n = P00 + J0 %*% (Ps[, , 1] - kf$Pp[, , 1]) %*% t(J0)
  list(xs = xs, Ps = Ps, x0n = x0n, P0n = P0n, J0 = J0, J = J, 
       xp = kf$xp, Pp = kf$Pp, xf = kf$xf, Pf = kf$Pf, like = kf$like, 
       Kn = kf$K)
} 
#<environment: namespace:astsa>