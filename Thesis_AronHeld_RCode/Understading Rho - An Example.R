# This file should only be used for gaining a general understanding 
  # of rho (ρ) in an AR(1) process (plots AR process plus ACF function):

set.seed(123)

plot_sample_rho <- function(numberobs, phi1, phi2){
  require(latex2exp)
  require(exuber)
  
  Z1 <- rep(0,numberobs)
  Z2 <- rep(0,numberobs)
  e <- rnorm(numberobs)
  ae <- sim_dgp1(numberobs)
  
  for(t in 3:numberobs) {
    Z1[t] <- phi1*Z1[t-1]+e[t]
    Z2[t] <- phi2*Z2[t-1]+e[t]
  }
  Z11 <- Z1[1:numberobs]
  Z22 <- Z2[1:numberobs]
  
  plot(Z11,type="l",ylab="Price",main=TeX("$\\rho\\, =\\, 1$
                                      (Non-Stationary)"))
  plot(Z22,type="l",ylab="Price",main=TeX("$\\rho\\, <\\, 1$
                                       (Stationary)"))
  plot(ae,type='l', ylab="Price", xlim=c(10,105), main=TeX("$\\rho\\, >\\, 1$  
                                                         (Non-Stationary)"))
  
  acf(Z11,main=TeX("ACF of AR with $\\rho\\, =\\, 1$(Non-Stationary)"))
  acf(Z22,main=TeX("ACF of AR with $\\rho\\, <\\, 1$(Stationary)"))
  acf(ae, main=TeX("ACF of AR with $\\rho\\, >\\, 1$(Non-Stationary)"))
  }

#layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
plot_sample_rho(200, 1.00, 0.40)

