log_lik_weibull <- function( data, param){
  -sum(dweibull(data, shape = param[1], scale = param[2], log = TRUE))
}
y <- c(155.9, 200.2, 143.8, 150.1,152.1, 142.2, 147, 146, 146,
       170.3, 148, 140, 118, 144, 97)
n <- length(y)
# log-likelihood function
weib.y.mle<-optim(c(1,1),fn=log_lik_weibull,hessian=T,
                  method='L-BFGS-B',lower=rep(1e-7,2),
                  upper=rep(Inf,2),data=y)

gammahat<-weib.y.mle$par[1]
betahat<-weib.y.mle$par[2]