logpoissongamma <- function(theta, datapar){
  data <- datapar$data
  par <- datapar$par
  lambda <- exp(theta)
  log_lik <- log(lik_pois(data, theta))
  log_prior <- log(prior_gamma(par, theta))
  return(log_lik+log_prior)
}
logpoissongamma.v <- Vectorize( logpoissongamma, "theta")
logpoissonnormal <- function( theta, datapar){
  data <- datapar$data
  npar <- datapar$par
  lambda <- exp(theta)
  log_lik <- log(lik_pois(data, theta))
  log_prior <- log(prior_norm(npar, theta))
  return(log_lik+log_prior)
}  
logpoissonnormal.v <- Vectorize( logpoissonnormal, "theta")

#log-likelihood
curve(log(lik_pois(y, theta=x)), xlim=c(-1,4),ylim=c(-20,2), lty =1,
      ylab="log-posteriors", xlab=expression(theta))
#log posterior 1
curve(logpoissongamma.v(theta=x, list(data=y, par=c(2, 4))), col="red", xlim=c(-1,4),ylim=c(-20,2), lty =1, add =TRUE)
#log posterior 2
curve(logpoissonnormal.v( theta=x, datapar <- list(data=y, par=c(1, .5))), lty =1, col="blue",  add =TRUE)
#log posterior 3
curve(logpoissonnormal.v( theta=x, datapar <- list(data=y, par=c(2, .5))), lty =1, col="green", add =TRUE, lwd =2)
#log posterior 4
curve(logpoissonnormal.v( theta=x, list(data=y, par=c(1, 2))), lty =1, col="violet", add =TRUE, lwd =2)
legend(2.6, 1.3, c( "loglik", "lpost 1", "lpost 2", "lpost 3", "lpost 4" ),
       lty=1, col=c("black", "red", "blue", "green", "violet"),lwd=2, cex=0.9)
