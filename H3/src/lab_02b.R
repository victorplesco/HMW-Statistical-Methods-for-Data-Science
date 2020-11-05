#observed information matrix
jhat <- weib.y.mle$hessian
mle.se <-sqrt(diag(solve(jhat)))
# c.i.
alpha <- 1- 0.95
w.ci.gamma <- gammahat + qnorm(1- alpha/2) * mle.se[1] *c(-1,1)
w.ci.gamma

# profile log-likelihood function
log_lik_weibull_profile  <- function(data, gamma){
  beta.gamma <- mean(data^gamma)^(1/gamma)        # beta nuisance parameter
  log_lik_weibull( data, c(gamma, beta.gamma) )   # gamma parameter of interest
}

log_lik_weibull_profile_v <-Vectorize(log_lik_weibull_profile, 'gamma'  )

plot(function(x) -log_lik_weibull_profile_v(data=y, x)+weib.y.mle$value, 
     from=0.1,to=15,xlab=expression(gamma), 
     ylab='profile relative log likelihood',ylim=c(-8,0))

segments(w.ci.gamma[1],-log_lik_weibull_profile_v(y, w.ci.gamma[1])+weib.y.mle$value, 
         w.ci.gamma[1], -log_lik_weibull_profile_v(y, w.ci.gamma[1]), col="red", lty=2)

segments(w.ci.gamma[2],-log_lik_weibull_profile_v(y, w.ci.gamma[2])+weib.y.mle$value, 
         w.ci.gamma[2], -log_lik_weibull_profile_v(y, w.ci.gamma[2]), col="red", lty=2)

points(w.ci.gamma[1], -log_lik_weibull_profile_v(y, w.ci.gamma[1])+weib.y.mle$value,
       pch=16, col=2, cex=1.5)

points(w.ci.gamma[2], -log_lik_weibull_profile_v(y, w.ci.gamma[2])+weib.y.mle$value,
       pch=16, col=2, cex=1.5)

segments( w.ci.gamma[1], -8.1, w.ci.gamma[2], -8.1, col="red", lty =1, lwd=2)

text(7,-7.5,"95% Wald CI",col=2)