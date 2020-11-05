gamma <- seq(0.1, 15, length=100)
beta <- seq(100,200, length=100)
parvalues <- expand.grid(gamma, beta)
llikvalues <- apply(parvalues, 1, log_lik_weibull, data=y)
llikvalues <- t(matrix(-llikvalues, nrow=length(gamma), ncol=length(beta), byrow=F))
conf.levels <- c(0,0.5,0.75,0.9,0.95,0.99)

#contour plot
contour(beta, gamma, llikvalues-max(llikvalues),
        levels=-qchisq(conf.levels, 2)/2,
        xlab=expression(beta),
        labels=as.character(conf.levels),
        ylab=expression(gamma)) 
title('Weibull profile log-likelihood')

log_lok_dgamma <- function(x){
  uniroot(function(g) 
  n/g-n*log(x)+sum(log(y))-sum((y/x)^g*log(y/x)),
  c(1e-5,15))$root
}
gamma.beta <- sapply(beta, log_lok_dgamma)
lines(beta, gamma.beta, lty='dashed',col=2)
points(weib.y.mle$par[1],weib.y.mle$par[2])

# profile log likelihood
log_lik_weibull_profile  <- function(data, beta){
  gamma.beta <-log_lok_dgamma(beta)
  log_lik_weibull( data, c(gamma.beta, beta) )
}

log_lik_weibull_profile_v <-Vectorize(log_lik_weibull_profile, 'beta'  )

plot(function(x) -log_lik_weibull_profile_v(data=y, x)+weib.y.mle$value,
     from=100,to=200,xlab=expression(beta),
     ylab='profile relative log likelihood',ylim=c(-8,0))
conf.level<-0.95
abline(h=-qchisq(conf.level,1)/2,lty='dashed',col=2)

lrt.ci1<-uniroot(function(x) -log_lik_weibull_profile_v(y, x)+
                   weib.y.mle$value+
                   qchisq(conf.level,1)/2,
                 c(1e-7,weib.y.mle$par[2]))$root
lrt.ci1<-c(lrt.ci1,uniroot(function(x) -log_lik_weibull_profile_v(y,x)+
                             weib.y.mle$value+
                             qchisq(conf.level,1)/2,
                           c(weib.y.mle$par[2],200))$root)
segments( lrt.ci1[1],-qchisq(conf.level,1)/2, lrt.ci1[1],
          -log_lik_weibull_profile_v(y, lrt.ci1[1]), col="red", lty=2  )
segments( lrt.ci1[2],-qchisq(conf.level,1)/2, lrt.ci1[2],
          -log_lik_weibull_profile_v(y, lrt.ci1[2]), col="red", lty=2  )
points(lrt.ci1[1], -qchisq(conf.level,1)/2, pch=16, col=2, cex=1.5)
points(lrt.ci1[2], -qchisq(conf.level,1)/2, pch=16, col=2, cex=1.5)
segments( lrt.ci1[1],
          -8.1, lrt.ci1[2],
          -8.1, col="red", lty =1, lwd=2  )
text(157,-7.5,"95% Deviance CI",col=2)

