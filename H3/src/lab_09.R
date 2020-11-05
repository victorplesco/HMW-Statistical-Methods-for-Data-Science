library(LearnBayes)
data(soccergoals)

y <- soccergoals$goals

#write the likelihood function via the gamma distribution
lik_pois<- function(data, theta){
  n <- length(data)
  lambda <- exp(theta)
  dgamma(lambda, shape =sum(data)+1, scale=1/n)
}

prior_gamma <- function(par, theta){
  lambda <- exp(theta)
  dgamma(lambda, par[1], rate=par[2])*lambda  
}

prior_norm <- function(npar, theta){
  lambda=exp(theta)  
  (dnorm(theta, npar[1], npar[2]))
  
}

lik_pois_v <- Vectorize(lik_pois, "theta")
prior_gamma_v <- Vectorize(prior_gamma, "theta")
prior_norm_v <- Vectorize(prior_norm, "theta")

# plot
#likelihood
curve(lik_pois_v(theta=x, data=y), xlim=c(-3,5), xlab=expression(theta), ylab = "density", lwd =2 )
#prior 1 with parameters 2 and 4
curve(prior_gamma_v(theta=x, par=c(2, 4)), lty =2, col="red", add = TRUE, lwd =2)
#prior 2 
curve(prior_norm_v(theta=x, npar=c(1, .5)), lty =3, col="blue", add =TRUE, lwd=2)
#prior 3 
curve(prior_norm_v(theta=x, npar=c(2, .5)), lty =4, col="green", add =TRUE, lwd =2)
#prior 4 
curve(prior_norm_v(theta=x, npar=c(1, 2)), lty =5, col="violet", add =TRUE, lwd =2)
legend(2.6, 1.8, c("Lik.", "Ga(2,4)", "N(1, 0.25)", "N(2,0.25)","N(1, 4)" ),
       lty=c(1,2,3,4,5), col=c("black", "red", "blue", "green", "violet"),lwd=2, cex=0.9)
