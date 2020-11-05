# simulate data
lambda <- 5
num <- 15 # sample size
set.seed(123)
data <- rpois(num,lambda) # data

# likelihood
# write the likelihood function via the gamma distribution
likelihood_pois<- function(data, l){
  n <- length(data)
  dgamma(l, shape =sum(data)+1, scale=1/n)
}

likelihood_pois_v <- Vectorize(likelihood_pois, "l")

#prior
alpha <- 4
beta <- 2

#posterior parameters
alphastar <- alpha + sum(data)
betastar <- beta + length(data)

# plots
#likelihood
curve(likelihood_pois_v(l=x, data=data), xlim=c(-1,15), xlab=expression(theta), ylim =c(0,1), ylab = "density", lwd =2 )
curve(dgamma(x, alpha, beta), col="red", lty=1,lwd=2,  add =T)
curve(dgamma(x, alphastar, betastar), col="blue", lwd=2, add= T)  
legend(8, 0.9, c("Prior", "Likelihood", "Posterior"), c("red", "black", "blue"), lty=c(1,1,1),lwd=c(2,2,2))
