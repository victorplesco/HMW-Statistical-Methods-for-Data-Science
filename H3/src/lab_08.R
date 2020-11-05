# sigma ~ uniform(0.1,10);
library(bayesplot)
#launch biparametric Stan model
data2 <- list(N=n, y=y, a=-10, b=10)
fit2 <- stan(file="./src/biparametric.stan", data = data2, chains = 4, iter=2000, refresh=-1)
#extract stan output for biparametric model
sim2 <- extract(fit2)
posterior_biv <- as.matrix(fit2)
theta_est <- mean(sim2$theta)
sigma_est <- mean(sim2$sigma)
c(theta_est, sigma_est)
traceplot(fit2, pars=c("theta", "sigma"))
plot_title <- ggtitle("Posterior distributions", "with medians and 80% intervals")
mcmc_areas(posterior_biv, pars = c("theta","sigma"), prob = 0.8) + plot_title
