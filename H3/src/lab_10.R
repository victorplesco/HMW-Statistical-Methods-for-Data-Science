library(rstan)
library(bayesplot)
y <- c(1,0,0,1,0,0,0,0,0,1,0,0,1,0)
k <- sum(y)
N <- 14
a <- 3
b <- 3
data <- list(N=N, n=k, a=a, b=b)
fit <- stan(file="./src/binomial_beta.stan", data=data, chains= 4, iter= 2000, refresh=-1)

# extract the posterior
posterior <- extract(fit, pars="p", permuted=F)

# plots
mcmc_intervals(posterior, pars = "p", prob= 0.5, prob_outer=0.9, point_est = "mean") +
  ggplot2::ggtitle("Posterior interval")+
  ggplot2::xlab("value")

mcmc_areas(posterior, pars = "p", prob=0.8, prob_outer = 1, point_est="mean") +
  ggplot2::labs(title = "Posterior distributions", subtitle = "with mean and 80% intervals")+
  ggplot2::ylab("density") +
  ggplot2::xlab("value")

mcmc_hist(posterior, pars= "p", binwidth = 0.005, freq=F)+
  yaxis_text(TRUE)+
  ggplot2::ggtitle("Marginal posterior distribution for p") +
  ggplot2::ylab("density")

mcmc_dens_overlay(posterior, pars="p") +
  ggplot2::labs(title = "Posterior distributions", subtitle = "overlaid densities of the Markov chains")+
  ggplot2::ylab("density") +
  ggplot2::xlab("value")
