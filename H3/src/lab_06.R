library(bayesplot)
posterior <- as.array(fit)
?bayesplot
# posterior intervals
mcmc_intervals(posterior, pars = "theta", prob= 0.5, prob_outer=0.9, point_est = "mean") +
  ggplot2::ggtitle("Posterior interval")+
  ggplot2::xlab("value")
# posterior areas
mcmc_areas(posterior, pars = "theta", prob=0.8, prob_outer = 1, point_est="mean") +
  ggplot2::labs(title = "Posterior distributions", subtitle = "with mean and 80% intervals")+
  ggplot2::ylab("density") +
  ggplot2::xlab("value")
# marginal posterior distribution for the parameters
mcmc_hist(posterior, pars= "theta", binwidth = 0.05, freq=F)+
  yaxis_text(TRUE)+
  ggplot2::ggtitle("Marginal posterior distribution for theta") +
  ggplot2::ylab("density")
