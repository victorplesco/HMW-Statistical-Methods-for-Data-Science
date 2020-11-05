y <- c(155.9, 200.2, 143.8, 150.1,152.1, 142.2, 147, 146, 146,
       170.3, 148, 140, 118, 144, 97)

log_lik_weibull <- function(data, param){
  -sum(dweibull(data, shape = param[1], scale = param[2], log = TRUE))
}

omega <- function(theta) log(theta)
theta <- function(omega) exp(omega)
log_lik_weibull_rep <- function(data, param) log_lik_weibull(data, theta(param))

omega_param <- nlm(log_lik_weibull_rep, c(0,0), hessian=T, data=y)

# Variance of omega=log(param)) of the estimator using nlm
# compute the inverse of the info matrix resulting in the covariance matrix
omega_param$hessian %>% solve %>% diag 

# Variance of the param=theta(omega_param) using optimHess
optimHess(theta(omega_param$estimate), log_lik_weibull, data=y) %>% solve %>% diag
