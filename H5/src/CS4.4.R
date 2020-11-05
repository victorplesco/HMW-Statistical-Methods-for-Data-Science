# given values for T_i
T_i = c(243, 14, 121, 63, 45, 407, 34)

# compute loglikelihood
loglikelihood <- function(x, lambda){
  length(x) * log(sqrt(4 * lambda / pi)) - lambda * sum(x^2)
}

# compute estimation
lambda_hat <- length(T_i)/(2 * sum(T_i^2))

print(paste("MLE for lambda = ", lambda_hat))

l_0 <- loglikelihood(T_i, 10^(-4))
l_hat <- loglikelihood(T_i, lambda_hat)

V <- 2 * (l_hat - l_0)
print(paste("Likelihood ratio test statistic is = ", V))

p_lrt <- pchisq(V, df=1, lower.tail = FALSE)
print(paste("p-value of likelihood ratio test is =", p_lrt))