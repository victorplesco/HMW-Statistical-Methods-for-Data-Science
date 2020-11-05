datapar <- list(data=y, par=c(2, 4))
fit1 <- laplace(logpoissongamma, .5, datapar)
datapar <- list(data=y, par=c(1, .5))
fit2 <- laplace(logpoissonnormal, .5, datapar)
datapar <- list(data=y, par=c(2, .5))
fit3 <- laplace(logpoissonnormal, .5, datapar)
datapar <- list(data=y, par=c(1, 2))
fit4 <- laplace(logpoissonnormal, .5, datapar)

logmarg <- c(fit1$int, fit2$int, fit3$int, fit4$int)
BF_matrix <- matrix(1, 4,4)
for (i in 1:3){
  for (j in 2:4){
    BF_matrix[i,j]<- exp(logmarg[i]-logmarg[j])
    BF_matrix[j,i]=(1/BF_matrix[i,j]) 
  }
}
round_bf <- round(BF_matrix,3)
round_bf
