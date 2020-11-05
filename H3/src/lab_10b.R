hist(posterior, freq = FALSE, ylim=c(0,4), main="Analitical VS simulated posterior distribution", xlab = "p", breaks = 30)
curve(dbeta(x, a + sum(y), b + length(y)- sum(y)), add=T, col=2)
