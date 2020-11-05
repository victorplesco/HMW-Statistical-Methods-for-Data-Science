roller.lm <- lm(depression~weight, data=roller)
roller.lm2 <- lm(depression~weight+I(weight^2), data=roller)
anova(roller.lm, roller.lm2) #P-f test- very high
plot(roller.lm)
plot(roller.lm2)
