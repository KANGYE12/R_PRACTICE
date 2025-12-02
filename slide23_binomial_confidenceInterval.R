sizea <- 500
sizeb <- 300
a <- 0.05
pA <- 0.43
pB <- 0.42
#binomial
z <- 1- (a/2)
lowerIntervalA <- pA - qnorm(z, 0, 1)*sqrt(pA*(1-pA)/sizea)

upperIntervalA <- pA + qnorm(z, 0, 1)*sqrt(pA*(1-pA)/sizea)

lowerIntervalB <- pB - qnorm(z, 0, 1)*sqrt(pB*(1-pB)/sizeb)

upperIntervalB <- pB + qnorm(z, 0, 1)*sqrt(pB*(1-pB)/sizeb)
