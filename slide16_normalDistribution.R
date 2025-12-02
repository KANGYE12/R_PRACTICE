populationMean <- 12
populationVariance <- 16
populationSigma <- sqrt(populationVariance)

#a
a <- pnorm(14, populationMean, populationSigma, lower.tail = FALSE)
print(a)

#b
n <- 9
sampleSigma <- populationSigma/sqrt(n)
b <- pnorm(14, populationMean, sampleSigma, lower.tail = FALSE)
print(b)

#c
c <- sampleSigma^2
print(c)

#d
#Yes because the variance is lower in the sample