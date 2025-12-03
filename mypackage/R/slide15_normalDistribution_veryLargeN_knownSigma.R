n <- 25
atMost <- 6.3
mean <- 6
sigma <- 2

#we use infinite population (very large N)
sampleSigma <- (sigma/sqrt(n))

prob <- pnorm(6.3, mean, sampleSigma, lower.tail=TRUE)
#we use lower.tail = TRUE because we want lower than 6.3
print(prob)