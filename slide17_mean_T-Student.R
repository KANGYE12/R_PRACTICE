n <- 16
sampleSigma <- 10.85
difference <- 8

#populationVariance is unknown
#P[mean-8 <= X <= mean+8]

#t = (sampleMean - mean)/(sampleSigma/sqrt(n))

#degrees of freedom
df <- n-1
t1 <- difference/(sampleSigma/sqrt(n))
t2 <- -difference/(sampleSigma/sqrt(n))
#cumulative probability
probability <- pt(t1, df) - pt(t2, df)

print(probability)