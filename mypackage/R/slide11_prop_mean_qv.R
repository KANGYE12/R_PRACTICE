score <- c(0, 1, 2, 3, 4, 5)
ni <- c(10, 8, 12, 9, 7, 3)

x <- c(rep(score, ni))

N <- 1500
#proportion
p <- (12+9+7+3)/(length(x))
#mean
sampleMean <- mean(x)
mean <- sampleMean
#variance
Quasivariance <- var(x)*length(x)/(length(x)-1)