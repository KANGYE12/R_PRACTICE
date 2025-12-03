n <- 150
infested <- 27
p <- infested/n

#95% confidence interval
a <- 0.05
e <- qnorm(1-(a/2), 0, 1)*sqrt(p*(1-p)/n)
e
lowerInterval <- p - e

upperInterval <- p + e

print(lowerInterval)
print(upperInterval)