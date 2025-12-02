before <- c(39.3, 39.7, 39.9, 40, 38.9, 39.7, 39.3)
after <- c(38.1, 38, 38.3, 37.9, 38.7, 38.3, 37)

#unkown variance, small sample, check if equal sample variance
var1 <- var(before)
var2 <- var(after)

n1 <- length(before)
n2 <- length(after)
a <- 0.1
#since they are different 
f <- ((var1/n1 + var2/n2)^2/((var1/n1)^2/(n1+1) + (var2/n2)^2/(n2+1))) - 2
t <- qt(a/2, f)

difMeans <- mean(after)-mean(before)
e <- t*sqrt(var1/n1 + var2/n2)

lowerBound <- difMeans + e
upperBound <- difMeans - e
print(lowerBound)
print(upperBound)