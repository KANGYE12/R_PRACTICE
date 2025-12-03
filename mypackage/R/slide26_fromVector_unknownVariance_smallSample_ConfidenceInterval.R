x <- c (2.2 ,2.66 ,2.74 ,3.41 ,2.46 ,2.96 ,3.34 ,
        2.16 ,2.46 ,2.71 ,2.04 ,3.74 ,3.24 ,3.92 ,2.38 ,
        2.82 ,2.2 , 2.42 ,2.82 ,2.84 ,4.22 ,3.64 ,1.77 ,
        3.44 ,1.53)
n <- length(x)
a <- 0.05
#normal distribution, unknown variance (we only know sample variance), small sample
tStudent <- qt (.975 , n - 1)
sigma <- sqrt ( var ( x ))
mean <- mean(x)
lowerBound <- mean - tStudent * sigma / sqrt ( n )
upperBound <- mean + tStudent * sigma / sqrt ( n )