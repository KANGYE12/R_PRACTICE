std <- 1200
n <- 25
value <- 2500
#prob that quasivar is greater that 2500
#we know quasiVar(n-1)/variance = chisquare(n-1)
#probability that chisquare(n-1) > (n-1)*2500/variance

#degrees of freedom of chisquare 
df <- n-1
variance <- std^2
greaterThan <- (n-1)*value/variance


chisquare <- qchisq(greaterThan, df)
print(chisquare)

#test comment