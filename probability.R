f<- function(n,k){
  return (factorial(n)/(factorial(k)*factorial(n-k)))
}

PD<-f(3,1)*f(17,3)/f(20,4)
#not getting a pair of 6 has to be less than 50%

PHT<-6/2**3
PFH<-4/2**3
n<-3/2**3
nn<-PHT*PFH
#INdependent

#sypmtom
B<-0.2*0.9 + 0.4*0.8
#disease
A<-0.2
AB<-0.2*0.9
Ac<-0.8
AcB<-0.8*0.4

#Without
PAB<-A*0.9/(0.2*0.9 + 0.4*0.8)
PAiB<-0.4*0.8/(0.2*0.9 + 0.4*0.8)

#Flu test
PFlu<-0.1
PPFlu<-0.95
PFluP<-0.1*0.95/(0.1*0.95 + 0.9*0.1)

#MASKS
MArg<- 1/6
MBel<- 0.5
MCh<- 1/3
MArgD<- 0.18
MBelD<- 2/25
D<- 0.1
#D<-MArg*MArgD +MBel*MBelD+MCh*0.01
MChD<-(D-(MArg*MArgD)-(MBel*MBelD))/MCh
PDC<-MCh*MChD/(MCh*MChD + (1-MCh)*(1-MChD))