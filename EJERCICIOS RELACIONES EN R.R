#### EJERCICIOS TEMA 1 ####

# EJERCICIO 2
# a)
datos <- c(6.7,6.3,6.5,6.5,6.4,6.6) 
table(datos) #absolute frequency
sum(datos)

sort(datos)

media <- mean(datos)

quantile(datos)
quantile(datos, c(.01,.99))

sd(datos) ##√(Σ(xi – μ)^2/ (n-1))

varianza <- sd(datos)^2*(N-1)/N
varianza


#### EJERCICIOS TEMA 2 ####

## EJ 1

# a)
# Para representarla: 
y <- c(2,5,7,10)
n <- c(4,6,6,4)

N <- sum(c(4,6,6,4))
N

mean(y)
sd(y)
var <- sd(y)^2
var
var(y)
desvti3 <- sd(y)^3
desvti3

desvti4 <- sd(y)^4
desvti4

install.packages(moments)

library(moments)
# Calculamos el sesgo
skewness(y) # como el sesgo es 0, es una sim?trica

# Y luego la curtosis
desvti4 <- sd(y)^4
desvti4

mu4 <- sum((y-mean(y))^4*n / N)
mu4

curtosis <- (mu4 / desvti4) - 3
curtosis


kurtosis(y) - 3

# b)  Calcular las rectas de regresi?n de X sobre Y y de Y sobre X.

# Y/X:

x <- c(0,0,0,1,1,1,1,2,2,2,2,3,3,4,5)
y <- c(2,5,7,2,5,7,10,2,5,7,10,7,10,10,10)
n <- c(3,3,1,3,4,3,1,1,2,2,1,1,2,3,2)

sx <- sum(x*n)
sx2 <- sum(x^2*n)
sy <- sum(y*n)
sy2 <- sum(y^2*n)
sxy <- sum(x*y*n)
N <- sum(n)
mean(x)
mean(y)

var(x)
desvtiDeX <- sqrt(var(x))
desvtiDeX

var(y)
desvtiDeY <- sqrt(var(y))
desvtiDeY


M <- matrix(c(N, sx, sx, sx2), ncol=2, nrow=2)
S <- c(sy, sxy)
solve(M,S)
# y=a+bx, entonces y = 3.3884 + 1.392x

# X/Y:
M <- matrix(c(N, sy, sy, sy2), ncol=2, nrow=2)
S <- c(sx, sxy)
solve(M2,S2)
# x= a'+b'y entonces x= 0.3539y - 0.5025




# c) Calcular el coeficiente de correlacion lineal y la raiz del error cuadratico medio del modelo lineal Y sobre X.

# Coeficiente de correlacion lineal:

r <- cor(x,y) / desvtiDeX*desvtiDeY
r

r2 <- ((sxy / N) - mean(x)*mean(y)) / desvtiDeX*desvtiDeY
r2

cor(x,y)

coefCorrel <- cor.test(x, y) 
shapiro.test(x)
shapiro.test(y)



# MSE:

raizMSE <- sqrt(var(y)-solve(M,S)^2*var(x))
raizMSE





## EJ 3

# X/Y1:
x <- c(1,2,3,4,5)
y1 <- c(4,2,3,2,4) #Y1
n <- c(1,1,1,1,1)

N <- sum(n)
N

sxi <- sum(x)
syi <- sum(y1)
sxi2 <- sum(x^2*n)
syi2 <- sum(y1^2*n)
sxiyi <- sum(x*y1*n)

M <- matrix(c(N, sxi, sxi, sxi2), ncol=2, nrow=2)
S <- c(syi, sxiyi)
solve(M,S) 
# y= a'+b'x: a'=3 y b'= 0, entonces y=3

# X/Y2:
x <- c(1,2,3,4,5)
y1 <- c(1,3,5,7,9) #Y2
n <- c(1,1,1,1,1)

N <- sum(n)
N

sxi <- sum(x)
syi <- sum(y1)
sxi2 <- sum(x^2*n)
syi2 <- sum(y1^2*n)
sxiyi <- sum(x*y1*n)

M <- matrix(c(N, syi, syi, syi2), ncol=2, nrow=2)
S <- c(sxi, sxiyi)
solve(M,S) 
# y=a+bx: a=0.5 y b=0.5, entonces y=0.5+0.5x




## EJ 4

densidad <- c(43,55,40,52,39,33,50,33,44,21)
velocidad <- c(27.0,23.8,30.7,24.0,34.8,41.4,27.0,40.4,31.7,51.2)

# (a) Representar el diagrama de dispersi?n.
plot(densidad,velocidad)

# (b) A la vista del diagrama, elegir el valor correcto de r entre estos tres valores: 0'968, -0'968, -0'198

r <- cor(densidad,velocidad) 
r #Como vemos ser?a el de -0.968

# (c) Verificar la respuesta calculando r.
r <- cor(densidad,velocidad)
r

# (d)  ?Hay alguna evidencia real de que exista asociaci?on entre la velocidad de los veh?iculos y la densidad?
#Lo que podr?amos decir es que cu?nto menos denso es el vehiculo, m?s velocidad alcanza


## EJ 5

x <- c(0,0,0,0,0)
y <- c(0,0,0,0,0)
n <- c(1,1,1,1,1)
N <- sum(n)

sd(x)
sd(y)
medX <- mean(x)
medY <- mean(y)

cov <- (sum(x*y) -medX*medY) / N
cov

r <- cor(x,y)
r


#### EJERCICIOS TEMA 3 ####
library(zoo)
library(tidyverse)

## EJ 1


a?o <- c(1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983)
pob <- c(9.47,9.26,8.86,8.25,7.81,8.01,7.55,7.24,7.01,6.88,7.03)

# (a) Obtener la media movil de orden 4 y de orden 5 y representar en una grafica los promedios conjuntamente con los datos originales.

tmp <- rollmean(pob,4)
tmp
p4 <- c(NA,NA,rollmean(tmp,2),NA,NA)
p4


p5 <- c(NA,NA,rollmean(pob,5),NA,NA)
p5

# Para representar ahora esto:
plot(pob)
plot(p4)
plot(p5)

# (b) Calcular la tendencia por el metodo de los minimos cuadrados, ajustando una recta y representar graficamente el resultado junto a los valores originales.

#regresi?n por el m?todo de los m?nimos cuadrados:

regresion <- lm(pob~a?os)
regresion
predict(regresion)

plot(predict(regresion))

# (c) Presentar en una tabla los valores de la tendencia obtenidos en los metodos anteriores y comparar los distintos resultados.

matrizdatos <- matrix(c(a?os,pob,p4,p5,predict(regresion)), byrow = TRUE, nrow=5)
matrizdatos





## EJ 3


#Calcular los indices de variacion estacional para la produccion cuatrimestral y desestacionalizar la serie original.
#Interpreta los resultados e indica el a~no y cuatrimestre con produccion anormalmente grande si lo hubiese.

y <- c(3.9, 4, 4.8, 5.1, 5, 5.5, 6.1, 6.3, 6.9)
y

# Calculamos la tendencia y los ciclos largos por mm de orden k=3 pq son 3 subperiodos:
mm3 <- rollmean(as.numeric(y),3)
mm3

tc <- matrix(c(NA,mm3,NA),nrow=3)
tc 

#Dividimos y/tc= ea
ea <- y/tc
ea

#Ahora tenemos que calcular las medias (e):
e <- rowMeans(ea,na.rm = TRUE)
e

#Las sumamos(las e) para ver si alcanza el n?mero de estaciones, que son 3, si son menores o superiores, normalizamos:

sum(e) #supera al 3

#Normalizamos:
e <- e*3/sum(e)
e
#Ahora al sumarlos nos da 3 
sum(e)

#Calculamos la aletoriedad:
a <- ea/e
a

#Y finalmente, desestacionalizamos:
des <- y/e
des

#Introducimos los datos por ajuste de los m?nimos cuadrados
trim <- c(1:9)
datos <- as.vector(y)
datos

#Hacemos nuestro ajuste lineal
aLineal <- lm(datos~trim)
aLineal

#Y hacemos la predicci?n
predict(aLineal)




## EJ 4
#PARA HACER ESTE EJERCICIO USAMOS LA HIP?TESIS MULTIPLICATIVA: Y=TECA

Y <- matrix(c(178.2, 156.7, 164.2, 153.2, 157.5, 172.6, 185.9, 185.8, 165.0, 163.6, 169.0, 183.1,
              196.3, 162.8, 168.6, 156.9, 168.2, 180.2, 197.9, 195.9, 176.0, 166.4, 166.3, 183.9,
              197.3, 173.7, 173.2, 159.7, 175.2, 187.4, 202.6, 205.6, 185.6, 175.6, 176.3, 191.7,
              209.5, 186.3, 183.0, 169.5, 178.2, 186.7, 202.4, 204.9, 180.6, 179.8, 177.4, 188.9,
              200.0, 188.7, 187.5, 168.6, 175.7, 189.4, 216.1, 215.4, 191.5, 178.5, 178.6, 195.6,
              205.2, 179.6, 185.4, 172.4, 177.7, 202.7, 220.2, 210.2, 186.9, 181.4, 175.6, 195.6), byrow=TRUE, nrow=6)
Y


#Hacemos la mm de orden 12 con 6 datos del principio y otros 6 del final y luego la haremos de orden 2 para ajustar los valores, luego tenemos el tc:
p12 <- rollmean(as.numeric(t(Y)), 12)
p12real <- rollmean(p12, 2)
p12real


#Definimos la matriz TC con los datos calculados de las medias
TC <- matrix(c(NA, NA, NA, NA, NA, NA, p12real, NA, NA, NA, NA, NA, NA), byrow=TRUE, nrow=6)
TC


#Dividimos y/tc para calcular ea

EA <- matrix(as.vector(t(Y)) / as.vector(t(TC)), byrow=TRUE, nrow=6)
EA


# (a) Hacemos las medias de cada columna para calcular e, que son los ?ndices de variaci?n estacional:
E <- rowMeans(t(EA), na.rm = TRUE)
E


#Hacemos el sumatorio de de las e para ver si nos da exacto 12 meses
sum(E)

#Como no da exacto, tendremos que normalizar:
E <- E*12/sum(E)
E

sum(E) #Ahora al sumarlos s? que nos da 12


# (d) Una vez hemos corregido los ?ndices, podemos calcular la desestacionalizaci?n, por lo que nos quedar?a esta tabla:
des <- Y/E
des


# (e) Calculamos el gr?fico de dispersi?n
plot(as.vector(Y), col="red")
points(as.vector(Ydes), col="green")


# (f) Introducimos los datos para el ajuste por m?nimos cuadrados, esto es para calcular la tendencia por el m?todo de los m?nimos cuadrados:
meses <- c(1:72)
datos <- as.vector(t(Y))


# Ajustamos a una recta (regresi?n lineal)
reg <- lm(datos ~ meses)
reg
#y'=a+bt -> y'= 169.9070 + 0.3617t -> le multiplicamos la tendencia


#Calculamos r (coeficiente de correlaci?n de pearson)
cor.test(datos,meses, method = "pearson")

#realizamos la predicci?n
predict(reg)

# (g) Calculamos la variaci?n c?clica y la accidental

#Primero la accidental
A <- EA / E
A

#Luego la c?clica
C <- Y/ T*EA
C




## EJ 5

# OPCI?N 1

y <- matrix(c(66, 77, 100, 62, 77, 97, 63, 78, 96, 61, 78, 97), nrow=1)
y

# (a) Calcular las componentes de la serie temporal considerando la Hip?tesis multiplicativa

#Hacemos las medias m?viles de k=3 porque hay 3 cuatrimestres:
mm3 <- rollmean(as.numeric(y), 3)
mm3

tc <- matrix(c( NA, mm3, NA), nrow= 1)
tc

#Ahora calculamos el ea = y/tc
ea <- matrix(as.vector(y) / as.vector(tc), nrow= 1)
ea

#Calculamos los ?ndices de estacionalizaci?n, haciendo las medias de los cuatrimestres
e1 <- mean(ea[c(4,7,10)]) 
e1
e2 <- mean(ea[c(2,5,8,11)]) 
e2
e3 <- mean(ea[c(3,6,9)]) 

e <- c(e1,e2,e3)
e

#Los sumamos para ver si nos da un n?mero exacto
sum(e)
#Como la suma no llega a los 3 cuatrimestres, tendremos que normalizar:

e <- e*3/sum(e)
e

sum(e) #Ahora si los sumamos, nos da exacto

#Calculamos la variaci?n de accidente:

a <- matrix(as.vector(ea) / as.vector(e), nrow= 1)
a




# (b) Calcular las componentes de la serie temporal considerando la hipotesis aditiva: Y = T+C+E+A

# DE ESTA MANERA ES M?S F?CIL HACERLO
EA <- y-tc
EA

m1<-mean(EA[c(4,7,10)])
m1
m2<-mean(EA[c(2,5,8,11)])
m2
m3<-mean(EA[c(3,6,9)])
m3
media <- c(m1, m2, m3)
media
sum(m1,m2,m3)

EA[c(4,7,10)]-(-17.02)
EA[c(2,5,8,11)]-(-1.74)
EA[c(3,6,9)]-18.76





# OPCI?N 2 

#(a) hip?tesis multiplicativa
y <- c(66,77,100,62,77,97,63,78,96,61,78,97)

mm <- rollmean(y,3)
mm

tc <- c(NA,mm,NA)
tc

ea <- y/tc
ea

m1<-mean(ea[c(4,7,10)])
m1
m2<-mean(ea[c(2,5,8,11)])
m2
m3<-mean(ea[c(3,6,9)])
m3
sum(m1,m2,m3)

ea[c(4,7,10)]/m1
ea[c(2,5,8,11)]/m2
ea[c(3,6,9)]/m3



#(b) hip?tesis aditiva
EA<-y-tc
EA

m1<-mean(EA[c(4,7,10)])
m1
m2<-mean(EA[c(2,5,8,11)])
m2
m3<-mean(EA[c(3,6,9)])
m3
media<-c(m1, m2, m3)
media
sum(m1,m2,m3)

EA[c(4,7,10)]-(-17.02)
EA[c(2,5,8,11)]-(-1.74)
EA[c(3,6,9)]-18.76





## EJ 6

y <- matrix(c(2,0.8,2.5,3.4,1.8,1,2.3,3.7,2,0.7,2.2,3.5), nrow = 1)
y

# (a) Estimar la tendencia mediante medias moviles.

mm4 <- rollmean(as.numeric(y), 4)
mm4final <- rollmean(mm4, 2)
mm4final

tc <- matrix(c(NA, NA, mm4final, NA, NA), nrow= 1)
tc


#### EJERCICIOS TEMA 5 ####

## EJ 2

# (a) Encuentre la probabilidad de que en una caja que contenga frutos tratados, se encuentren mas de 5 atacados.

1 - pbinom(5, 200, 0.01)

# (b) Halle la probabilidad de que en una caja cuyos frutos no fueron tratados, se encuentren m?as de 5 atacados.

1 - pbinom(5, 200, 0.1)

# (c) A un almacen llega un 30% de cajas con frutos tratados. ?Cual es la probabilidad de que una caja con 4 frutos atacados, no haya sido tratada?

dbinom(4, 200, 0.01) #y
dbinom(4, 200, 0.1) #x

# (d) ?Cual es la probabilidad de que una caja con m?as de 5 frutos atacados, haya recibido el tratamiento?

1 - pbinom(5, 200, 0.01) #y
1 - pbinom(5, 200, 0.1) #x


# (e) Halle la probabilidad de que de 5 frutos extra?idos al azar de una caja, encontremos exactamente 2 atacados
dbinom(2, 5, 0.01) #y
dbinom(2, 5, 0.1) #x

# (f) Encuentre la probabilidad de obtener 2 frutos atacados al extraer 5 frutos de una caja con exactamente22 atacados.

dhyper(2, 22, 178, 5)



## EJ 3

# (a) Probabilidad de que haya tenido alguna aver?ia el a~no anterior (365 dias).
1 - ppois(0, 365*0.00028)

# (b) Probabilidad de que est?e situado en un barco sabiendo que tuvo alguna aver?ia en el a~no pasado
1 - dpois(0,0.365)



## EJ 4

# (a) Estimar la distribucion que sigue W

# (b) Hallar P(W > 1.8).

1 - pnorm(1.8, 1.42, 0.1276)

# (c) Hallar el cuartil 3.
qnorm(0.75) * 0.1276 + 1.42 # para destipificarlo




## EJ 5 

# (a) Hallar la media y el parametro ?? de la exponencial
1 - pexp(18, 0.0040317) #?? lo sacamos con P(X>= 18) = 0.07 y la media se saca de E[X] = 1/?? 

# (b) Si se piensa en ampliar la garant?ia a 2 a~nos. ?Qu?e porcentaje se espera que hagan uso de ella en esos 2a~nos?

pexp(24, 0.0040317) # La que x ahora es 24 meses y la ?? la que hemos calculado antes en el (a)




## EJ 7

1 - pchisq(20, 10)




## EJ 8

# (a) Probabilidad de que resulte peligroso el ajuste de una rueda.
1 - pbinom(1, 5, 0.01)

# (b) Si un coche lleva 5 ruedas, probabilidad de que alguna resulte peligrosa
1 - pbinom(1, 5, 0.01)^5

# (c) Si de la factor?ia salen 10000 veh?iculos al a~no, probabilidad de que resulte alguno con alguna rueda peligrosa.
1 - pbinom(0, 1000, 0.00489115)

# (d) Supongamos que un coche tiene alguna rueda defectuosa. Calcula la probabilidad de que la rueda defectuosa sea ?unicamente la de repuesto.

alguna <- 1 - dbinom(0,5,1- pbinom(1,5, 0.01))
alguna


1 - dbinom(0,1000 ,1 - dbinom(1,5,1- pbinom(1,5, 0.01)))


una <- dbinom(1,5,0.0009801496)
una
(1/5)*una / alguna




## EJ 9

# (a) Probabilidad de que un componente del tipo A dure m?as de 15 a~nos.

1 - pexp(15, 1/8)


# (b)  Sabiendo que un componente ha durado m?as de 15 a~nos, calcula la probabilidad de que el componente sea de tipo A.

1 - pexp(15, 1/9)



#### EJERCICIOS TEMA 6 ####

## EJ 2

#a)
1 - pnorm(14,12,4)

#b)
1 - pnorm(14,12,4/3)


## EJ 5 

#a)
qt(0.975, 27)

#b)
qt(1-0.025, 13.42521931)

#c)
qt(1-0.025, 12,15)
qt(1-0.975, 12, 15)

## EJ 6 

qt(1-0.05, 17)

## EJ 7 
qf(1-0.95, 9, 8)
qf(1-0.05, 9, 8)  

## EJ 8 

qf(1-0.975, 11, 7)
qf(1-0.025, 11, 7)  
