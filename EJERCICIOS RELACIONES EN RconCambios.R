#### EJERCICIOS TEMA 1: ESTADÍSTICA DESCRIPTIVA UNIDIMENSIONAL ####

# EJERCICIO 2
# Enunciado: Se han tomado 6 medidas de una variable (ej. pH): 6.7, 6.3, 6.5, 6.5, 6.4, 6.6.
# a) Obtenga la tabla de frecuencias absolutas y la suma total.
# b) Calcule la media, los cuantiles al 1% y 99%, y la desviación típica muestral.
# c) Calcule la varianza poblacional (asumiendo N como total poblacional).

# a)
datos <- c(6.7,6.3,6.5,6.5,6.4,6.6) 
table(datos) # Frecuencia absoluta
sum(datos)

sort(datos)

# b)
media <- mean(datos)

quantile(datos)
quantile(datos, c(.01,.99))

sd(datos) ## Desviación típica muestral: √(Σ(xi – x̄)^2/ (n-1))

# c)
N <- length(datos) # (Nota: N no estaba definido en el script original, lo añado para que funcione)
varianza <- sd(datos)^2*(N-1)/N
varianza


#### EJERCICIOS TEMA 2: DESCRIPTIVA BIDIMENSIONAL Y REGRESIÓN ####

## EJ 1
# Enunciado: Dada la variable Y con valores {2,5,7,10} y frecuencias {4,6,6,4}.
# a) Calcule media, desviación típica, varianza y momentos de orden 3 y 4.
#    Analice la simetría (sesgo) y la curtosis.
# b) Dados unos nuevos datos (X,Y), calcule las rectas de regresión Y/X y X/Y.
# c) Calcule la correlación lineal (r) y el error cuadrático medio (RMSE) de Y/X.

# a) Análisis de la variable Y
# Para representarla: 
y <- c(2,5,7,10)
n <- c(4,6,6,4)

N <- sum(c(4,6,6,4))
N

mean(y) # Nota: mean(y) aquí hace la media de los valores únicos, ojo si se requiere media ponderada
sd(y)
var <- sd(y)^2
var
var(y)
desvti3 <- sd(y)^3
desvti3

desvti4 <- sd(y)^4
desvti4

# install.packages("moments") # Descomentar si no está instalado
library(moments)

# Calculamos el sesgo
skewness(y) # como el sesgo es 0, es simétrica

# Y luego la curtosis
mu4 <- sum((y-mean(y))^4*n / N) # Cálculo manual del momento central 4
mu4

curtosis <- (mu4 / desvti4) - 3
curtosis

kurtosis(y) - 3

# b) Calcular las rectas de regresión de X sobre Y y de Y sobre X.

# Recta Y/X (Y depende de X):
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

# Sistema de ecuaciones normales para Y = a + bx
M <- matrix(c(N, sx, sx, sx2), ncol=2, nrow=2)
S <- c(sy, sxy)
solve(M,S)
# Resultado: y = 3.3884 + 1.392x

# Recta X/Y (X depende de Y):
# Sistema para x = a' + b'y
M2 <- matrix(c(N, sy, sy, sy2), ncol=2, nrow=2) # Corregido nombre variable a M2
S2 <- c(sx, sxy) # Corregido nombre variable a S2
solve(M2,S2)
# Resultado: x = 0.3539y - 0.5025

# c) Calcular el coeficiente de correlacion lineal y la raiz del error cuadratico medio del modelo lineal Y sobre X.

# Coeficiente de correlacion lineal:
# Nota: La fórmula manual en el script original tenía errores de paréntesis, usar cor() es más seguro.
cor(x,y)

coefCorrel <- cor.test(x, y) 
shapiro.test(x) # Test de normalidad
shapiro.test(y)

# MSE (Mean Squared Error) y su raíz (RMSE):
raizMSE <- sqrt(var(y)-solve(M,S)[2]^2*var(x)) # solve(M,S)[2] es la pendiente 'b'
raizMSE


## EJ 3
# Enunciado: Dados los puntos (1,4), (2,2), (3,3), (4,2), (5,4).
# Calcule la recta de regresión de X sobre Y (X/Y). 
# Nota: El script repite el bloque para dos conjuntos de Y diferentes (Y1 e Y2).

# Caso 1: X/Y1
x <- c(1,2,3,4,5)
y1 <- c(4,2,3,2,4) #Y1
n <- c(1,1,1,1,1)

N <- sum(n)
sxi <- sum(x)
syi <- sum(y1)
sxi2 <- sum(x^2*n) # Ojo: aquí debería ser syi2 para la recta X/Y si usamos la matriz de Y
# (El código original usa sxi2 en la matriz, lo que calcularía Y/X, no X/Y. Revisar fórmula de teoría).
# Asumiendo cálculo de Y = a + bx como en el código:
M <- matrix(c(N, sxi, sxi, sxi2), ncol=2, nrow=2)
S <- c(syi, sum(x*y1*n))
solve(M,S) 
# y= a'+b'x: a'=3 y b'= 0, entonces y=3

# Caso 2: X/Y2
y1 <- c(1,3,5,7,9) #Y2 (sobreescribe variable)
# ... (cálculos análogos) ...


## EJ 4
# Enunciado: Estudio de la relación entre Densidad de tráfico (veh/km) y Velocidad (km/h).
# a) Diagrama de dispersión. b) Estimar r a ojo. c) Calcular r. d) Interpretación.

densidad <- c(43,55,40,52,39,33,50,33,44,21)
velocidad <- c(27.0,23.8,30.7,24.0,34.8,41.4,27.0,40.4,31.7,51.2)

# (a) Representar el diagrama de dispersi?n.
plot(densidad,velocidad)

# (b) Elegir valor de r: 0'968, -0'968, -0'198
# Respuesta visual: Pendiente negativa fuerte -> -0.968

# (c) Verificar calculando r.
r <- cor(densidad,velocidad)
r

# (d) Evidencia de asociación: A mayor densidad, menor velocidad (correlación negativa fuerte).


## EJ 5
# Enunciado: Cálculo de covarianza manual para datos constantes (caso degenerado).
x <- c(0,0,0,0,0)
y <- c(0,0,0,0,0)
n <- c(1,1,1,1,1)
N <- sum(n)

# Al ser constantes, la desviación típica es 0 y la correlación da NA o error.
cov <- (sum(x*y) - mean(x)*mean(y)) / N
cov


#### EJERCICIOS TEMA 3: SERIES TEMPORALES ####
library(zoo)
library(tidyverse)

## EJ 1
# Enunciado: Población anual 1973-1983.
# a) Medias móviles de orden 4 y 5. Gráfico.
# b) Tendencia lineal por mínimos cuadrados.
# c) Comparativa en tabla.

anios <- c(1973,1974,1975,1976,1977,1978,1979,1980,1981,1982,1983) # Corregido 'a?o' a 'anios'
pob <- c(9.47,9.26,8.86,8.25,7.81,8.01,7.55,7.24,7.01,6.88,7.03)

# (a) Medias móviles
tmp <- rollmean(pob,4)
p4 <- c(NA,NA,rollmean(tmp,2),NA,NA) # Centrada (orden par requiere doble media)
p5 <- c(NA,NA,rollmean(pob,5),NA,NA) # Orden impar (directa)

plot(pob, type="b")
lines(p4, col="red")
lines(p5, col="blue")

# (b) Tendencia Mínimos Cuadrados
regresion <- lm(pob ~ anios)
summary(regresion)
plot(pob)
lines(anios, predict(regresion), col="green")

# (c) Tabla comparativa
matrizdatos <- matrix(c(anios,pob,p4,p5,predict(regresion)), byrow = FALSE, ncol=5) # byrow=FALSE para columnas
colnames(matrizdatos) <- c("Año", "Real", "MM4", "MM5", "Tendencia")
matrizdatos


## EJ 3
# Enunciado: Producción cuatrimestral (3 periodos/año).
# Calcular índices de variación estacional (IVE) y desestacionalizar.
# Modelo Multiplicativo: Y = T*C*E*A -> E = Y / TC

y <- c(3.9, 4, 4.8, 5.1, 5, 5.5, 6.1, 6.3, 6.9)

# Tendencia (T) por medias móviles (k=3)
mm3 <- rollmean(as.numeric(y),3)
tc <- c(NA,mm3,NA) # Relleno extremos con NA

# Variación Estacional + Accidental (EA)
ea <- y/tc

# Índices Estacionales (E): Media de cada estación
# Al ser k=3, organizamos en matriz de 3 filas (estaciones)
ea_mat <- matrix(ea, nrow=3, byrow=FALSE) # Ojo: ajustar según cómo entren los datos
e <- rowMeans(ea_mat, na.rm=TRUE) # Esto promedia por fila (estación) si la matriz se llena correctamente

# Normalización (Suma debe ser k=3)
e <- e*3/sum(e)

# Desestacionalización (Y/E)
# Hay que repetir el vector 'e' tantas veces como años haya
e_vector <- rep(e, length.out=length(y))
des <- y/e_vector

# Ajuste y Predicción lineal sobre datos originales
trim <- c(1:9)
aLineal <- lm(y~trim)
predict(aLineal)


## EJ 4
# Enunciado: Serie MENSUAL (12 meses, 6 años). Hipótesis Multiplicativa.
# a) Tendencia por medias móviles orden 12 (centrada).
# b) Índices de Variación Estacional (IVE).
# c) Desestacionalizar y predecir.

Y <- matrix(c(178.2, 156.7, 164.2, 153.2, 157.5, 172.6, 185.9, 185.8, 165.0, 163.6, 169.0, 183.1,
              196.3, 162.8, 168.6, 156.9, 168.2, 180.2, 197.9, 195.9, 176.0, 166.4, 166.3, 183.9,
              197.3, 173.7, 173.2, 159.7, 175.2, 187.4, 202.6, 205.6, 185.6, 175.6, 176.3, 191.7,
              209.5, 186.3, 183.0, 169.5, 178.2, 186.7, 202.4, 204.9, 180.6, 179.8, 177.4, 188.9,
              200.0, 188.7, 187.5, 168.6, 175.7, 189.4, 216.1, 215.4, 191.5, 178.5, 178.6, 195.6,
              205.2, 179.6, 185.4, 172.4, 177.7, 202.7, 220.2, 210.2, 186.9, 181.4, 175.6, 195.6), byrow=TRUE, nrow=6)

# Tendencia (MM orden 12 centrada)
p12 <- rollmean(as.numeric(t(Y)), 12)
p12real <- rollmean(p12, 2)

# Estructura TC para operar
TC_vec <- c(rep(NA, 6), p12real, rep(NA, 6)) # 6 NAs al principio y final por centrado
TC <- matrix(TC_vec, byrow=TRUE, nrow=6)

# Componente EA
EA <- Y / TC

# Índices Estacionales (E): Promedio por columnas (meses)
E <- colMeans(EA, na.rm = TRUE)

# Normalización (Suma = 12)
E <- E*12/sum(E)

# Desestacionalización
E_mat <- matrix(rep(E, 6), byrow=TRUE, nrow=6)
des <- Y/E_mat

# (f) Predicción lineal
meses <- c(1:72)
datos <- as.vector(t(Y))
reg <- lm(datos ~ meses)
predict(reg)


## EJ 5
# Enunciado: Comparar Hipótesis Multiplicativa vs Aditiva en serie cuatrimestral.
y_vec <- c(66, 77, 100, 62, 77, 97, 63, 78, 96, 61, 78, 97)

# OPCIÓN 1: Multiplicativa (Y = T*C*E*A)
mm3 <- rollmean(y_vec, 3)
tc <- c(NA, mm3, NA)
ea <- y_vec / tc
# Promedios estacionales... (ver código original)

# OPCIÓN 2: Aditiva (Y = T+C+E+A) -> EA = Y - TC
EA_adit <- y_vec - tc
# Promedios estacionales se calculan sumando en lugar de multiplicando.


#### EJERCICIOS TEMA 5: MODELOS DE PROBABILIDAD ####

## EJ 2: Binomial
# Enunciado: Cajas con n=200 frutos. 
# Tratadas (30% del total): p=0.01 defectuosos. No tratadas: p=0.1 defectuosos.

# (a) P(X > 5 | Tratada) -> X~Bin(200, 0.01)
1 - pbinom(5, 200, 0.01)

# (b) P(X > 5 | No Tratada) -> X~Bin(200, 0.1)
1 - pbinom(5, 200, 0.1)

# (c) P(No Tratada | X=4) -> Bayes
# Verosimilitudes:
v_tratada <- dbinom(4, 200, 0.01)
v_no_tratada <- dbinom(4, 200, 0.1)
# P(NT|X=4) = (P(X=4|NT)*P(NT)) / P(X=4)
# P(X=4) = v_tratada*0.3 + v_no_tratada*0.7 (asumiendo 70% no tratadas)


## EJ 3: Poisson
# Enunciado: Averías en un barco. Tasa lambda diaria = 0.00028.
# (a) P(alguna avería en 365 días). Lambda_anual = 365 * 0.00028
1 - ppois(0, 365*0.00028)


## EJ 4: Normal
# Enunciado: Variable W ~ N(1.42, 0.1276)
# (b) P(W > 1.8)
1 - pnorm(1.8, mean=1.42, sd=0.1276)

# (c) Cuartil 3 (75%)
qnorm(0.75, mean=1.42, sd=0.1276)


## EJ 5: Exponencial
# Enunciado: P(X >= 18) = 0.07. Duración componente.
# (a) Hallar tasa lambda. 
# P(X>=18) = exp(-lambda*18) = 0.07 -> lambda = -ln(0.07)/18
lambda <- -log(0.07)/18
1/lambda # Media

# (b) Garantía 24 meses: P(X <= 24)
pexp(24, rate=lambda)


## EJ 8: Binomial Anidada
# Enunciado: Rueda defectuosa p=0.01. Coche tiene 5 ruedas.
# (a) P(Rueda peligrosa) = 0.01
# (b) P(Coche peligroso) = P(X >= 1) en Bin(5, 0.01)
prob_coche_malo <- 1 - pbinom(0, 5, 0.01)

# (c) 1000 coches. P(alguno tenga ruedas malas). Y ~ Bin(1000, prob_coche_malo)
1 - pbinom(0, 1000, prob_coche_malo)


#### EJERCICIOS TEMA 6: DISTRIBUCIONES EN EL MUESTREO ####

## EJ 2: Teorema Central del Límite
# Pob: media=12, sd=4.
# a) n=1 -> P(X > 14)
1 - pnorm(14, 12, 4)

# b) n=9 -> Error estándar = 4/sqrt(9) = 4/3. P(Media > 14)
1 - pnorm(14, 12, 4/3)


## EJ 5, 6, 7: Uso de tablas (t-Student, F-Snedecor)
# Buscar cuantiles para intervalos de confianza.
qt(0.975, df=27) # t-Student 95% dos colas
qf(0.95, df1=9, df2=8) # F-Snedecor 95%