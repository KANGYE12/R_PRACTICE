# ====================================================================
# TEMA 1: ESTADÍSTICA DESCRIPTIVA UNIDIMENSIONAL
# ====================================================================

## EJERCICIO 2
# Enunciado: Se han tomado 6 medidas de una variable (ej. pH): 6.7, 6.3, 6.5, 6.5, 6.4, 6.6.
# a) Obtenga la tabla de frecuencias absolutas y la suma total.
# b) Calcule la media, los cuantiles al 1% y 99%, y la desviación típica muestral.
# c) Calcule la varianza poblacional (asumiendo N como total poblacional).

# a)
datos <- c(6.7, 6.3, 6.5, 6.5, 6.4, 6.6)
cat("\n--- T1.EJ2.a: Frecuencias y Suma ---\n")
print(table(datos)) # Frecuencia absoluta
print(sum(datos))   # Suma total

# b)
cat("\n--- T1.EJ2.b: Media, Cuantiles y Desviación Típica Muestral ---\n")
media <- mean(datos)
print(paste("Media:", media))

print("Cuantiles (1% y 99%):")
print(quantile(datos, c(.01, .99)))

desv_tip_muestral <- sd(datos)
print(paste("Desviación Típica Muestral:", desv_tip_muestral))

# c)
cat("\n--- T1.EJ2.c: Varianza Poblacional ---\n")
N <- length(datos)
# Varianza poblacional: s^2 * (n-1) / n
varianza_poblacional <- sd(datos)^2 * (N-1) / N
print(paste("Varianza Poblacional:", varianza_poblacional))


# ====================================================================
# TEMA 2: DESCRIPTIVA BIDIMENSIONAL Y REGRESIÓN
# ====================================================================

## EJERCICIO 1
# Enunciado: Dada la variable Y con valores {2,5,7,10} y frecuencias {4,6,6,4}.
# a) Calcule media, desviación típica, varianza y momentos de orden 3 y 4. Analice la simetría y la curtosis.
# b) Dados nuevos datos (X,Y), calcule las rectas de regresión Y/X y X/Y.
# c) Calcule la correlación lineal (r) y el error cuadrático medio (RMSE) de Y/X.

# a) Análisis de la variable Y
cat("\n\n--- T2.EJ1.a: Análisis Variable Y ---\n")
# Para este ejercicio se asumen las funciones de R para valores únicos.
y <- c(2, 5, 7, 10) # Valores
n <- c(4, 6, 6, 4)  # Frecuencias
N <- sum(n)         # Total de observaciones (20)

# --- MEDIA PONDERADA ---
# NO usar mean(y) directamente porque ignora las frecuencias.
media_ponderada <- sum(y * n) / N
# O también: weighted.mean(y, n)
print(paste("Media Real (Ponderada):", media_ponderada))

# --- DESVIACIÓN TÍPICA PONDERADA ---
# NO usar sd(y) directamente.
var_ponderada <- sum((y - media_ponderada)^2 * n) / (N - 1) # Cuasivarianza muestral
sd_ponderada <- sqrt(var_ponderada)
print(paste("Desviación Típica Muestral (Ponderada):", sd_ponderada))

# --- MOMENTOS Y FORMA ---
if (!requireNamespace("moments", quietly = TRUE)) {
  install.packages("moments")
}
library(moments)

# Para skewness y kurtosis con frecuencias, lo mejor es expandir los datos:
y_expandida <- rep(y, n) # Crea un vector con 20 datos: 2,2,2,2,5,5...

sesgo <- skewness(y_expandida)
curtosis_exceso <- kurtosis(y_expandida) - 3

print(paste("Sesgo (Skewness):", sesgo))
print(paste("Curtosis (Exceso):", curtosis_exceso))


# b) Calcular las rectas de regresión de Y/X y X/Y.
cat("\n--- T2.EJ1.b: Rectas de Regresión ---\n")
x <- c(0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 4, 5)
y <- c(2, 5, 7, 2, 5, 7, 10, 2, 5, 7, 10, 7, 10, 10, 10)
n <- c(3, 3, 1, 3, 4, 3, 1, 1, 2, 2, 1, 1, 2, 3, 2)

sx <- sum(x*n); sx2 <- sum(x^2*n)
sy <- sum(y*n); sy2 <- sum(y^2*n)
sxy <- sum(x*y*n)
N_b <- sum(n)

# Recta Y/X (Y = a + bx)
M_YX <- matrix(c(N_b, sx, sx, sx2), ncol=2, nrow=2)
S_YX <- c(sy, sxy)
sol_YX <- solve(M_YX, S_YX)
print("Coeficientes Y/X (a, b):")
print(sol_YX) # y = a + bx

# Recta X/Y (X = a' + b'y)
M_XY <- matrix(c(N_b, sy, sy, sy2), ncol=2, nrow=2)
S_XY <- c(sx, sxy)
sol_XY <- solve(M_XY, S_XY)
print("Coeficientes X/Y (a', b'):")
print(sol_XY) # x = a' + b'y

# c) Coeficiente de correlación lineal (r) y Raíz del Error Cuadrático Medio (RMSE)
cat("\n--- T2.EJ1.c: Correlación y RMSE ---\n")
correlacion_r <- cor(x, y) # Uso correcto de la función nativa
print(paste("Coeficiente de Correlación (r):", correlacion_r))

# RMSE (Raíz del Error Cuadrático Medio) para el modelo Y/X
pendiente_b <- sol_YX[2]
raizMSE <- sqrt(var(y) - pendiente_b^2 * var(x))
print(paste("Raíz del Error Cuadrático Medio (RMSE) Y/X:", raizMSE))


## EJERCICIO 3
# Enunciado: Dados los puntos (1,4), (2,2), (3,3), (4,2), (5,4) y (1,3), (2,5), (3,7), (4,9), (5,11) [Caso Y2].
# Calcule la recta de regresión de X sobre Y (X/Y) para ambos casos.

cat("\n\n--- T2.EJ3: Recta X/Y (Caso 1 y 2) ---\n")
# Caso 1: X/Y1 (X depende de Y1)
x <- c(1, 2, 3, 4, 5)
y1 <- c(4, 2, 3, 2, 4)
N <- length(x)

sxi <- sum(x); syi <- sum(y1)
syi2 <- sum(y1^2); sxiyi <- sum(x*y1)

# Recta X/Y1 (X = a' + b'y)
M_XY1 <- matrix(c(N, syi, syi, syi2), ncol=2, nrow=2)
S_XY1 <- c(sxi, sxiyi)
print("Coeficientes X/Y1:")
print(solve(M_XY1, S_XY1))

# Caso 2: X/Y2 (X depende de Y2)
y2 <- c(1, 3, 5, 7, 9)

syi <- sum(y2)
syi2 <- sum(y2^2); sxiyi <- sum(x*y2)

# Recta X/Y2 (X = a' + b'y)
M_XY2 <- matrix(c(N, syi, syi, syi2), ncol=2, nrow=2)
S_XY2 <- c(sxi, sxiyi)
print("Coeficientes X/Y2:")
print(solve(M_XY2, S_XY2))


## EJERCICIO 5
# Enunciado: Cálculo de covarianza y correlación manual para datos constantes (caso degenerado).
cat("\n--- T2.EJ5: Caso Degenerado ---\n")
x <- c(0, 0, 0, 0, 0)
y <- c(0, 0, 0, 0, 0)
N <- length(x)

# Varianza y desviación de datos constantes es 0
print(paste("Desviación Típica de X:", sd(x)))
print(paste("Desviación Típica de Y:", sd(y)))

# Cálculo manual de covarianza
medX <- mean(x); medY <- mean(y)
cov_manual <- (sum(x*y) - medX*medY * N) / N # Ajuste de fórmula
print(paste("Covarianza (manual):", cov_manual))

# Coeficiente de correlación
r_constantes <- cor(x, y)
print(paste("Correlación (r):", r_constantes)) # R devolverá NA o error (caso indeterminado)


# ====================================================================
# TEMA 3: SERIES TEMPORALES
# ====================================================================

if (!requireNamespace("zoo", quietly = TRUE)) {
  install.packages("zoo")
}
library(zoo)

## EJERCICIO 1
# Enunciado: Población anual 1973-1983.
# a) Medias móviles de orden 4 y 5. Gráfico.
# b) Tendencia lineal por mínimos cuadrados.
# c) Presentar en una tabla los valores de la tendencia obtenidos y comparar.

cat("\n\n--- T3.EJ1: Medias Móviles y Regresión Lineal ---\n")
anios <- c(1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981, 1982, 1983)
pob <- c(9.47, 9.26, 8.86, 8.25, 7.81, 8.01, 7.55, 7.24, 7.01, 6.88, 7.03)

# (a) Medias móviles
tmp <- rollmean(pob, 4)
p4 <- c(NA, NA, rollmean(tmp, 2), NA, NA) # Centrada (orden par)
p5 <- c(NA, NA, rollmean(pob, 5), NA, NA) # Orden impar (directa)

plot(anios, pob, type="b", main="Serie Original y Tendencias", ylab="Población", xlab="Años")
lines(anios, p4, col="red")
lines(anios, p5, col="blue")

# (b) Tendencia Mínimos Cuadrados
regresion <- lm(pob ~ anios)
tendencia_mc <- predict(regresion)
lines(anios, tendencia_mc, col="green")

# (c) Tabla comparativa
cat("\n--- T3.EJ1.c: Tabla Comparativa ---\n")
# Usamos matrix() con cbind() para asegurar que los vectores son columnas
matrizdatos <- cbind(anios, pob, MM4=p4, MM5=p5, Tendencia=tendencia_mc)
colnames(matrizdatos) <- c("Año", "Real", "MM4", "MM5", "Tendencia")
print(matrizdatos)


## EJERCICIO 3
# Enunciado: Producción cuatrimestral (3 periodos/año).
# Calcular índices de variación estacional (IVE) y desestacionalizar. Interpreta los resultados e indica el año y cuatrimestre con producción anormalmente grande si lo hubiese.

cat("\n\n--- T3.EJ3: IVE Multiplicativo y Desestacionalización ---\n")
y <- c(3.9, 4, 4.8, 5.1, 5, 5.5, 6.1, 6.3, 6.9)

# Tendencia (T) por medias móviles (k=3)
mm3 <- rollmean(as.numeric(y), 3)
tc <- c(NA, mm3, NA)

# Variación Estacional + Accidental (EA)
ea <- y/tc

# Índices Estacionales (E)
ea_mat <- matrix(ea, nrow=3, byrow=FALSE)
e <- rowMeans(ea_mat, na.rm=TRUE)

# Normalización (Suma debe ser k=3)
e_normalizado <- e * 3 / sum(e)
print("Índices Estacionales Normalizados (E):")
print(e_normalizado)

# Desestacionalización (Y/E)
e_vector <- rep(e_normalizado, length.out=length(y))
des <- y / e_vector
print("Serie Desestacionalizada:")
print(des)

# Análisis de la componente accidental (A = EA / E)
a_componente <- ea[!is.na(ea)] / e_vector[!is.na(tc)]
print("Componente Accidental (A):")
print(a_componente)


# ====================================================================
# TEMA 5: MODELOS DE PROBABILIDAD
# ====================================================================

## EJERCICIO 2: Binomial
# Enunciado: Cajas con n=200 frutos. Tratadas (30%): p=0.01 defectuosos. No tratadas (70%): p=0.1.
# a) P(X > 5 | Tratada). b) P(X > 5 | No Tratada). c) P(No Tratada | X=4).
# d) P(Tratada | X>5). e) P(2 atacados) en muestra de 5 (Tratada). f) P(2 atacados) en muestra de 5 con 22 atacados (Hipergeométrica).

cat("\n\n--- T5.EJ2: Probabilidad Binomial y Bayes ---\n")
n_frutos <- 200
p_t <- 0.01; p_nt <- 0.1
P_T <- 0.3; P_NT <- 0.7

# a) P(X > 5 | Tratada), X~Bin(200, 0.01)
p_a <- 1 - pbinom(5, n_frutos, p_t)
print(paste("a) P(X > 5 | Tratada):", p_a))

# b) P(X > 5 | No Tratada), X~Bin(200, 0.1)
p_b <- 1 - pbinom(5, n_frutos, p_nt)
print(paste("b) P(X > 5 | No Tratada):", p_b))

# c) P(No Tratada | X=4) (Bayes)
v_t <- dbinom(4, n_frutos, p_t)
v_nt <- dbinom(4, n_frutos, p_nt)
p_x4 <- v_t * P_T + v_nt * P_NT
p_c <- (v_nt * P_NT) / p_x4
print(paste("c) P(No Tratada | X=4):", p_c))

# f) P(2 atacados) en muestra de 5 con 22 atacados (Hipergeométrica)
p_f <- dhyper(2, 22, n_frutos - 22, 5) # m=22, n=178, k=5
print(paste("f) P(2 atacados) en muestra (Hipergeométrica):", p_f))


## EJERCICIO 3: Poisson
# Enunciado: Averías en un barco. Tasa lambda diaria = 0.00028.
# a) P(alguna avería en 365 días).

cat("\n--- T5.EJ3: Probabilidad Poisson ---\n")
lambda_diaria <- 0.00028
lambda_anual <- 365 * lambda_diaria

# a) P(alguna avería en 365 días) = 1 - P(0 averías)
p_a <- 1 - ppois(0, lambda_anual)
print(paste("a) P(alguna avería en 365 días):", p_a))


## EJERCICIO 4: Normal
# Enunciado: Variable W ~ N(1.42, 0.1276).
# b) Hallar P(W > 1.8). c) Hallar el cuartil 3 (75%).

cat("\n--- T5.EJ4: Probabilidad Normal ---\n")
mu <- 1.42; sigma2 <- 0.1276
sigma <- sqrt(sigma2)

# b) P(W > 1.8)
p_b <- 1 - pnorm(1.8, mean=mu, sd=sigma)
print(paste("b) P(W > 1.8):", p_b))

# c) Cuartil 3 (75%)
q3 <- qnorm(0.75, mean=mu, sd=sigma)
print(paste("c) Cuartil 3 (75%):", q3))


## EJERCICIO 9: Exponencial y Bayes
# Enunciado: Duración de componente X ~ Exp(lambda) con media E[X]=8 (Tipo A, 65%) o Y ~ Exp(lambda) con media E[Y]=9 (Tipo B, 35%).
# a) P(dure más de 15 años | Tipo A). b) P(Tipo A | X > 15).

cat("\n--- T5.EJ9: Probabilidad Exponencial y Bayes ---\n")
# a) P(X > 15). Tipo A: E[X]=8 -> lambda_A = 1/8
lambda_A <- 1/8
p_a <- 1 - pexp(15, rate=lambda_A)
print(paste("a) P(X > 15 | Tipo A):", p_a))

# b) P(Tipo A | X > 15) (Bayes)
P_A <- 0.65; P_B <- 0.35
lambda_B <- 1/9

p_x15_A <- p_a
p_x15_B <- 1 - pexp(15, rate=lambda_B)
p_x15 <- p_x15_A * P_A + p_x15_B * P_B

p_b <- (p_x15_A * P_A) / p_x15
print(paste("b) P(Tipo A | X > 15):", p_b))


# ====================================================================
# TEMA 6: DISTRIBUCIONES EN EL MUESTREO
# ====================================================================

## EJERCICIO 2: Teorema Central del Límite
# Enunciado: Población: media=12, sd=4.
# a) n=1. P(X > 14). b) n=9. P(Media > 14).

cat("\n\n--- T6.EJ2: Teorema Central del Límite ---\n")
mu <- 12; sigma <- 4

# a) n=1. P(X > 14).
p_a <- 1 - pnorm(14, mean=mu, sd=sigma)
print(paste("a) P(X > 14 | n=1):", p_a))

# b) n=9. P(Media > 14). Error estándar = sigma / sqrt(n)
error_estandar <- sigma / sqrt(9)
p_b <- 1 - pnorm(14, mean=mu, sd=error_estandar)
print(paste("b) P(Media > 14 | n=9):", p_b))


## EJERCICIOS 5, 6, 7 y 8: Uso de Cuantiles
# Enunciado: Determinar cuantiles de t-Student y F-Snedecor.

cat("\n--- T6.EJ5, 6, 7, 8: Cuantiles t-Student y F-Snedecor ---\n")

# EJERCICIO 5.a: t-Student Cuantil 0.975 (95% dos colas) con df=27
t_q5a <- qt(0.975, df=27)
print(paste("T5.a: qt(0.975, 27) =", t_q5a))

# EJERCICIO 6: t-Student Cuantil 0.95 (90% una cola) con df=17
t_q6 <- qt(1-0.05, df=17)
print(paste("T6: qt(0.95, 17) =", t_q6))

# EJERCICIO 7: F-Snedecor
# F_0.95 con gl1=9, gl2=8
f_q7_095 <- qf(1-0.05, df1=9, df2=8)
print(paste("T7: qf(0.95, 9, 8) =", f_q7_095))

# EJERCICIO 8: F-Snedecor
# F_0.975 con gl1=11, gl2=7
f_q8_0975 <- qf(1-0.025, df1=11, df2=7)
print(paste("T8: qf(0.975, 11, 7) =", f_q8_0975))

# F_0.025 con gl1=11, gl2=7
f_q8_0025 <- qf(0.025, df1=11, df2=7)
print(paste("T8: qf(0.025, 11, 7) =", f_q8_0025))