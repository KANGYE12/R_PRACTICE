# ==============================================================================
#  CHULETA MAESTRA DE ESTADÍSTICA E INFERENCIA EN R
# ==============================================================================

# ------------------------------------------------------------------------------
# BLOQUE 1: FUNDAMENTOS (PREFIJOS DE COMANDOS)
# ------------------------------------------------------------------------------
# d = Densidad (Continuas) o Probabilidad puntual (Discretas) -> P(X=x)
# p = Probabilidad Acumulada (P(X <= x)) -> P-valor / Área a la izquierda
# q = Cuantil (Dada el área, dame el valor) -> Valores Críticos (Z, t, Chi)
# r = Random -> Generar datos aleatorios para simulaciones


# ------------------------------------------------------------------------------
# BLOQUE 2: MODELOS DE DISTRIBUCIÓN DE PROBABILIDAD
# ------------------------------------------------------------------------------

# === 2.1 DISTRIBUCIONES DISCRETAS ===

# --- A. BINOMIAL (Éxitos en n intentos) ---
n <- 10; p <- 0.5 #10 veces con 50% de que ocurra
dbinom(5, n, p)        # P(X = 5)  -> Exactamente 5 éxitos
pbinom(5, n, p)        # P(X <= 5) -> 5 o menos Acumulada
qbinom(0.95, n, p)     # Cuantil 95%
#8 = si repito experimento muchas veces, 95% obtendré 8 exitos o menos

# --- B. POISSON (Eventos por intervalo) ---
lambda <- 4 #esperar que pasen 4 eventos en cambio calcular probabilidad de que pase
dpois(5, lambda)       # P(X = 5) -> Exactamente 5 éxitos
ppois(5, lambda)       # P(X <= 5) -> 5 o menos Acumulada

# --- C. GEOMÉTRICA (Fallos hasta el primer éxito) ---
# Probabilidad de que encuentre algo de 4% a la sexta
k <- 6; p_geo <- 0.04
dgeom(k-1, prob = p_geo) # Probabilidad de éxito en el intento k

# === 2.2 DISTRIBUCIONES CONTINUAS ===

# --- D. NORMAL (Gaussiana) ---
media <- 170; sd_pob <- 10
# Probabilidad de un INDIVIDUO
pnorm(180, mean = media, sd = sd_pob) #Probabilidad elegir alguien menos de 180
# Probabilidad de una MEDIA MUESTRAL (Error estándar)
n <- 50
pnorm(180, mean = media, sd = sd_pob / sqrt(n))

# --- E. T-STUDENT (Muestras pequeñas, Sigma desconocida) ---
gl <- 24 # n - 1
pt(2.5, df = gl)       # Probabilidad acumulada de sacar menos de 2.5
qt(0.975, df = gl)     # Valor sacar para estar por encima 97'5%

# --- F. CHI-CUADRADO (Varianza) ---
gl <- 24
pchisq(15, df = gl)    # Probabilidad acumulada
qchisq(0.95, df = gl)  # Valor crítico

# --- G. UNIFORME (Probabilidad constante) ---
min <- 0; max <- 10
punif(3, min, max)     # P(X <= 3)

# --- H. EXPONENCIAL (Tiempo de espera) ---
lambda <- 0.5
pexp(2, rate = lambda) # P(X <= 2)

# --- I. F DE SNEDECOR (Cociente de varianzas) ---
df1 <- 10; df2 <- 15
qf(0.95, df1, df2)     # Valor crítico F

# ------------------------------------------------------------------------------
# BLOQUE 3: INFERENCIA DE UNA MUESTRA (CÁLCULO MANUAL DE IC)
# ------------------------------------------------------------------------------
# Datos ejemplo:
x_bar <- 100; s <- 15; n <- 25; alpha <- 0.05

# --- A. INTERVALO PARA LA MEDIA (t-Student) ---
t_crit <- qt(1 - alpha/2, df = n-1)
se <- s / sqrt(n)
ic_media <- x_bar + c(-1, 1) * t_crit * se
cat("IC Media:", ic_media, "\n")

# --- B. INTERVALO PARA LA PROPORCIÓN ---
p_hat <- 0.45
Z_crit <- qnorm(1 - alpha/2)
se_prop <- sqrt((p_hat * (1 - p_hat)) / n)
ic_prop <- p_hat + c(-1, 1) * Z_crit * se_prop 
cat("IC Proporción:", ic_prop, "\n")

# --- C. INTERVALO PARA LA VARIANZA (Chi-Cuadrado) ---
# ¡Cuidado! Es asimétrico. Se cruzan los cuantiles.
chi_der <- qchisq(1 - alpha/2, df = n-1) # Denominador grande -> Límite inferior
chi_izq <- qchisq(alpha/2, df = n-1)     # Denominador pequeño -> Límite superior
ic_var <- c((n-1)*s^2 / chi_der, (n-1)*s^2 / chi_izq)
cat("IC Varianza:", ic_var, "\n")

# ------------------------------------------------------------------------------
# BLOQUE 4: INFERENCIA DE DOS MUESTRAS (TESTS AUTOMÁTICOS)
# ------------------------------------------------------------------------------
# Datos ejemplo:
grupoA <- c(20, 22, 19, 23, 21)
grupoB <- c(18, 17, 20, 16, 19)

# --- PASO 1: COMPROBAR VARIANZAS (F-Test) ---
# H0: Varianzas Iguales vs H1: Diferentes
var.test(grupoA, grupoB) 
# Si p-value > 0.05 -> Asumimos var.equal = TRUE

# --- PASO 2: COMPARAR MEDIAS (T-Test Independiente) ---
# H0: Medias Iguales vs H1: Diferentes
t.test(grupoA, grupoB, var.equal = TRUE, conf.level = 0.95)

# --- CASO ESPECIAL: DATOS PAREADOS (Mismos sujetos antes/después) ---
antes <- c(38, 39, 40); despues <- c(37, 38, 38)
t.test(despues, antes, paired = TRUE)

# ------------------------------------------------------------------------------
# BLOQUE 5: DATOS CATEGÓRICOS (TESTS NO PARAMÉTRICOS)
# ------------------------------------------------------------------------------

# --- A. BONDAD DE AJUSTE (¿Se ajusta a un modelo teórico?) ---
observados <- c(30, 70)
prob_teoricas <- c(0.5, 0.5) # Ej: Moneda justa
chisq.test(x = observados, p = prob_teoricas)

# --- B. INDEPENDENCIA (¿Hay relación entre variables?) ---
tabla <- matrix(c(10, 20, 30, 40), nrow = 2)
chisq.test(tabla)

# ------------------------------------------------------------------------------
# BLOQUE 6: SERIES TEMPORALES (DESCOMPOSICIÓN - MODELO MULTIPLICATIVO)
# ------------------------------------------------------------------------------
# Modelo: X = T * S * R (Tendencia * Estacionalidad * Residuo/Aleatorio)
# Objetivo: Aislar componentes usando Medias Móviles (Moving Averages).

# === PASO 1: DEFINIR LA SERIE TEMPORAL ===
datos_brutos <- c(3459, 3458, 4002, 4564, 4221, 4529, 4466, 4137, 4126, 4259, 4240, 4936)
# frequency = 12 (Mensual), frequency = 4 (Trimestral)
serie <- ts(datos_brutos, start = c(1992, 1), frequency = 12)

# === PASO 2: CALCULAR LA TENDENCIA (T) ===
# Usamos medias móviles centradas.
# IMPORTANTE: Los pesos del filtro dependen de la frecuencia.

# A. Para datos MENSUALES (f=12) -> Ventana de 13 términos (1/2 extremos)
pesos_mensual <- c(0.5, rep(1, 11), 0.5) / 12
tendencia <- stats::filter(serie, pesos_mensual)

# B. Para datos TRIMESTRALES (f=4) -> Ventana de 5 términos (1/2 extremos)
pesos_trimestral <- c(0.5, rep(1, 3), 0.5) / 4
# tendencia_trim <- stats::filter(serie_trim, pesos_trimestral)

# === PASO 3: OBTENER COMPONENTES INTERMEDIOS ===
# Si el modelo es Multiplicativo: Serie = T * S * R

# Componente (Estacional * Aleatoria) = Serie / Tendencia
ratio_SR <- serie / tendencia 

# === PASO 4: CALCULAR ÍNDICES ESTACIONALES (S) ===
# Promedio de los ratios para cada mes (ignorando NAs)
# Truco: Convertir a matriz donde cada columna es un mes
matriz_estacional <- matrix(ratio_SR, ncol = frequency(serie), byrow = TRUE)
indices_brutos <- colMeans(matriz_estacional, na.rm = TRUE)

# Normalizar (para que su media sea 1)
indices_S <- indices_brutos / mean(indices_brutos) 

# === PASO 5: OBTENER EL COMPONENTE ALEATORIO (R) ===
# Desestacionalizar la serie original: X / S
# (Hay que repetir los índices S para cubrir toda la longitud de la serie)
serie_desestacionalizada <- serie / indices_S

# Aleatorio = Desestacionalizada / Tendencia
componente_aleatorio <- serie_desestacionalizada / tendencia

# Visualizar resultado específico (Ej: Dic 2002)
# print(componente_aleatorio) # Buscar la fecha concreta en la salida