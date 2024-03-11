#Un educador musical evaluó a miles de estudiantes finlandeses sobre 
#sus habilidades musicales nativas para establecer normas nacionales en Finlandia.
# A continuación se muestran las estadísticas resumidas para parte del conjunto de datos. 
#Estas estadísticas se basan en una muestra de n = 96 alumnos finlandeses de 12º grado.
# las variables medidas fueron:

#X1: Melodía
#X2: Armonía
#X3: Tempo
#X4: Métrica
#X5: Fraseo
#X6: Balance
#X7: Estilo.

# media muestra de cada una de las variables
xbar <- c(28.1, 26.6, 35.4, 34.2, 23.6, 22, 22.7)

# desviación estándar muestral de cada una de las variables
Si <- c(5.76, 5.85, 3.82, 5.12, 3.76, 3.93, 4.03)

n <- 96
p <- 7

# Tomando como base estás mismas características en estudiantes de Norteamerica,
# el profesor quiere investigar si las puntuaciones promedio son iguales.
# en Norteamética las puntuaciones promedio son: 31, 27, 34, 31, 23, 22, 22 respectivamente.

#H0: µ=µ0 vrs H1: µ≠µ0
mu0 <- c(31, 27, 34, 31, 23, 22, 22)

# rechazamos H0 si la estadística de prueba es mayor que el cuantil de la distribución chi cuadrado
alpha <- 0.05

# Cuantil de la dsitribución chi cuadrado
cc <- qchisq(alpha, df=p, lower.tail = F)
cc


# estadística de prueba
# n(xbar-mu0)^tS^{-1}(xbar-mu0)

# Vamos a asumir independencia, es decir que la covarianza entre cada una de estas
# variables es cero.


# construimos la matriz de covarianza
S <- diag(Si^2, nrow = 7)
S

xbar<- matrix(xbar, nrow = 7)
xbar

mu0 <- matrix(mu0, nrow = 7)
mu0

# calculamos la estadística de prueba
n*t(xbar-mu0)%*%solve(S)%*%(xbar-mu0)


cc

# Como la estadística de prueba es mayor que el cuantil de la distribución, entonces
# rechazamos H0.

# Concluimos que las puntuaciones promedio de los estudiantes finlandeses son diferentes a las 
# puntuaciones promedio de los estudiantes Norteamericanos.


# Construyamos Intervalos de confianza
# Como proyecciones de la elipse de confianza
# para la puntuación promedio de melodía

xbar[1]-sqrt(cc)*Si[1]/sqrt(n)
xbar[1]+sqrt(cc)*Si[1]/sqrt(n)


# como las funciones en R estan vectorizadas

LI <- xbar-sqrt(cc)*Si/sqrt(n)
LS <- xbar+sqrt(cc)*Si/sqrt(n)

caracteristica <- c("Melodía", "Armonía", "Tempo",
                    "Métrica", "Fraseo", "Balance",
                    "Estilo")

intervalo_1 <- data.frame(caracteristica,LI,LS)

























