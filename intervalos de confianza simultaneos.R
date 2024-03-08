library(ggplot2)
library(ggpubr)
library(gridExtra)
library(ggforce)
library(ellipse)


# El archivo college.csv contiene las puntuaciones de el examen de admisión a una universidad de
# 88 candidatos, las variables medidas fueron las puntuaciones obtenidas en 
# Ciencias sociales e Historia
# Verbal
# Ciencias

college <- read.csv("college.csv")
n <- 88
p <- 3

# estamos bajo el supuesto de normalidad multivariada
# µ1: El puntaje promedio poblacional en ciencias sociales e Historia
# µ2: El puntaje promedio poblacional en la prueba verbal.
# µ3: El puntaje promedio poblacional en la prueba de ciencias.

# queremos contruir IC para µ1, µ2 y µ3 de forma simultanea

# primero vamos a contruir las ellipses de confianza tomando las variables dos a dos

# calculemos las estadísticas descriptivas de la muestra
xbar <- colMeans(college)
xbar
S <- cov(college)
S


c <- sqrt((p*(n-1)/(n-p))*qf(0.05,p,n-p, lower.tail = F))

#elipse para ciencias sociales e Historia y verbal

plot(ellipse::ellipse(S[1:2,1:2],centre=c(xbar[1],xbar[2]),t=c/sqrt(n)),
     type="l",xlab="Ciencias sociales e Historia ",ylab="Prueba Verbal")

#elipse para ciencias sociales e Historia y Ciencia

plot(ellipse::ellipse(S[1:3,1:3],centre=c(xbar[1],xbar[3]),t=c/sqrt(n)),
     type="l",xlab="Ciencias sociales e Historia ",ylab="Prueba Ciencias")

#elipse para prueba verbal y Ciencia

plot(ellipse::ellipse(S[2:3,2:3],centre=c(xbar[2],xbar[3]),t=c/sqrt(n)),
     type="l",xlab="Prueba Verbal ",ylab="Prueba Ciencias")


# tomemos las puntuaciones de ciencia y verbal y construyamos el IC 
# de forma simultanea para ambas medias (µ2 y µ3)


# El IC t-student
# para Ciencias (µ3)

alpha <- 0.05
t <- qt(alpha/2, df=n-1, lower.tail = F)

LI_C <- as.numeric(xbar[3]-t*sqrt(S[3,3]/n))
LS_C <- as.numeric(xbar[3]+t*sqrt(S[3,3]/n))
cat(LI_C,LS_C)

# Para verbal

LI_V <- as.numeric(xbar[2] - t*sqrt(S[2,2]/n))
LS_V <- as.numeric(xbar[2]+t*sqrt(S[2,2]/n))

cat(LI_V,LS_V)


plot(ellipse::ellipse(S[2:3,2:3],centre=c(xbar[2],xbar[3]),t=c/sqrt(n)),
     type="l",xlab="Prueba Verbal ",ylab="Prueba Ciencias")

abline(v=c(LI_V,LS_V), col="red")
abline(h=c(LI_C,LS_C), col="red")

  

# Ahora construyamos los Intervalos de confianza T^2

# para Ciencias (µ3)

alpha <- 0.05
T2 <- p*(n-1)/(n-p)*qf(alpha, p, n-p, lower.tail = F) # Cuantil de la distribución T^2 de Hotelling  

LI_C_T <- as.numeric(xbar[3] - sqrt(T2)*sqrt(S[3,3]/n))
LS_C_T <- as.numeric(xbar[3] + sqrt(T2)*sqrt(S[3,3]/n))
cat(LI_C_T,LS_C_T)

# para Verbal (µ2)

LI_V_T <- as.numeric(xbar[2] - sqrt(T2)*sqrt(S[2,2]/n))
LS_V_T <- as.numeric(xbar[2] + sqrt(T2)*sqrt(S[2,2]/n))

# Ellipse a mano

plot(ellipse::ellipse(S[2:3,2:3],centre=c(xbar[2],xbar[3]),t=c/sqrt(n)),
     type="l",xlab="Prueba Verbal ",ylab="Prueba Ciencias")

abline(v=c(LI_V,LS_V), col="red")
abline(h=c(LI_C,LS_C), col="red")

abline(v=c(LI_V_T,LS_V_T ), col="blue")
abline(h=c(LI_C_T,LS_C_T), col="blue")


# ahora construyamos los intervalos de confianza usando Bonferroni

# para Ciencias (µ3)

alpha_B <- alpha/p
alpha_B


t_B <- qt(alpha_B/2, df=n-1, lower.tail = F)

LI_C_B <- as.numeric(xbar[3]-t_B *sqrt(S[3,3]/n))
LS_C_B <- as.numeric(xbar[3]+t_B *sqrt(S[3,3]/n))
cat(LI_C_B,LS_C_B)

# Para verbal

LI_V_B <- as.numeric(xbar[2] - t_B*sqrt(S[2,2]/n))
LS_V_B <- as.numeric(xbar[2]+t_B*sqrt(S[2,2]/n))

cat(LI_V_B,LS_V_B)

plot(ellipse::ellipse(S[2:3,2:3],centre=c(xbar[2],xbar[3]),t=c/sqrt(n)),
     type="l",xlab="Prueba Verbal ",ylab="Prueba Ciencias")

abline(v=c(LI_V,LS_V), col="red")
abline(h=c(LI_C,LS_C), col="red")

abline(v=c(LI_V_T,LS_V_T ), col="blue")
abline(h=c(LI_C_T,LS_C_T), col="blue")

abline(v=c(LI_V_B,LS_V_B), col="darkgreen")
abline(h=c(LI_C_B,LS_C_B), col="darkgreen")



## Intervalo de confianza para la diferencia de medias poblacionales de
# las puntuaciones de ciencia y verbal (µ2-µ3)

alpha <- 0.05
T2 <- p*(n-1)/(n-p)*qf(alpha, p, n-p, lower.tail = F) # Cuantil de la distribución T^2 de Hotelling

LI_DIFF <- as.numeric((xbar[2]-xbar[3])-sqrt(T2)*sqrt((S[2,2]-2*S[2,3]+S[3,3])/n))
LS_DIFF <- as.numeric((xbar[2]-xbar[3])+sqrt(T2)*sqrt((S[2,2]-2*S[2,3]+S[3,3])/n))

c(LI_DIFF,LS_DIFF )









