library(ggplot2)
library(ggpubr)
library(gridExtra)
library(ggforce)


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

elipse1 <- ggplot(college, aes(Ciencias.sociales.e.historia, Verbal))+
  stat_conf_ellipse() + theme_minimal()

elipse2 <- ggplot(college, aes(Ciencias.sociales.e.historia, Ciencia))+
  stat_conf_ellipse() + theme_minimal()
  
elipse3 <- ggplot(college, aes(Verbal, Ciencia))+
  stat_conf_ellipse() + theme_minimal()

grid.arrange(elipse2,elipse3,elipse1, ncol=2)

# tomemos las puntuaciones de ciencia y verbal y construyamos el IC 
# de forma simultanea para ambas medias (µ2 y µ3)


# calculemos las estadísticas descriptivas de la muestra
xbar <- colMeans(college)
xbar
S <- cov(college)
S

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


grafico1 <- ggplot(college, aes(Verbal, Ciencia))+
  stat_conf_ellipse() + theme_minimal() +
  geom_hline(yintercept=c(LI_C,LS_C), col="red") +
  geom_vline(xintercept =c(LI_V, LS_V), col="red")

grafico1
# Ahora construyamos los Intervalos de confianza T^2

# para Ciencias (µ3)

alpha <- 0.05
T2 <- p*(n-1)/(n-p)*qf(alpha, p, n-p, lower.tail = F) # Cuantil de la distribución T^2 de Hotelling  

LI_C_T <- as.numeric(xbar[3] - sqrt(T2)*sqrt(S[3,3]/n))
LS_C_T <- as.numeric(xbar[3] + sqrt(T2)*sqrt(S[3,3]/n))
cat(LI_C_T,LS_C_T)

grafico1+
  geom_hline(yintercept=c(LI_C_T,LS_C_T), col="blue") 


# para Verbal (µ2)

LI_V_T <- as.numeric(xbar[2] - sqrt(T2)*sqrt(S[2,2]/n))
LS_V_T <- as.numeric(xbar[2] + sqrt(T2)*sqrt(S[2,2]/n))

grafico1+
  geom_hline(yintercept = c(LI_C_T,LS_C_T), col="blue") +
  geom_vline(xintercept = c(LI_V_T,LS_V_T), col="blue")

# Ellipse a mano
c=sqrt((p*(n-1)/(n-p))*qf(0.05,p,n-p, lower.tail = F))

plot(ellipse::ellipse(S[2:3,2:3],centre=c(xbar[2],xbar[3]),t=c/sqrt(n)),
     type="l",xlab="Verabal",ylab="Ciencia")
abline(h=c(LI_C_T,LS_C_T), col="red")
abline(v=c(LI_V_T,LS_V_T), col="red")





