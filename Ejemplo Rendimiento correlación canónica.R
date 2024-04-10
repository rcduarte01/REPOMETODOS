library(CCA)
# Correlación canónica
rendimiento <- read.csv("rendimiento.csv")

# 3 Variables independientes 
# X1: Peso (En libras)
# X2: Cintura (En pulgadas)
# X3: Pulsaciones por minuto en reposo 

# 3 Variables dependientes
# Y1: Número de flexiones
# Y2: Número de sentadillas
# Y3: Número de saltos


# analisis exploratorio de los datos
colMeans(rendimiento)
apply(rendimiento,2,var)

# usemos cc para calcular la correlación canónica
help(cc)


X <- rendimiento[,c(2:4)]
Y <- rendimiento[,c(5:7)]

correl <- matcor(X, Y)
correl
img.matcor(correl, type = 2)

help("cc")


# escalemos los datos
X_e <- scale(X)
X
X_e
cov(X_e)
cor(X)

Y_e <- scale(Y)
cov(Y_e)
cor(Y)

# con los datos escalados podemos realizar el análisis de correlación 
#canónica

corr_can <- cc(X_e, Y_e)
corr_can$cor

# veamos los coeficients de las proyecciones en las variables canónicas
corr_can$xcoef
corr_can$ycoef

t(corr_can$xcoef[,3])%*%cov(X_e)%*%corr_can$xcoef[,3]

#U1 <-0.77*Peso - 1.58*Cintura + 0.06*Pulso
#V1 <-0.35*Flexiones + 1.05*Sentadillas - 0.72*Saltos

# proyecciones en las variables canónicas
corr_can$scores$xscores # Proyecciones de los X
corr_can$scores$yscores # Proyecciones de los Y

par(mfrow=c(1,3))
plot(corr_can$scores$xscores[,1],
     corr_can$scores$yscores[,1], 
     pch=19, col="firebrick",
     main = "Primeras variables canónicas")

plot(corr_can$scores$xscores[,2],
     corr_can$scores$yscores[,2], 
     pch=19, col="firebrick",
     main = "Segundas variables canónicas")

plot(corr_can$scores$xscores[,3],
     corr_can$scores$yscores[,3], 
     pch=19, col="firebrick",
     main = "Terceras variables canónicas")
par(mfrow=c(1,1))

cor(corr_can$scores$xscores[,1],
     corr_can$scores$yscores[,1])

cor(corr_can$scores$xscores[,2],
    corr_can$scores$yscores[,2])

cor(corr_can$scores$xscores[,3],
    corr_can$scores$yscores[,3])

corr_can$cor







