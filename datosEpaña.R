# El archivo demograficos.csv contiene datos censales de españa (Baillo 2008)
# x1: Población total (miles)
# x2: Inmigrantes extranjeros de los últimos 5 años (decenas).
# x3: Número promedio de hijos por pareja.
# x4: porcentaje de desocupados
# x5: porcentaje de hogares ocupados por una sola persona.

datos <- read.csv("demograficos.csv", sep=";")
n <- 14
p <- 5

# calculemos el vector de medias muestrales
Xbar <- colMeans(datos[,2:6])
Xbar

matriz_medias <- matrix(rep(Xbar,c(n,n,n,n,n)), byrow = F, ncol = p)
matriz_medias

data_c <- datos[,2:6]-matriz_medias
data_c 
colMeans(data_c)


S <- cov(data_c)
S

autovalores <- eigen(S)$values
autovectores <- eigen(S)$vectors
autovalores
autovectores

# calculamos la variabilidad explicada por cada componente
cumsum(autovalores)/sum(autovalores)*100





# proyectamos en las primeras dos componentes 
y1 <- t(autovectores[,1]%*%t(as.matrix(data_c)))
y2 <- t(autovectores[,2]%*%t(as.matrix(data_c)))
cor(y1,y2)


# prcomp
componetes <- prcomp(datos[,2:6])
componetes$rotation
autovectores

biplot(componetes)

