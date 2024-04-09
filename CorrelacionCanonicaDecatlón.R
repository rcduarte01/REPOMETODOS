library(ade4)
library(CCA)
data("olympic")
# 100 metros (100), salto largo (largo), tiro (poid), salto alto (alto), 400 metros (400),
#obstáculos de 110 metros (110 ), lanzamiento de disco (disq), salto con p?rtiga (perc),
#jabalina (jave) y 1500 metros (1500)

#Vamos a separar en dos conjuntos de datos
#las actividades que tienen que ver con los brazos
#y las que tiene que ver con las piernas
#X: tiro(poid), disco (disq), javelina (jave), (perc) pole vault (variables brazos)
#Y: 100, salto en largo (long), salto en alto (haut), 400, obstaculos (110),1500 (piernas)

olympic$tab[,c(1,5,6,10)]<- -olympic$tab[,c(1,5,6,10)]

X<- olympic$tab[,c( "poid",  "disq" ,"perc", "jave")]
Y<-olympic$tab[,c("100",  "long","haut", "400" , "110",   "1500")]

m <- 4

p <- 6

r <- min(m,p)
r


correl <- matcor(X, Y)
correl
img.matcor(correl, type = 2)

# analisis de correlación canónica
# calcular las matrices de varianza y covarianza de los X y Y
SigmaXX <- cov(X)
SigmaXX

SigmaYY <- cov(Y)
SigmaYY

SigmaXY<- cov(X,Y)
SigmaXY

SigmaYX <- t(SigmaXY)


# hagamos la descomposición espectral
desc1<-eigen(solve(SigmaXX)%*%SigmaXY%*%solve(SigmaYY)%*%SigmaYX)

desc2 <- eigen(solve(SigmaYY)%*%SigmaYX%*%solve(SigmaXX)%*%SigmaXY)

# autovalores
desc1$values
desc2$values

# la máxima correlación canonica es
sqrt(desc1$values[1])

# encontremos los coeficientes a y b
a <- desc1$vectors
a

# primeras variables canonicas
t(a[,1])%*%SigmaXX%*%a[,1]

a_N <- a[,1]/(as.numeric(sqrt(t(a[,1])%*%SigmaXX%*%a[,1])))
a_N

t(a_N)%*%SigmaXX%*%a_N

#U1 <- 0.71066443*X1 - 0.18313809X2 + 2.19793095X3 - 0.05174114X4


# encontremos ahora las proyecciones de V
b <- desc2$vectors
b

b

t(b[,1])%*%SigmaYY%*%b[,1]

b_N <- b[,1]/(as.numeric(sqrt(t(b[,1])%*%SigmaYY%*%b[,1])))
b_N

t(b_N)%*%SigmaYY%*%b_N

# V1 = 1.031778575Y1  + 0.292719536Y2 - 0.981536040Y3 - 0.237463076Y4
# + 1.834411200Y5 +   0.003311523Y6


solve(SigmaXX)%*%SigmaXY%*%b_N/sqrt(desc1$values[1])
a_N


# calculemos las variables canónicas, para esto debemos centrar los datos
xbar <- matrix(colMeans(X), ncol = 4, nrow = 33, byrow = T)
Xc <- X-xbar



ybar <- matrix(colMeans(Y), ncol = 6, nrow = 33, byrow = T)
ybar
Yc <- Y-ybar


# proyectemos en las variables canonicas
U1 <- as.matrix(Xc)%*%as.matrix(a_N)
U1

V1 <- as.matrix(Yc)%*%as.matrix(b_N)
V1


cor(U1,V1)

plot(U1,V1)


help(cc)

correlacionCanonica <- cc(X,Y)

correlacionCanonica$scores
correlacionCanonica$ycoef
correlacionCanonica$xcoef

correlacionCanonica$cor
sqrt(desc1$values)











