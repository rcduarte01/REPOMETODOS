# correlación canonica 

SigmaXX <- matrix(c(1,0.4,0.4,1), ncol = 2, byrow = T)
SigmaXX

SigmaYY <- matrix(c(1,0.2,0.2,1), ncol = 2, byrow = T)
SigmaYY

SigmaXY<- matrix(c(0.5,0.6,0.3,0.4), byrow = T, ncol = 2)
SigmaXY
SigmaYX <- t(SigmaXY)
SigmaYX

# encontremos las correlaciones canonicas (autovalores)
eigen(solve(SigmaXX)%*%SigmaXY%*%solve(SigmaYY)%*%SigmaYX)$values

eigen(solve(SigmaYY)%*%SigmaYX%*%solve(SigmaXX)%*%SigmaXY)$values

# la primera correlación canonica es
sqrt(eigen(solve(SigmaXX)%*%SigmaXY%*%solve(SigmaYY)%*%SigmaYX)$values[1])

# para determinar los coeficientes de las variables canonicas debemos
# encontrar los autovectores
# primeras correlaciones canonicas
alpha <- eigen(solve(SigmaXX)%*%SigmaXY%*%solve(SigmaYY)%*%SigmaYX)$vectors[,1]

# este vector hay que normalizarlo
t(alpha)%*%SigmaXX%*%alpha

alpha_N <- alpha/sqrt(t(alpha)%*%SigmaXX%*%alpha)

t(alpha_N)%*%SigmaXX%*%alpha_N

alpha_N

########

gamma <- eigen(solve(SigmaYY)%*%SigmaYX%*%solve(SigmaXX)%*%SigmaXY)$vectors[,1]

# este vector hay que normalizarlo
t(gamma)%*%SigmaYY%*%gamma

gamma_N <- gamma/sqrt(t(gamma)%*%SigmaYY%*%gamma)

t(gamma_N)%*%SigmaYY%*%gamma_N 

gamma_N 

solve(SigmaXX)%*%SigmaXY%*%gamma_N/sqrt(eigen(solve(SigmaXX)%*%SigmaXY%*%solve(SigmaYY)%*%SigmaYX)$values[1])














