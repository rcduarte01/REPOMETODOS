#pata = 194, abdomen = 124,  organo_sexual = 49

#############################
# EJEMPLO 1: DATOS INSECTOS #
#############################
# Se quiere identificar a que especie (a o b) pertenece un determinado insecto.
# Para ello, se han medido tres variables (longitud de las patas, diámetro del abdomen 
#y diámetro del órgano sexual) en 10 individuos de cada una de las dos especies.

input <- ("
especie pata abdomen organo_sexual 
a 191 131 53
a 185 134 50
a 200 137 52
a 173 127 50
a 171 128 49
a 160 118 47
a 188 134 54
a 186 129 51
a 174 131 52
a 163 115 47
b 186 107 49
b 211 122 49
b 201 144 47
b 242 131 54
b 184 108 43
b 211 118 51
b 217 122 49
b 223 127 51
b 208 125 50
b 199 124 46
")

datos <- read.table(textConnection(input), header = TRUE)

especie <- as.factor(datos$especie)
datos$especie <- especie

plot(datos$abdomen, datos$pata, col=datos$especie, pch=19)
plot(datos$abdomen, datos$organo_sexual, col=datos$especie, pch=19)

plot(datos$pata, datos$organo_sexual, col=datos$especie, pch=19)


# programemos k vecinos mas cercanos para clasificar una nueva especie
# según su longitud de abdomen y longitud del organo sexual
# segun el color: Rosa: tipo b, Negro: tipo a

distancia <- function(x,x0){
  #x: arreglo cuyas filas son los puntos
  #x0: es la nueva observación a la cual queremos calcular su distancia
  sqrt((x[,1]-x0[1])^2 + ((x[,2]-x0[2])^2))
  
}


distancia(x= datos[,c(3:4,1)], x0=c(124,49))
sort(distancia(x= datos[,c(3:4,1)], x0=c(124,49)))
o <- order(distancia(x= datos[,c(3:4,1)], x0=c(124,49)))
order(sort(distancia(x= datos[,c(3:4,1)], x0=c(124,49))))

datos[o,]

k_vecinos <- function(k, x0, data){
  #k: Cuantos vecinos considerar
  # x0: nueva observación que queremos clasificar
  # data: contiene la matriz de los datos, mas la columna de la categoria al final.
  d <- distancia(data, x0)
  o <- order(d)
  tt <- table(data[o[1:k],3])
  print(tt)
  if(tt[1] < tt[2]){
    return(names(tt)[2])
  }else{
    return(names(tt)[1])
  }
}

k_vecinos(k=5, x0=c(110,54),data=datos[,c(3:4,1)])

# ahora quitemos un punto y hagamos la predicción con los puntos restantes


vector_prediccion <- c()
for(i in 1:20){
  vector_prediccion[i] <- k_vecinos(k=5,x0=as.numeric(datos[i,c(3:4)]),data=datos[-i,c(3:4,1)])
}

table(datos$especie,vector_prediccion)







