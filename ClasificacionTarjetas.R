library(ISLR2)
datos <- ISLR2::Default
help("Default")

# queremos hacer una clasificación de los usuarios basandonos primero
# solo en la información de la variable balance

# primero dividimos el archivo en dos conjuntos
# entrenamiento y testeo

datos$default <- ifelse(datos$default=="Yes",1,0)

set.seed(2024)
indices <- sample(1:10000,8000)
data_entrenamiento <- datos[indices,]

data_test <- datos[-indices,]

# hagamos un gráfico con la data de entrenamiento

plot(data_entrenamiento$income, data_entrenamiento$default)
plot(data_entrenamiento$balance, data_entrenamiento$default)


# ajustamos el modelo de regresión logistica
ajuste_logistica <- glm(default ~ balance, data=data_entrenamiento,
                        family = "binomial")

# ahora hagamos la predicción en los datos de test
prediccion<- predict(ajuste_logistica, 
                         newdata = data.frame(balance=data_test$balance),
                         type = "response")
clasificacion <- ifelse(prediccion<0.5, 0, 1)
clasificacion
table(clasificacion, data_test$default)

# hagamos un balance en las categorias, es decir, que ambas categorias
# tengan la misma proporción de datos

table(datos$default)

datos0 <- subset(datos,datos$default==0)
datos1 <- subset(datos,datos$default==1)

# extraigamos una muestra de 333 de los de la categoría 0
set.seed(2024)
indices <- sample(1:9667, 333)

datos0_muestra <- datos0[indices,]

data_balanceada <- rbind(datos0_muestra, datos1)

table(data_balanceada $default)

# dividamos la data en entrenamiento y test
set.seed(2024)
indices <- sample(1:666,500)


dataBalanceada_entrenamiento <-data_balanceada[indices,]
dataBalanceada_test <- data_balanceada[-indices,]

plot(default~balance, data=dataBalanceada_entrenamiento)

# ajustamos el modelo
ajusteBalanceado <- glm(default~balance, data=dataBalanceada_entrenamiento,
                        family="binomial")

prediccion <- predict.glm(ajusteBalanceado,
                          newdata = data.frame(balance=dataBalanceada_test$balance),
                          type = "response")

clasificacion <- ifelse(prediccion<0.5, 0,1)

table(clasificacion,dataBalanceada_test$default)


# para hacer la elección del umbral hagamos un mallado de valores de umbral
# y hagamos la clasificación para cada valor del umbral, luego calculamos
# una metrica.



accuracy <- function(prediccion, real){
  n <- length(prediccion)
  sum(diag(table(prediccion, real)))/n
}

accuracy(clasificacion, dataBalanceada_test$default)


ajusteBalanceado <- glm(default~balance, data=dataBalanceada_entrenamiento,
                        family="binomial")
prediccion <- predict.glm(ajusteBalanceado,
                          newdata = data.frame(balance=dataBalanceada_test$balance),
                          type = "response")

malla_umbral <- seq(0.01, 0.9, length=100)
vector_accuracy <- c()
for(i in 1: length(malla_umbral)){
  clasificacion <- ifelse(prediccion < malla_umbral[i], 0,1)
  vector_accuracy[i] <- accuracy(clasificacion, dataBalanceada_test$default)
}

plot(malla_umbral, vector_accuracy, type = "l")

malla_umbral[which.max(vector_accuracy)]
































