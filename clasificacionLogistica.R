hongos_clasificados <- read.csv("hongos_clasificados.txt", sep="")
View(hongos_clasificados)
plot(hongos_clasificados)

# debemos considerar la variable respuesta como 1 o 0
hongos_clasificados$Variety <- hongos_clasificados$Variety-1

plot(hongos_clasificados$Height, hongos_clasificados$Variety)

# hagamos un ajuste lineal de los datos
ajuste <- lm(Variety ~ Height, data=hongos_clasificados)

summary(ajuste)

plot(hongos_clasificados$Height, hongos_clasificados$Variety)

abline(ajuste)


# Función sigmoidea

curve(1/(1+exp(-x)), from=-10, to =10, lwd=2)

#En R el ajuste de la regresión logistica se puede hacer con la función
# glm indicando la función de enlace de la familia binomial.

# ajuste regresión logistica
ajuste_logistica <- glm(Variety ~ Height, data=hongos_clasificados,
                        family = "binomial")

summary(ajuste_logistica)
beta0 <- as.numeric(coef(ajuste_logistica)[1])
beta1 <- as.numeric(coef(ajuste_logistica)[2])

plot(hongos_clasificados$Height, hongos_clasificados$Variety)
curve(1/(1+exp(-(beta0+beta1*x))), add = T, col="red", lwd=2)

# una forma de entrenar un modelo, es dividir la data en dos conjuntos
# Entrenamiento: Conjunto de datos para ajustar el modelo.(80%) 
# Testeo: Conjunto de datos para validar el ajuste del modelo.(20%)

# dividamos los datos originales en entrenamiento y testeo
indices <- sample(1:500, 400)

entrenamiento_hongos <- hongos_clasificados[indices,]

testeo_hongos<-hongos_clasificados[-indices,]

# ajustemos el modelo con la data de entrenamiento
ajuste_entrenamiento <- glm(Variety ~ Height, data=entrenamiento_hongos,
                            family = "binomial")
plot(entrenamiento_hongos$Height, entrenamiento_hongos$Variety)
beta0 <- as.numeric(coef(ajuste_entrenamiento)[1])
beta1 <- as.numeric(coef(ajuste_entrenamiento)[2])
curve(1/(1+exp(-(beta0+beta1*x))), add = T, col="red", lwd=2)

# con R podemos hacer predicciones del modelo
predict.glm(ajuste_entrenamiento,
            newdata = data.frame(Height=8.5), type = "response")

# hagamos las predicciones de todos los datos de testeo

clasificacion_testeo <-ifelse(predict.glm(ajuste_entrenamiento,
            newdata = data.frame(Height=testeo_hongos$Height),
            type = "response")<0.5,0,1)

table(clasificacion_testeo, testeo_hongos$Variety)


































