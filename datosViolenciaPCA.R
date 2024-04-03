library(factoextra)
library(corrplot)
#El set de datos USArrests del paquete básico de R contiene el porcentaje
#de asaltos (Assault), asesinatos (Murder) y violaciones (Rape) 
# por cada 100,000 habitantes para cada uno de los 50 estados de USA (1973).
#Además, también incluye el porcentaje de la población de cada estado 
#que vive en zonas rurales(UrbanPoP).

datos <- USArrests

stars(datos, labels = abbreviate(rownames( datos ),4), 
       nrow = 8, key.loc = c( 18, 19 ), full = TRUE )

corrplot(cor(datos), order = "hclust")

apply(datos,2,mean)
apply(datos,2,var)


analisi1<- prcomp(datos)
analisi1$rotation
analisi1$x
analisi1$scale 
analisi1$center
analisi1$sdev

# hagamos un escalod en los datos
analisis2 <- prcomp(datos, scale. = TRUE)
analisis2$rotation

# realizar el analisis de componentes principales con los datos escalados
# es equivalente a realizar la descomposición en autovalores y atovectores
# de la matriz de correlacion.

R <- cor(datos)
R
eigen(R)$vectors


# exploremos los resultados
analisis2$rotation

analisis2$sdev^2 # estos son los autovalores


analisis2$sdev^2/sum(analisis2$sdev^2)*100
cumsum(analisis2$sdev^2/sum(analisis2$sdev^2)*100)



eig.val <- get_eigenvalue(analisis2)
eig.val

#GRAFICOS
fviz_eig(analisis2,choice="variance")

# grafico de variables
fviz_pca_var(analisis2,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

#grafico de los individuos
fviz_pca_ind(analisis2,
             col.ind = "contrib", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
             )





