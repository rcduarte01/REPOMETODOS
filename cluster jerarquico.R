library(factoextra)


data <- cbind(c(2,4,0,3),
              c(3.5,3,6,3),
              c(0,1.5,4,1),
              c(4,5,2,4),
              c(7,6,3,77))

dist(data) # distancia euclidea por defecto
dist(data, method = "manhattan")# distancia manhattan
help("dist")


datos <- c(7, 10, 20, 28, 35)

# calculamos la matriz de disimilaridad
# usando la distancia euclidea
dist_euc <- dist(datos)
dist_euc
class(dist_euc)

help("hclust")

clust_sing <- hclust(d=dist_euc, method = "single")

plot(clust_sing)


# que pasa si cambiamos el método
clust_comp <- hclust(d=dist_euc, method = "complete")
plot(clust_comp, hang = -1)


par(mfrow=c(1,2))
plot(clust_sing, hang = -1,cex=0.8)
plot(clust_comp, hang = -1)
par(mfrow=c(1,1))


# puedo hacer el corte para encontrar
# k clusters
cutree(clust_sing, k = 3)

# data arrest
data("USArrests")
help("USArrests")
head(USArrests)
datos <- USArrests

distEuc <- dist(x = datos, method = "euclidean")
round(as.matrix(distEuc)[1:5, 1:5], 2)

fviz_dist(dist.obj = distEuc, lab_size = 5) 

hc <- hclust(distEuc, method="ave")
plot(hc)
plot(hc, hang = -1,cex=0.8)

groupos10 <- cutree(hc, k=10) 
groupos10

datos <- cbind(groupos10,datos)

groupos5 <- cutree(hc, k=5) 
groupos5

datos <- cbind(groupos5,datos)



g1 <- fviz_dend(x = hc, k = 5, cex = 0.6) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Cluster jerarquico",
       subtitle = "Distancia euclídea, Average Linkage, K=5")





hc_single <- hclust(distEuc, method="single")

g2 <- fviz_dend(x = hc_single, k = 5, cex = 0.6) +
  geom_hline(yintercept = 5.5, linetype = "dashed") +
  labs(title = "Cluster jerarquico",
       subtitle = "Distancia euclídea, Single Linkage, K=5")


library(gridExtra)

grid.arrange(g1,g2, ncol=2 )






