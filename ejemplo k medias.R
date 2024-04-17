Notas <- data.frame(Estudiante=c("Lucía","Pedro","Inés",
                                 "Luis","Andrés","Ana",
                                 "Carlos","José","Sonia","María"),
                    Matemática=c(7,7.5,7.6,5,6,7.8,6.3,7.9,6,6.8),
                    Ciencias =c(6.5,9.4,9.2,6.5,6,9.6,6.4,9.7,6,7.2),
                    Español=c(9.2,7.3,8, 6.5, 7.8, 7.7, 8.2, 7.5, 6.5, 8.7),
                    Historia =c(8.6, 7, 8, 7, 8.9, 8,9, 8, 5.5, 9),
                    Ed.Física=c(8, 7, 7.5, 9.0, 7.3, 6.5, 7.2, 6, 8.7,7))
####
data <- Notas[,2:6]
kmeans(x = data, centers = 3)

set.seed(2024)
cluster_kmedias <- kmeans(x = data, centers = 3)

help("kmeans")
cluster_kmedias$withinss
cluster_kmedias$tot.withinss

cluster_kmedias$size
cluster_kmedias$iter


library(factoextra)
fviz_cluster(object = cluster_kmedias, data = data,
             show.clust.cent = TRUE, ellipse.type = "euclid",
             star.plot = TRUE, repel = TRUE) +
  labs(title = "Resultados clustering K-means") +
  theme_bw() +  theme(legend.position = "none")

#cuantos clusters
#wss es Elowb
fviz_nbclust(x = data, FUNcluster = kmeans, method = "wss", k.max = 9, 
             diss = get_dist(data, method = "euclidean"), nstart = 25)+
  labs(title = "Número óptimo de clusters")



