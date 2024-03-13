# considremos los datos sobre la nieve en Buffalo 
bufalo<-scan()
126.4  82.4  78.1  51.1  90.9  76.2  104.5 87.4 110.5 25.0  69.3  53.5
39.8   63.6  46.7  72.9  79.6  83.6  80.7  60.3 79.0  74.4  49.6  54.7
71.8   49.1  103.9 51.6  82.4  83.6  77.8  79.3 89.6  85.5  58.0  120.7
110.5  65.4  39.9  40.1  88.7  71.4  83.0  55.9 89.9  84.8  105.2 113.7
124.7  114.5 115.6 102.4 101.4 89.8  71.5  70.9 98.3  55.5  66.1  78.4
120.5  97.0  110.0



range(bufalo)
# creamos una malla de valores tomando como referencia el rango de bufalor
malla <- seq(25, 126.4, length=200 )



#programemos el estimador de la densidad
est.parzen <- function(muestra, h, x){
  # muestra: es la muestra aleatoria de la cual queremos ajustar
  # su función de densidad.
  
  #h: Es la ventana para la estimación
  #x: Malla de valores sobre las cuales queremos hacer la estimación
  # de la densidad.
  
  n <- length(x)
  estimacion <- c()
  for(i in 1: n){
    estimacion[i]<- mean(ifelse (abs((x[i]-muestra)/h)<=1, 1/2, 0))/h
  }
  
  return(estimacion)
  
}




# grafiquemos sobre el histograma la densidad estimada
fn <- est.parzen(muestra=bufalo, h=10, x=malla)
hist(bufalo, freq = F, ylim = c(0, 0.02))
lines(malla, fn, col="red", type="l")

# cambiemos el tamaño de la ventana

fn_h20 <- est.parzen(muestra=bufalo, h=20, x=malla)
fn_h30 <- est.parzen(muestra=bufalo, h=30, x=malla)
hist(bufalo, freq = F, ylim = c(0, 0.02))
lines(malla, fn, col="red", type="l")
lines(malla, fn_h20, col="blue", type="l")
lines(malla, fn_h30, col="darkgreen", type="l")

# R tiene la función density que hace una estimación de la densidad
# usando Kernells

estimacion_densidad_gauss <- density(bufalo, kernel = "gaussian", bw=10)

hist(bufalo, freq = F, ylim = c(0, 0.02))
lines(estimacion_densidad_gauss)


# consideremos diferentes valores de ventana
estimacion_densidad_gauss_2 <- density(bufalo, kernel = "gaussian", bw=2)
estimacion_densidad_gauss_5 <- density(bufalo, kernel = "gaussian", bw=5)
estimacion_densidad_gauss_10 <- density(bufalo, kernel = "gaussian", bw=10)
estimacion_densidad_gauss_20 <- density(bufalo, kernel = "gaussian", bw=20)



hist(bufalo, freq = F, ylim = c(0, 0.025))
lines(estimacion_densidad_gauss_2, col="red")
lines(estimacion_densidad_gauss_5, col="blue")
lines(estimacion_densidad_gauss_10, col="darkgreen")
lines(estimacion_densidad_gauss_20, col="violet")

# como estimar la densidad en un punto x0=50

density(bufalo,from=50, to= 50, n=1, 
        kernel = "gaussian", bw=10)$y


# probemos los otro núcleos
estimacion_densidad_triangular_10 <- density(bufalo, kernel = "triangular", bw=10)
estimacion_densidad_gaussiano_10 <- density(bufalo, kernel = "gaussian", bw=10)
estimacion_densidad_epanechnikov_10 <- density(bufalo, kernel = "epanechnikov", bw=10)
estimacion_densidad_biweight_10 <- density(bufalo, kernel = "biweight", bw=10)

hist(bufalo, freq = F, ylim = c(0, 0.025))
lines(estimacion_densidad_triangular_10, col="red")
lines(estimacion_densidad_gaussiano_10, col="blue")
lines(estimacion_densidad_epanechnikov_10, col="darkgreen")
lines(estimacion_densidad_biweight_10, col="darkviolet")





