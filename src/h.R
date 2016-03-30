
library(rgl)
#Lectura de datos.
df = read.csv(file = "C:/Users/Eric/Desktop/AprendizajeNoSupervisado/data/h.csv")
#Modificamos el nombre de las columnas por comodidad.
colnames(df) <- c("x","y","z","class")

plot3d(df$x, df$y, df$z, type = "s",size = 2, col = df$class)


#****************************************************************************************
#Analisis exploratorio del dataset
#****************************************************************************************
#Podemos observar que hay 3 columnas.
head(df)

#Observamos cuantos elementos hay de cada clase.
table(df$class)
#0    1    2 
#1000  999 1000

#Grafico 
plot(df)
df$class <- NULL
scatter3D(df)
#Podemos observar 3 conglomerados

length(unique(df$class))
#Existen 3 clases.
#****************************************************************************************
#K-MEDIAS
#****************************************************************************************
#Aplicamos k=3 ya que identificamos 3 conglomerados y existen 3 clases.
modelo.kmedias = kmeans(x = df[, c("x", "y")], centers = 3)

#GRAFICAMOS LOS CLUSTERS
plot(x = df$x, y = df$y, col = modelo.kmedias$cluster)

# Ahora graficamos los centroides 
points(x = modelo.kmedias$centers[, c("x", "y")], col = 1:4, pch = 19, cex = 3)

#Generamos la matriz de confusion
matrizconfusion <- table(df$class,modelo.kmedias$cluster,dnn=c("Clase", "Cluster"))

#****************************************************************************************
#Cluster Jerárquicos
#****************************************************************************************
#Copy del dataset
datos = df
#Elimino la columna clase para realizar aprendizaje no supervisado.
datos$class <- NULL
#Convierto el dataframe en una matriz
datos= as.matrix(datos)
#Calculamos la matriz de distancia
distancia = dist(datos)

clusterJ <- function(method, k, h){
  #Aplicamos cluster jerarquico utilizando el metodo complete
  cluster = hclust(distancia, method = method)
  #Graficamos el dendogram
  plot(cluster)
  
  ############################################################
  #Determinar la altura requerida dado un numero de clusters k
  ############################################################
  #Cortamos el dendograma con 3 clases
  corte = cutree(cluster, k = k)
  #Observamos la cantidad de clusters
  unique(corte)
  #Graficamos los clusters
  plot(x = df$x, y = df$y, col = corte)
  
  ############################################################
  #Dada una altura h (una medida de disimilaridad) determinar 
  #el número de clústers que se obtienen
  ############################################################
  # Cortamos por altura
  corte = cutree(cluster, h = h)
  #Observamos la cantidad de clusters
  unique(corte)
  #Graficamos los clusters
  plot(x = df$x, y = df$y, col = corte)
}
#--------------------------------------------------------------------------------------
#METHOD COMPLETE
#--------------------------------------------------------------------------------------
clusterJ("complete",3,30)
#--------------------------------------------------------------------------------------
#METHOD SINGLE
#--------------------------------------------------------------------------------------
clusterJ("single",3,30)
#--------------------------------------------------------------------------------------
#METHOD AVERAGE
#--------------------------------------------------------------------------------------
clusterJ("average",3,15)
#--------------------------------------------------------------------------------------
#METHOD ward.D
#--------------------------------------------------------------------------------------
clusterJ("ward.D",3,10000)