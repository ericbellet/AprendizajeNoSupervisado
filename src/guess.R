#Lectura de datos.
df = read.csv(file = "C:/Users/Eric/Desktop/AprendizajeNoSupervisado/data/guess.csv")
#Modificamos el nombre de las columnas por comodidad.
colnames(df) <- c("x","y")

#****************************************************************************************
#Analisis exploratorio del dataset
#****************************************************************************************
#Podemos observar que hay 2 columnas.
head(df)

#Grafico 
plot(df$x, df$y)
#Podemos observar 2 conglomerados

#Aplicamos codo de Jambu como ayuda para seleccionar el K
plot(df, pch = 19)
InerciaIC = rep(0, 30)
for (k in 1:30) {
  grupos = kmeans(df, k)
  InerciaIC[k] = grupos$tot.withinss
}
plot(InerciaIC, col = "blue", type = "b")
#Segun mi analisis el k adecuado es 2
#****************************************************************************************
#K-MEDIAS
#****************************************************************************************
#Aplicamos k=2 ya que identificamos 2 conglomerados.
modelo.kmedias = kmeans(x = df[, c("x", "y")], centers = 2)

#GRAFICAMOS LOS CLUSTERS
plot(x = df$x, y = df$y, col = modelo.kmedias$cluster)

# Ahora graficamos los centroides 
points(x = modelo.kmedias$centers[, c("x", "y")], col = 1:4, pch = 19, cex = 3)


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
  #Cortamos el dendograma con 2 clases
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
clusterJ("complete",2,100)
#--------------------------------------------------------------------------------------
#METHOD SINGLE
#--------------------------------------------------------------------------------------
clusterJ("single",2,10)
#--------------------------------------------------------------------------------------
#METHOD AVERAGE
#--------------------------------------------------------------------------------------
clusterJ("average",2,50)
#--------------------------------------------------------------------------------------
#METHOD ward.D
#--------------------------------------------------------------------------------------
clusterJ("ward.D",2,30000)