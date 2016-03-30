library('FactoMineR')
#Lectura de datos.
df = read.csv(file = "C:/Users/Eric/Desktop/AprendizajeNoSupervisado/data/good_luck.csv")
#Modificamos el nombre de las columnas por comodidad.
colnames(df)[11] <- "class"

#****************************************************************************************
#Analisis exploratorio del dataset
#****************************************************************************************
#Podemos observar que hay 11 columnas.
head(df)

#Observamos cuantos elementos hay de cada clase.
table(df$class)
#0   1 
#513 486 


#Grafico 
plot(df)
PCA <- PCA(df)

plot(PCA)

length(unique(df$class))
#Existen 2 clases.
#****************************************************************************************
                                    #K-MEDIAS
#****************************************************************************************
#Aplicamos k=2 ya que  existen 2 clases.
modelo.kmedias = kmeans(x = df[1:10], centers = 2)

#GRAFICAMOS LOS CLUSTERS
plot(df, col = modelo.kmedias$cluster)

# Ahora graficamos los centroides 
points(x = modelo.kmedias$centers, col = 1:4, pch = 19, cex = 3)

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