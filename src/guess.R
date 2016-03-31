setwd("C:/Users/Eric/Desktop/AprendizajeNoSupervisado")
source("src/funciones.R")
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
#kmedias(Dataframe, Columnas,K)
clusters <- kmedias(df, 1:2, 2)

#****************************************************************************************
                                    #CLUSTER JERARQUICO
#****************************************************************************************
#Copy del dataset
datos = df
#Elimino la columna clase para realizar aprendizaje no supervisado.
datos$class <- NULL
#Convierto el dataframe en una matriz
datos= as.matrix(datos)
#Calculamos la matriz de distancia
distancia = dist(datos)

#--------------------------------------------------------------------------------------
                                    #METHOD COMPLETE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:2, "complete", 2)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "complete", 100)

#--------------------------------------------------------------------------------------
                                  #METHOD SINGLE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:2, "single", 2)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "single", 10)

#--------------------------------------------------------------------------------------
                                  #METHOD AVERAGE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:2, "average", 2)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "average", 50)

#--------------------------------------------------------------------------------------
#METHOD ward.D
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:2, "ward.D", 2)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "ward.D", 30000)


