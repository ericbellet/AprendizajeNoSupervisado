setwd("C:/Users/Eric/Desktop/AprendizajeNoSupervisado")
source("src/funciones.R")

#Lectura de datos.
df = read.csv(file = "C:/Users/Eric/Desktop/AprendizajeNoSupervisado/data/moon.csv")
#Modificamos el nombre de las columnas por comodidad.
colnames(df) <- c("x","y","class")

#****************************************************************************************
#Analisis exploratorio del dataset
#****************************************************************************************
#Podemos observar que hay 3 columnas.
head(df)

#Observamos cuantos elementos hay de cada clase.
table(df$class)
#0   1 
#499 500 

#Grafico 
plot(df$x, df$y)
#Podemos observar 2 conglomerados

length(unique(df$class))
#Existen 2 clases.

#****************************************************************************************
#K-MEDIAS
#****************************************************************************************
#Aplicamos k=3 ya que identificamos 2 conglomerados y existen 2 clases.
#kmedias(Dataframe, Columnas,K)
clusters <- kmedias(df, 1:2, 2)

#Generamos la matriz de confusion
MatrixConfusionK <- matrizconfusion(df$class,clusters)

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

#--------------------------------------------------------------------------------------
#METHOD COMPLETE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:2, "complete", 2)
#Generamos la matriz de confusion
MatrixConfusionCJDC <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "complete", 3)
#Generamos la matriz de confusion
MatrixConfusionCJHC <- matrizconfusion(df$class, clustersH)
#--------------------------------------------------------------------------------------
#METHOD SINGLE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:2, "single", 2)
#Generamos la matriz de confusion
MatrixConfusionCJDS <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "single", 0.20)
#Generamos la matriz de confusion
MatrixConfusionCJHS <- matrizconfusion(df$class, clustersH)

#--------------------------------------------------------------------------------------
#METHOD AVERAGE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:2, "average", 2)
#Generamos la matriz de confusion
MatrixConfusionCJDA <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "average", 1.5)
#Generamos la matriz de confusion
MatrixConfusionCJHA <- matrizconfusion(df$class, clustersH)

#--------------------------------------------------------------------------------------
#METHOD ward.D
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:2, "ward.D", 2)
#Generamos la matriz de confusion
MatrixConfusionCJDW <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "ward.D", 300)
#Generamos la matriz de confusion
MatrixConfusionCJHW <- matrizconfusion(df$class, clustersH)
