setwd("C:/Users/Eric/Desktop/AprendizajeNoSupervisado")
source("src/funciones.R")
library(rgl)

#Lectura de datos.
df = read.csv(file = "C:/Users/Eric/Desktop/AprendizajeNoSupervisado/data/help.csv")
#Modificamos el nombre de las columnas por comodidad.
colnames(df) <- c("x","y","z","class")
df <- df[ order(df$class), ]

  #. Cuántos clústers ve en el dataset help ? 
#3

#. Qué pasa al aplicar la regla de asignación de clases en este dataset? 
#Colocar en la misma clase las 2 S.

#. Qué solución daría para asignar de manera correcta los valores de las clases y 
#pueda analizar el desempeño del algoritmo de clustering de manera correcta? 
#Generar las reglas por partes.

reglas <- function(x,i){
  
  if ((i %% 2 == 0) == TRUE){
    return(1)
  }else{
    return(2)
  }
}

reglas2 <- function(x,i){
  
  if ((i %% 2 == 0) == TRUE){
    return(2)
  }else{
    return(1)
  }
}

for (x in 1:204) {
  
  df$class[x] <- reglas(df$class[x],x)
}

for (x in 205:1999) {
  
  df$class[x] <- reglas2(df$class[x],x)
}

df$class[2000:2999] <- 3

plot3d(df$x, df$y, df$z, type = "s",size = 2, col = df$class)

df$class[df$class == 1] <- 0
df$class[df$class == 2] <- 1
df$class[df$class == 3] <- 2

#Observamos cuantos elementos hay de cada clase.
table(df$class)
#0    1    2 
#1000  999 1000

length(unique(df$class))
#Existen 3 clases.
#****************************************************************************************
                                      #K-MEDIAS
#****************************************************************************************
#Aplicamos k=3 ya que identificamos 3 conglomerados.
#kmedias(Dataframe, Columnas,K)
clusters <- kmedias(df, 1:3, 3)

plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clusters)
#Generamos la matriz de confusion
MatrixConfusionK <- matrizconfusion(df$class,clusters)


#****************************************************************************************
                              #Cluster Jerarquicos
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
#                               METHOD COMPLETE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:3, "complete", 3)
#Generamos la matriz de confusion
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
MatrixConfusionCJDC <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "complete", 50)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHC <- matrizconfusion(df$class, clustersH)

#--------------------------------------------------------------------------------------
#METHOD SINGLE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:3, "single", 3)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
#Generamos la matriz de confusion
MatrixConfusionCJDS <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "single", 5)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHS <- matrizconfusion(df$class, clustersH)

#--------------------------------------------------------------------------------------
#METHOD AVERAGE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:3, "average", 3)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
#Generamos la matriz de confusion
MatrixConfusionCJDA <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "average", 30)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHA <- matrizconfusion(df$class, clustersH)

#--------------------------------------------------------------------------------------
#METHOD ward.D
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:3, "ward.D", 3)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
#Generamos la matriz de confusion
MatrixConfusionCJDW <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "ward.D", 10000)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHW <- matrizconfusion(df$class, clustersH)