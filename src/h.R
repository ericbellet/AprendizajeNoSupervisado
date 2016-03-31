setwd("C:/Users/Eric/Desktop/AprendizajeNoSupervisado")
source("src/funciones.R")
library(rgl)
#Lectura de datos.
df = read.csv(file = "C:/Users/Eric/Desktop/AprendizajeNoSupervisado/data/h.csv")
#Modificamos el nombre de las columnas por comodidad.
colnames(df) <- c("x","y","z","class")
#df <- df[ order(df$class), ]

#*********************REGLA PARA ASIGNAR CLASES***************************
for (x in 1:nrow(df)) {
  
  df$class[x] <- reglas(df$class[x])
}

reglas <- function(x){
  if (x < 4.99735){
    return(0)
  }else if (x < 5.989202){
    return(1)
  }else if (x < 6.994868){
    return(2)
  }else if (x < 7.997107){
    return(3)
  }else if (x < 8.992769){
    return(4)
  }else if (x < 9.987271){
    return(5)
  }else if (x < 10.99652){
    return(6)
  }else if (x < 11.99878){
    return(7)
  }else if (x < 12.99898){
    return(8)
  }else if (x < 13.98096){
    return(9)
  }else{
    return(10)
  }
}

#****************************************************************************************
                      #Analisis exploratorio del dataset
#****************************************************************************************
#Podemos observar que hay 4 columnas.
head(df)

#Observamos cuantos elementos hay de cada clase.
table(df$class)
#1   2   3   4   5   6   7   8   9  10  11 
#29 110 104 115 116  97  98 103  95 118  14

#Grafico 
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = df$class)
#Podemos observar 11 conglomerados

length(unique(df$class))
#Existen 11 clases.

#****************************************************************************************
                                    #K-MEDIAS
#****************************************************************************************
#Aplicamos k=11 ya que identificamos 11 conglomerados.
#kmedias(Dataframe, Columnas,K)
clusters <- kmedias(df, 1:3, 11)

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
#METHOD COMPLETE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:3, "complete", 11)
#Generamos la matriz de confusion
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
MatrixConfusionCJDC <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "complete", 18)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHC <- matrizconfusion(df$class, clustersH)

#--------------------------------------------------------------------------------------
#METHOD SINGLE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:3, "single", 11)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
#Generamos la matriz de confusion
MatrixConfusionCJDS <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "single", 2.145)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHS <- matrizconfusion(df$class, clustersH)

#--------------------------------------------------------------------------------------
#METHOD AVERAGE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:3, "average", 11)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
#Generamos la matriz de confusion
MatrixConfusionCJDA <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "average", 10.2)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHA <- matrizconfusion(df$class, clustersH)

#--------------------------------------------------------------------------------------
#METHOD ward.D
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:3, "ward.D", 11)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
#Generamos la matriz de confusion
MatrixConfusionCJDW <- matrizconfusion(df$class, clustersD)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "ward.D", 280)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHW <- matrizconfusion(df$class, clustersH)