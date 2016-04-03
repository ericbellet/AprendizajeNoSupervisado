setwd("C:/Users/Eric/Desktop/AprendizajeNoSupervisado")
source("src/funciones.R")
name = "a.csv"
#Lectura de datos.
df = read.csv(file = "C:/Users/Eric/Desktop/AprendizajeNoSupervisado/data/a.csv", header = F)
#Modificamos el nombre de las columnas por comodidad.
colnames(df) <- c("x","y","class")
#Coloco las clases del 1:n
df$class = as.numeric(df$class)
if (min(df$class) == 0){
  df$class <- df$class + 1
}
#****************************************************************************************
                            #Analisis exploratorio del dataset
#****************************************************************************************
#Podemos observar que hay 3 columnas.
head(df)

#Observamos cuantos elementos hay de cada clase.
table(df$class)
#1     2    3 
#1000  1000 1000

#Grafico 
plot(df$x, df$y, xlab = "x", ylab = "y", main = name)
#Podemos observar 3 conglomerados

length(unique(df$class))
#Existen 3 clases.
#****************************************************************************************
                                       #K-MEDIAS
#****************************************************************************************
#Aplicamos k=3 ya que identificamos 3 conglomerados y existen 3 clases.
#kmedias(Dataframe, Columnas, K, name)
clusters <- kmedias(df, 1:2, 3, name)

#Generamos la matriz de confusion
MatrixConfusionK <- matrizconfusion(df$class,clusters)
MatrixConfusionK
#****************************************************************************************
                                  #CLUSTER JERARQUICOS
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
#Dado un numero de clusters k determinar la altura requerida 
#para que tengamos el numero de cluster k.
clustersD <- clusterJD(df, distancia, 1:2, "complete", 3, name)
#Generamos la matriz de confusion
MatrixConfusionCJDC <- matrizconfusion(df$class, clustersD)
MatrixConfusionCJDC
#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el numero de clusters que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "complete", 30, name)
#Generamos la matriz de confusion
MatrixConfusionCJHC <- matrizconfusion(df$class, clustersH)
MatrixConfusionCJHC
#--------------------------------------------------------------------------------------
                                    #METHOD SINGLE
#--------------------------------------------------------------------------------------
#Dado un numero de clusters k determinar la altura requerida 
#para que tengamos el numero de cluster k.
clustersD <- clusterJD(df, distancia, 1:2, "single", 3, name)
#Generamos la matriz de confusion
MatrixConfusionCJDS <- matrizconfusion(df$class, clustersD)
MatrixConfusionCJDS
#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el numero de clusters que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "single", 30)
#Generamos la matriz de confusion
MatrixConfusionCJHS <- matrizconfusion(df$class, clustersH)
MatrixConfusionCJHS
#--------------------------------------------------------------------------------------
                                    #METHOD AVERAGE
#--------------------------------------------------------------------------------------
#Dado un numero de clusters k determinar la altura requerida  
#para que tengamos el numero de cluster k.
clustersD <- clusterJD(df, distancia, 1:2, "average", 3, name)
#Generamos la matriz de confusion
MatrixConfusionCJDA <- matrizconfusion(df$class, clustersD)
MatrixConfusionCJDA
#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el numero de clusters que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "average", 15, name)
#Generamos la matriz de confusion
MatrixConfusionCJHA <- matrizconfusion(df$class, clustersH)
MatrixConfusionCJHA
#--------------------------------------------------------------------------------------
                                    #METHOD ward.D
#--------------------------------------------------------------------------------------
#Dado un numero de clusters k determinar la altura requerida 
#para que tengamos el numero de cluster k.
clustersD <- clusterJD(df, distancia, 1:2, "ward.D", 3, name)
#Generamos la matriz de confusion
MatrixConfusionCJDW <- matrizconfusion(df$class, clustersD)
MatrixConfusionCJDW
#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el numero de clusters que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "ward.D", 10000, name)
#Generamos la matriz de confusion
MatrixConfusionCJHW <- matrizconfusion(df$class, clustersH)
MatrixConfusionCJHW