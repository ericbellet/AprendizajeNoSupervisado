install = function(pkg)
{
  # Si ya está instalado, no lo instala.
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    if (!require(pkg, character.only = TRUE)) stop(paste("load failure:", pkg))
  }
}

#Instalo automaticamente los paquetes.
install('rgl')
install('FactoMineR')
install('pROC')
install('sampling')

#Cargo las librerias.
library(rgl)
library(FactoMineR)
library(pROC)
library(sampling)

setwd("C:/Users/Eric/Desktop/AprendizajeNoSupervisado")
source("src/funciones.R")

name = ""
#Lectura de datos.
df = read.csv(file = "C:/Users/Eric/Desktop/AprendizajeNoSupervisado/data/s.csv", header = F)
#Modificamos el nombre de las columnas por comodidad.
colnames(df) <- c("x","y","z","class")
#Ordenamos la columna clase
df <- df[ order(df$class), ]
#****************************************************************************************
#                   Analisis exploratorio del dataset
#****************************************************************************************
#Matriz de dispersion
plot(df)

#*********************REGLA PARA ASIGNAR CLASES***************************
reglas <- function(x){
  if (x < -2.230634){
    return(1)
  }else if (x < -0.010020){
    return(2)
  }else if (x < 2.500632){
    return(3)
  }else{
    return(4)
  }
}

for (x in 1:nrow(df)) {
  
  df$class[x] <- reglas(df$class[x])
}
#Observamos cuantos elementos hay de cada clase.
table(df$class)
#1   2   3   4 
#266 251 250 233 

palette(rainbow(length(unique(df$class))))
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = df$class)
#****************************************************************************************
#                               K-MEDIAS
#****************************************************************************************
#Aplicamos k=4 ya que identificamos 4 conglomerados.
#kmedias(Dataframe, Columnas,K)
modeloK <- kmedias(df, 1:3, 4, name)

plot3d(df$x, df$y, df$z, type = "s",size = 2, col = modeloK$cluster)
#Generamos la matriz de confusion
MatrixConfusionK <- matrizconfusion(df$class,modeloK$cluster)
MatrixConfusionK

#Calculamos la precision del modelo
PrecisionK <- precision(MatrixConfusionK)
PrecisionK

#Generamos la curva de ROC
modeloKROC <- roc(df$class, modeloK$cluster)
plot(modeloKROC,type="l",col="red")

#****************************************************************************************
#                                   Cluster Jerarquicos
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
clustersD <- clusterJD(df, distancia, 1:3, "complete", 4, name)
#Generamos la matriz de confusion
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
MatrixConfusionCJDC <- matrizconfusion(df$class, clustersD)
MatrixConfusionCJDC

#Calculamos la precision del modelo
PrecisionDC <- precision(MatrixConfusionCJDC)
PrecisionDC

#Generamos la curva de ROC
modeloDC <- roc(df$class, clustersD)
plot(modeloDC,type="l",col="red")
#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "complete", 2.8, name)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHC <- matrizconfusion(df$class, clustersH)
MatrixConfusionCJHC

#Calculamos la precision del modelo
PrecisionHC <- precision(MatrixConfusionCJHC)
PrecisionHC

#Generamos la curva de ROC
modeloHC <- roc(df$class, clustersH)
plot(modeloHC,type="l",col="red")
#--------------------------------------------------------------------------------------
#                           METHOD SINGLE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:3, "single", 4, name)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
#Generamos la matriz de confusion
MatrixConfusionCJDS <- matrizconfusion(df$class, clustersD)
MatrixConfusionCJDS

#Calculamos la precision del modelo
PrecisionDS <- precision(MatrixConfusionCJDS)
PrecisionDS

#Generamos la curva de ROC
modeloDS <- roc(df$class, clustersD)
plot(modeloDS,type="l",col="red")
#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "single", 0.21, name)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHS <- matrizconfusion(df$class, clustersH)
MatrixConfusionCJHS

#Calculamos la precision del modelo
PrecisionHS <- precision(MatrixConfusionCJHS)
PrecisionHS

#Generamos la curva de ROC
modeloHS <- roc(df$class, clustersH)
plot(modeloHS,type="l",col="red")
#--------------------------------------------------------------------------------------
#                           METHOD AVERAGE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:3, "average", 4, name)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
#Generamos la matriz de confusion
MatrixConfusionCJDA <- matrizconfusion(df$class, clustersD)
MatrixConfusionCJDA

#Calculamos la precision del modelo
PrecisionDA <- precision(MatrixConfusionCJDA)
PrecisionDA

#Generamos la curva de ROC
modeloDA <- roc(df$class, clustersD)
plot(modeloDA,type="l",col="red")
#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "average", 1.5, name)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHA <- matrizconfusion(df$class, clustersH)
MatrixConfusionCJHA

#Calculamos la precision del modelo
PrecisionHA <- precision(MatrixConfusionCJHA)
PrecisionHA

#Generamos la curva de ROC
modeloHA <- roc(df$class, clustersH)
plot(modeloHA,type="l",col="red")
#--------------------------------------------------------------------------------------
#                             METHOD ward.D
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:3, "ward.D", 4, name)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersD)
#Generamos la matriz de confusion
MatrixConfusionCJDW <- matrizconfusion(df$class, clustersD)
MatrixConfusionCJDW


#Calculamos la precision del modelo
PrecisionDW <- precision(MatrixConfusionCJDW)
PrecisionDW

#Generamos la curva de ROC
modeloDW <- roc(df$class, clustersD)
plot(modeloDW,type="l",col="red")
#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:3, "ward.D", 140, name)
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = clustersH)
#Generamos la matriz de confusion
MatrixConfusionCJHW <- matrizconfusion(df$class, clustersH)
MatrixConfusionCJHW

#Calculamos la precision del modelo
PrecisionHW <- precision(MatrixConfusionCJHW)
PrecisionHW

#Generamos la curva de ROC
modeloHW <- roc(df$class, clustersH)
plot(modeloHW,type="l",col="red")

#--------------------------------------------------------------------------------------
#                                 MEJOR MODELO
#--------------------------------------------------------------------------------------
precisiones <- c(PrecisionK, PrecisionDC, PrecisionHC, PrecisionDS, PrecisionHS, PrecisionDA, 
                 PrecisionHA, PrecisionDW, PrecisionHW)
x <- which.max(precisiones)
mejormodelo <- bestmodel(x)
cat("El mejor modelo es: ", mejormodelo, ", que posee una precision de: ", precisiones[x], ".")

