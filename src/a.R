library(pROC)
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

#Ordenamos la columna clase
df <- df[ order(df$class), ]
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
modeloK <- kmedias(df, 1:2, 3, name)

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

elem <- table(df$class)
elem <- as.vector(elem)
init <- table(df$class, clustersD)
for (h in 1:nrow(init)) {
  init[h,] <- 0
}
m <- 1
z <- 0
for (i in 1:length(elem)){
  
  n <- elem[i]
  n <- n + z
  t <- table(clustersD[m:n])
  c <- names(t)
  t <- as.vector(t)
  
  m <- n + 1
  z <- n
  for (j in 1:length(t)) {
    init[i, as.numeric(c[j])] <- t[j]
  }
  
}#endfor que recorre
#Calculamos la precision del modelo
PrecisionDC <- precision(MatrixConfusionCJDC)
PrecisionDC

#Generamos la curva de ROC
modeloDC <- roc(df$class, clustersD)
plot(modeloDC,type="l",col="red")
#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el numero de clusters que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "complete", 30, name)
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
                                    #METHOD SINGLE
#--------------------------------------------------------------------------------------
#Dado un numero de clusters k determinar la altura requerida 
#para que tengamos el numero de cluster k.
clustersD <- clusterJD(df, distancia, 1:2, "single", 3, name)
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
#el numero de clusters que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "single", 30, name)
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
                                    #METHOD AVERAGE
#--------------------------------------------------------------------------------------
#Dado un numero de clusters k determinar la altura requerida  
#para que tengamos el numero de cluster k.
clustersD <- clusterJD(df, distancia, 1:2, "average", 3, name)
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
#el numero de clusters que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "average", 15, name)
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
                                    #METHOD ward.D
#--------------------------------------------------------------------------------------
#Dado un numero de clusters k determinar la altura requerida 
#para que tengamos el numero de cluster k.
clustersD <- clusterJD(df, distancia, 1:2, "ward.D", 3, name)
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
#el numero de clusters que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "ward.D", 10000, name)
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



