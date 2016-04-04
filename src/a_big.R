setwd("C:/Users/Eric/Desktop/AprendizajeNoSupervisado")
source("src/funciones.R")
name = "a_big.csv"
#Lectura de datos.
df = read.csv(file = "data/a_big.csv", header = F)
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
#1       2        3
#100000 100000  100000 

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
MatrixConfusion <- matrizconfusion(df$class,modeloK$cluster)
MatrixConfusion

#Calculamos la precision del modelo
PrecisionK <- precision(MatrixConfusionK)
PrecisionK

#Generamos la curva de ROC
modeloKROC <- roc(df$class, modeloK$cluster)
plot(modeloKROC,type="l",col="red")
#****************************************************************************************
                                #Implementacion k-medias
#****************************************************************************************
kmedias <- function(df, k, maxIter){
  """
  Selects K centroids (K rows chosen at random)
  Assigns each data point to its closest centroid
  Recalculates the centroids as the average of all data points in a
  cluster (i.e., the centroids are p-length mean vectors, where p is the number of variables)
  Assigns data points to their closest centroids
  Continues steps 3 and 4 until the observations are not reassigned or the maximum number of 
  iterations (R uses 10 as a default) is reached.
  """
  return()
}