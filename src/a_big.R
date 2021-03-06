install = function(pkg)
{
  # Si ya est� instalado, no lo instala.
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

#Ordenamos la columna clase
df <- df[ order(df$class), ]
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


#********************************************************************************
#                                     K-MEDIAS
#********************************************************************************
kkmedias <- function(x, centers, dist, maxIter) {
  # Aplica el algoritmo de k-medias al dataframe.
  #
  # Args:
  #   df: Dataframe que se le desea aplicar k-medias.
  #   centers: Centroides.
  #   dist: Matriz de distancias
  #   maxIter: Numero maximo de iteracciones.
  #
  # Returns:
  #   Retorna una lista con los clusters generados por el modelo y los centroides finales.
    
  clusters <- vector(maxIter, mode="list")
  centers <- vector(maxIter, mode="list")
    
    for(i in 1:maxIter) {
      distsToCenters <- dist
      cluster <- apply(distsToCenters, 1, which.min)
      center <- apply(x, 2, tapply, cluster, mean)
      clusters <- cluster
      centers <- center
    }#endfor
    
    return(list(clusters = clusters, centers= centers))
}

#********************************************************************************
#                            DISTANCIA EUCLIDIANA
#********************************************************************************
distancia <- function(df, centros) {
  # Calcula la distancia entre los centros iniciales
  # y todos los puntos del dataframe utilizando la distancia euclidiana de norma 2.
  #
  # Args:
  #   df: Puntos del dataframe.
  #   centros: Centros iniciales.
  #
  # Returns:
  #   Retorna todas las distancias de los centros con los del dataframe.
  
  #Genero la matriz de las distancias vacia.
  euclidiana <- matrix(NA, 
                       nrow=dim(df)[1], 
                       ncol=dim(centros)[1])
  
  #Calculo la distancia euclidiana con cada centro.
  for(i in 1:nrow(centros)) {
    #Distancia euclidiana: d_E(P_1,P_2)=\sqrt{(x_2-x_1)^2+(y_2-y_1)^2}
    euclidiana[,i] <- sqrt(rowSums(t(t(df) - centros[i,])^2))
  }#endfor
  
  return(euclidiana)
}

#********************************************************************************
#                            SAMPLE ESTRATIFICADO
#********************************************************************************
stratified = function(df, class, size) {
  # Genera un un sample estratificado del dataset.
  #
  # Args:
  #   df: Dataframe original.
  #   class: Columna clase.
  #   size: Tamano del dataframe resultado
  #
  # Returns:
  #   Retorna un dataframe estratificado.
  
  temp = df[order(df[class]),]
  if (size < 1) {
    size = ceiling(table(temp[class]) * size)
  } else if (size >= 1) {
    size = rep(size, times=length(table(temp[class])))
  }  
  strat = strata(temp, stratanames = names(temp[class]), 
                 size = size, method = "srswor")
  (dsample = getdata(temp, strat))
}

#********************************************************************************
#                     CALCULO DE CENTROIDES SOBRE EL SUBCONJUNTO
#********************************************************************************
#Genero un sample estratificado de size 500.
df_stra <- stratified(df, 3, 500)

#Podemos observar que se parece a.csv
plot(df_stra$x,df_stra$y)

stra <- df_stra
stra$class <- NULL
stra$ID_unit <- NULL
stra$Prob <- NULL
stra$Stratum <- NULL
kstra <- as.matrix(stra)
#Genero centroides aleatorios
centers <- kstra[sample(nrow(kstra), 3),] 

#Calculo la distancia euclidiana
dist <- distancia(kstra, centers)

#Genero el k-medias para el sample.
res <- kkmedias(kstra, centers, dist, 1000)
plot(df_stra$x, df_stra$y, col = res$clusters)

# Ahora graficamos los centroides 
points(x = res$centers, col = 4:8, pch = 19, cex = 3)

#********************************************************************************
#APLICAR K-MEDIAS UTILIZANDO TODO EL CONJUNTO DE DATOS CON  LOS CENTROIDES
#FINALES DEL SAMPLE ESTRATIFICADO
#********************************************************************************
dfcompleto <- df
dfcompleto$class <- NULL
dfcompleto <- as.matrix(dfcompleto) 

#Utilizo como centroides iniciales, los centroides finales del sampling.
centers <- res$centers

dist <- distancia(dfcompleto, centers)

#Coloco un maximo de iteracciones bajo ya que indicandole unos centroides iniciales
#adecuados, el algoritmo converge rapido.

res <- kkmedias(dfcompleto, centers, dist, 5)

#Graficamos los clusters
plot(df$x, df$y, col = (res$clusters))
# Ahora graficamos los centroides 
points(x = res$centers, col = 4:8, pch = 19, cex = 3)

#Generamos la matriz de confusion
MatrixConfusion <- matrizconfusion(df$class,res$clusters)
MatrixConfusion




