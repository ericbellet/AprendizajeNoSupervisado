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
kmedias <- function(x, centers, dist, maxIter) {
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
  
  #Genero la matriz de las distancias
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


#********************************************************************************
#                     CALCULO DE CENTROIDES SOBRE EL SUBCONJUNTO
#********************************************************************************


#********************************************************************************
#APLICAR K-MEDIAS UTILIZANDO TODO EL CONJUNTO DE DATOS CON  LOS CENTROIDES
#FINALES DEL SAMPLE ESTRATIFICADO
#********************************************************************************
test=df
test$class <- NULL# A data.frame
ktest=as.matrix(test) # Turn into a matrix
centers <- ktest[sample(nrow(ktest), 3),] # Sample some centers, 5 for example
centers[1,1] <- modeloK$centers[1,1]
centers[1,2] <- modeloK$centers[1,2]

centers[2,1] <- modeloK$centers[2,1]
centers[2,2] <- modeloK$centers[2,2]

centers[3,1] <- modeloK$centers[3,1]
centers[3,2] <- modeloK$centers[3,2]


dist <- distancia(ktest, centers)

res <- kmedias(ktest, centers, dist, 1)
plot(df$x, df$y, col = (res$clusters))

# Ahora graficamos los centroides 

points(x = res$centers, col = 4:8, pch = 19, cex = 3)




