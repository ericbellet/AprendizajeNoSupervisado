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
name = "guess.csv"
#Lectura de datos.
df = read.csv(file = "C:/Users/Eric/Desktop/AprendizajeNoSupervisado/data/guess.csv", header = F)
#Modificamos el nombre de las columnas por comodidad.
colnames(df) <- c("x","y")

#****************************************************************************************
#                         Analisis exploratorio del dataset
#****************************************************************************************
#Podemos observar que hay 2 columnas.
head(df)

#Grafico 
plot(df$x, df$y, xlab = "x", ylab = "y", main = name)
PCA <- PCA(df)

plot(PCA)
#Podemos observar 2 conglomerados

#Aplicamos codo de Jambu como ayuda para seleccionar el K
InerciaIC = rep(0, 30)
for (k in 1:30) {
  grupos = kmeans(df, k)
  InerciaIC[k] = grupos$tot.withinss
}
plot(InerciaIC, col = "blue", type = "b", main = "Codo de Jambu")
#Segun mi analisis el k adecuado es 2

#****************************************************************************************
                                          #K-MEDIAS
#****************************************************************************************
#Aplicamos k=2 ya que identificamos 2 conglomerados.
#kmedias(Dataframe, Columnas,K)
modeloK <- kmedias(df, 1:2, 2, name)

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
clustersD <- clusterJD(df, distancia, 1:2, "complete", 2, name)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "complete", 100, name)

#--------------------------------------------------------------------------------------
                                  #METHOD SINGLE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:2, "single", 2, name)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "single", 10, name)

#--------------------------------------------------------------------------------------
                                  #METHOD AVERAGE
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:2, "average", 2, name)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "average", 50, name)

#--------------------------------------------------------------------------------------
#                                   METHOD ward.D
#--------------------------------------------------------------------------------------
#Dado un número de clústers k determinar la altura requerida 
#para que tengamos el número de clúster k.
clustersD <- clusterJD(df, distancia, 1:2, "ward.D", 2, name)

#**********************************************************
#Dada una altura h (una medida de disimilaridad) determinar 
#el número de clústers que se obtienen.
clustersH <- clusterJH(df, distancia, 1:2, "ward.D", 30000, name)


