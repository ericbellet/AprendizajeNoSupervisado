
setwd("C:/Users/Eric/Desktop/AprendizajeNoSupervisado")
source("src/funciones.R")
#Lectura de datos.
df = read.csv(file = "data/a_big.csv")
#Modificamos el nombre de las columnas por comodidad.
colnames(df) <- c("x","y","class")

#****************************************************************************************
#Analisis exploratorio del dataset
#****************************************************************************************
#Podemos observar que hay 3 columnas.
head(df)

#Observamos cuantos elementos hay de cada clase.
table(df$class)
#0      1      2 
#100000 100000  99999 

#Grafico 
plot(df$x, df$y)
#Podemos observar 3 conglomerados

length(unique(df$class))
#Existen 3 clases.
#****************************************************************************************
                                          #K-MEDIAS
#****************************************************************************************
#Aplicamos k=3 ya que identificamos 3 conglomerados y existen 3 clases.
x<-kmedias(df,1:2,3)



#Generamos la matriz de confusion
matrizconfusion <- table(df$class,modelo.kmedias$cluster,dnn=c("Clase", "Cluster"))
matrizconfusion
#****************************************************************************************
                                #Implementacion k-medias
#****************************************************************************************
