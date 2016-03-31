setwd("C:/Users/Eric/Desktop/AprendizajeNoSupervisado")
source("src/funciones.R")
library(rgl)

#Lectura de datos.
df = read.csv(file = "C:/Users/Eric/Desktop/AprendizajeNoSupervisado/data/s.csv")

colnames(df) <- c("x","y","z","class")
df <- df[ order(df$class), ]
plot3d(df$x, df$y, df$z, type = "s",size = 2, col = df$class)
df$class <- df$class +10
df2 <- df[1:2000,]
plot3d(df2$x, df2$y, df2$z, type = "s",size = 2, col = df2$class)
