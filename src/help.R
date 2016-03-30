
library(rgl)
#Lectura de datos.
df = read.csv(file = "C:/Users/Eric/Desktop/AprendizajeNoSupervisado/data/help.csv")
#Modificamos el nombre de las columnas por comodidad.
colnames(df) <- c("x","y","z","class")

plot3d(df$x, df$y, df$z, type = "s",size = 2, col = rainbow(1000))
