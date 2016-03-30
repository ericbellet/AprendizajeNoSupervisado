kmedias <- function(df,columns,k){
  #Aplicamos k=3 ya que identificamos 3 conglomerados y existen 3 clases.
  modelo.kmedias = kmeans(x = df[, columns], centers = k)
  
  #GRAFICAMOS LOS CLUSTERS
  plot(x = df$x, y = df$y, col = modelo.kmedias$cluster)
  
  # Ahora graficamos los centroides 
  points(x = modelo.kmedias$centers[, c("x", "y")], col = 1:4, pch = 19, cex = 3)
  
  return(modelo.kmedias$cluster)
}