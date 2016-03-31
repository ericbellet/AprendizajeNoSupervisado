#****************************************************************************************
                                        #K-MEDIAS
#****************************************************************************************
kmedias <- function(df,columns,k){
  
  modelo.kmedias = kmeans(x = df[, columns], centers = k)
  
  #GRAFICAMOS LOS CLUSTERS
  plot(df[, columns], col = modelo.kmedias$cluster)
  
  # Ahora graficamos los centroides 
  points(x = modelo.kmedias$centers[, columns], col = 1:4, pch = 19, cex = 3)
  
  return(modelo.kmedias$cluster)
}
#****************************************************************************************
                                  #MATRIZ DE CONFUSION
#****************************************************************************************
matrizconfusion <- function(class, clusters){
  x <- table(class, clusters, dnn=c("Clase", "Cluster"))
  posicion <- vector(mode = "numeric", length = nrow(x))
  for (j in 1:ncol(x)) {
    maximo <- 0
    for (i in 1:nrow(x)) {
      if (maximo <= x[i,j]){
        maximo <- x[i,j]
        posicion[j] <- i
      }
    }
  }
  colnames(x) <- 0:(ncol(x)-1)
  z <- x
  
  for (i in 1:length(posicion)) {
    z[i,] <- x[posicion[i],] 
  }
  return(z)
}
#****************************************************************************************
                                  #CLUSTERS JERARQUICOS
#****************************************************************************************
#Dado un número de clústers k determinar la altura requerida para que tengamos el número de clúster k.
clusterJD <- function(df, distancia, columns, method, k){
  #Aplicamos cluster jerarquico utilizando el metodo correspondiente
  cluster = hclust(distancia, method = method)
  ############################################################
  #Determinar la altura requerida dado un numero de clusters k
  ############################################################
  #Cortamos el dendograma con K clases
  corteD = cutree(cluster, k = k)
  #Observamos la cantidad de clusters
  unique(corteD)
  #Graficamos los clusters
  plot(df[, columns], col = corteD)
  
  return(corteD)
}
#Dada una altura h (una medida de disimilaridad) determinar el número de clústers que se obtienen.
clusterJH <- function(df, distancia, columns, method, h){
  #Aplicamos cluster jerarquico utilizando el metodo correspondiente
  cluster = hclust(distancia, method = method)
  #Graficamos el dendogram
  plot(cluster)
  
  ############################################################
  #Dada una altura h (una medida de disimilaridad) determinar 
  #el número de clústers que se obtienen
  ############################################################
  # Cortamos por altura
  corteH = cutree(cluster, h = h)
  #Observamos la cantidad de clusters
  print(unique(corteH))
  #Graficamos los clusters
  plot(df[, columns], col = corteH)
  
  return(corteH)
}