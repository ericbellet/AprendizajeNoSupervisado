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
matrizconfusion = function(class, clusters){
  x <- table(class, clusters, dnn=c("Clase", "Cluster")) 
  colnames(x) <- 0:(ncol(x)-1) #Nombres
  x1 <- x #Para buscar los maximos
  x2 <- x #Para asignar columnas
  #Tabla vacia
  for (h in 1:nrow(x1)) {
    x1[h,] <- -1
  }
  
  #Mientras la matriz no sea vacia.
  while (empty(x) == F) {
    maximo <- -1
    for (i in 1:nrow(x)) {
      for (j in 1:ncol(x)) {
        if (x[i,j] > maximo){
          maximo <- x[i,j]
          
          mi <- i
          mj <- j
        }#endif
      }#endfor
    }#endfor
    #Si no existe un valor en la diagonal.
    if (x1[mi,mi] == -1){
      
      x1[,mi] <- x2[,mj]
      x[,mj] <- -1
    }else{
    #Si existe, probar con otro maximo
      x[mi,mj] <- -1
    }#endif
   
    
  }#endwhile
  return(x1)
}

#Funcion que me retorna TRUE si la tabla esta vacia.
empty <- function(x){
  boolean <- T
  for (i in 1:nrow(x)) {
    for (j in 1:ncol(x)) {
      if (x[i,j] != -1){
        boolean <- F
      }
    }
  }
  return(boolean)
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