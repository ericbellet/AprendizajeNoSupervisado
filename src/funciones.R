#****************************************************************************************
                                        #K-MEDIAS
#****************************************************************************************
kmedias <- function(df, columns, k, name){
  # Aplica el algoritmo de k-medias al dataframe.
  #
  # Args:
  #   df: Dataframe que se le desea aplicar k-medias.
  #   columns: Columnas del dataframe que se le desea aplicar k-medias.
  #   k: N�mero de clusters que se desean.
  #   name: Nombre del dataframe.
  #
  # Returns:
  #   Retorna el modelo generado.
  
  modelo.kmedias = kmeans(x = df[, columns], centers = k)
  
  #GRAFICAMOS LOS CLUSTERS
  plot(df[, columns], col = modelo.kmedias$cluster, main = paste(c("K-MEANS: ", name)))
  
  # Ahora graficamos los centroides 
  points(x = modelo.kmedias$centers[, columns], col = 4:8, pch = 19, cex = 3)
  
  return(modelo.kmedias)
}

#****************************************************************************************
#                                 MATRIZ DE CONFUSION
#****************************************************************************************
matrizconfusion = function(class, clusters){
  x <- table(class, clusters, dnn=c("Clase", "Cluster")) 
  #colnames(x) <- 0:(ncol(x)-1) #Nombres
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
#Dado un n�mero de clusters k determinar la altura requerida para que tengamos el 
#n�mero de cluster k.
clusterJD <- function(df, distancia, columns, method, k, name){
  # Aplica el algoritmo de clusterizaci�n utilizando un k para determinar
  # la altura.
  #
  # Args:
  #   df: Dataframe que se le desea aplicar k-medias.
  #   distancia: Matriz de distancias.
  #   columns: Columnas del dataframe que se le desea aplicar k-medias.
  #   method: M�todo deseado para aplicar el algoritmo (complete, single, average o ward.d).
  #   k: N�mero de clusters que se desean.
  #   name: Nombre del dataframe.
  #
  # Returns:
  #   Retorna el modelo generado.
  
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
  plot(df[, columns], col = corteD, main = paste(c("Cluster jerarquico K: ", name)))
  
  return(corteD)
}
############################################################
#Dada una altura h (una medida de disimilaridad) determinar el n�mero de clusters que se obtienen.
clusterJH <- function(df, distancia, columns, method, h, name){
  # Aplica el algoritmo de clusterizaci�n utilizando un k para determinar
  # la altura.
  #
  # Args:
  #   df: Dataframe que se le desea aplicar k-medias.
  #   distancia: Matriz de distancias.
  #   columns: Columnas del dataframe que se le desea aplicar k-medias.
  #   method: M�todo deseado para aplicar el algoritmo (complete, single, average o ward.d).
  #   h:  Medida de disimilaridad.
  #   name: Nombre del dataframe.
  #
  # Returns:
  #   Retorna el modelo generado.
  
  #Aplicamos cluster jerarquico utilizando el metodo correspondiente
  cluster = hclust(distancia, method = method)
  #Graficamos el dendogram
  plot(cluster)
  
  ############################################################
  #Dada una altura h (una medida de disimilaridad) determinar 
  #el numero de clusters que se obtienen
  ############################################################
  # Cortamos por altura
  corteH = cutree(cluster, h = h)
  #Observamos la cantidad de clusters
  print(unique(corteH))
  #Graficamos los clusters
  plot(df[, columns], col = corteH, main = paste(c("Cluster jerarquico H: ", name)))
  
  return(corteH)
}
#****************************************************************************************
#                                 PRECISION DEL MODELO
#****************************************************************************************
precision <- function(m){
  # Calcula la precisi�n del modelo utilizando la matriz de confusi�n.
  #
  # Args:
  #   m: Matriz de confusi�n.
  #
  # Returns:
  #   Retorna la precisi�n del modelo.
  
#Precision
#P = (a+d)/(a+b+c+d)
  return(sum(diag(m)) /sum(m))
}
#****************************************************************************************
#                           MEJOR MODELO SEGUN LA PRECISION
#****************************************************************************************
bestmodel <- function(x){
  # Busca el mejor modelo.
  #
  # Args:
  #   x: Modelo con mayor precisi�n.
  #
  # Returns:
  #   Retorna el mejor modelo.
  
  
  if (x == 1){
    return("K-MEDIAS")
  }else if (x == 2){
    return("CLASIFICACION JERARQUICA K: METHOD COMPLETE")
  }else if (x == 3){
    return("CLASIFICACION JERARQUICA H: METHOD COMPLETE")
  }else if (x == 4){
    return("CLASIFICACION JERARQUICA K: METHOD SINGLE")
  }else if (x == 5){
    return("CLASIFICACION JERARQUICA H: METHOD SINGLE")
  }else if (x == 6){
    return("CLASIFICACION JERARQUICA K: METHOD AVERAGE")
  }else if (x == 7){
    return("CLASIFICACION JERARQUICA H: METHOD AVERAGE")
  }else if (x == 8){
    return("CLASIFICACION JERARQUICA K: METHOD WARD.D")
  }else{
    return("CLASIFICACION JERARQUICA H: METHOD WARD.D")
  }
}