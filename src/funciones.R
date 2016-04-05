#****************************************************************************************
                                        #K-MEDIAS
#****************************************************************************************
kmedias <- function(df, columns, k, name){
  # Aplica el algoritmo de k-medias al dataframe.
  #
  # Args:
  #   df: Dataframe que se le desea aplicar k-medias.
  #   columns: Columnas del dataframe que se le desea aplicar k-medias.
  #   k: Número de clusters que se desean.
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
#                                     MATRIZ DE CONFUSION
#****************************************************************************************
matrizconfusion = function(class, clusters){
  # Genera la matriz de confusión asociada al modelo.
  #
  # Args:
  #   class: Columna clase del dataframe.
  #   clusters: Clusters generados por el modelo.
  #
  # Returns:
  #   Retorna la matriz de confusión del modelo correspondiente.
  
  #Multiplico * 10 simplemente para poder reemplazar los numeros de forma correcta.
  clusters <- clusters * 10
  #Obtengo los valores unicos ordenados de la forma del modelo.
  ordermodel <- unique(clusters)
  
  #Cambio el nombre de los clusters ordenados crecientemente.
  for (i in 1:length(clusters)) {
    for (j in 1:length(ordermodel)) {
      if (clusters[i] == ordermodel[j]){
        clusters[i] <- j
      }
    }
  }
  #Guardo cuantas clases hay
  elem <- table(df$class)
  elem <- as.vector(elem)
  
  #Inicializo una tabla vacia con el tamano adecuado
  init <- table(df$class, clusters)
  for (h in 1:nrow(init)) {
    init[h,] <- 0
  }
  
  #M significa la posicion donde va empezar a leer en el modelo.
  m <- 1
  #z acumula las distancias a leer.
  z <- 0
  
  #Recorro clase por clase.
  for (i in 1:length(elem)){
    
    n <- elem[i]
    n <- n + z
    t <- table(clusters[m:n])
    c <- names(t)
    t <- as.vector(t)
    
    m <- n + 1
    z <- n
    for (j in 1:length(t)) {
      init[i, as.numeric(c[j])] <- t[j]
    }
    
  }#endfor que recorre
  return(init)
}#endfunction
#****************************************************************************************
                                  #CLUSTERS JERARQUICOS
#****************************************************************************************
#Dado un número de clusters k determinar la altura requerida para que tengamos el 
#número de cluster k.
clusterJD <- function(df, distancia, columns, method, k, name){
  # Aplica el algoritmo de clusterización utilizando un k para determinar
  # la altura.
  #
  # Args:
  #   df: Dataframe que se le desea aplicar k-medias.
  #   distancia: Matriz de distancias.
  #   columns: Columnas del dataframe que se le desea aplicar k-medias.
  #   method: Método deseado para aplicar el algoritmo (complete, single, average o ward.d).
  #   k: Número de clusters que se desean.
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
#Dada una altura h (una medida de disimilaridad) determinar el número de clusters que se obtienen.
clusterJH <- function(df, distancia, columns, method, h, name){
  # Aplica el algoritmo de clusterización utilizando un k para determinar
  # la altura.
  #
  # Args:
  #   df: Dataframe que se le desea aplicar k-medias.
  #   distancia: Matriz de distancias.
  #   columns: Columnas del dataframe que se le desea aplicar k-medias.
  #   method: Método deseado para aplicar el algoritmo (complete, single, average o ward.d).
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
  # Calcula la precisión del modelo utilizando la matriz de confusión.
  #
  # Args:
  #   m: Matriz de confusión.
  #
  # Returns:
  #   Retorna la precisión del modelo.
  
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
  #   x: Modelo con mayor precisión.
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