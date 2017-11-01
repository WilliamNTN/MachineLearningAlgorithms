
euclidean_distance = function(p1,p2){
  return(sqrt(sum((p1-p2)^2)))
}

getDataClusters = function(dataset,clusters_centers,distance_function=euclidean_distance){
 
  data_clusters = data_clusters = apply(dataset,1,function(row){
    
   distance = distance_function(row,clusters_centers[1,])
    index = 1
    for(i in 2:nrow(clusters_centers)){
      dAux = distance_function(row,clusters_centers[i,])
      if(dAux < distance){
        distance = dAux
        index = i
      }
    }
    return (index)
  })
  return(unname(data_clusters))
}

kMeans = function(dataset,clusters_centers,distance_function=euclidean_distance,threshold = 1e-3){
  
  dataset = as.matrix(dataset)
  nClusters = nrow(clusters_centers)
  dimension = ncol(clusters_centers)
  gap = 2 * threshold
  
  while(gap > threshold){
      data_clusters = getDataClusters(dataset,clusters_centers,distance_function)
      last_centers = clusters_centers
      for(i in 1:nClusters){
        ids = which(data_clusters == i)
        clusters_centers[i,] = colMeans(dataset[ids,])
      }
      gap = 0
      for(i in 1:nClusters){
        gap = gap + distance_function(last_centers[i,],clusters_centers[i,])
      }
      gap = gap/nClusters

  }
  
  ret = NULL
  ret$clusters_centers = clusters_centers
  ret$data_clusters = getDataClusters(dataset,clusters_centers)
  
  return(ret)
}
