
euclidean_distance = function(p1,p2){
  return(sqrt(sum((p1-p2)^2)))
}

weightedDistance = function(x1,x2,sigma,distance_function = euclidean_distance){
  distance = euclidean_distance(x1,x2)
  w = exp(-(distance^2)/(2 * (sigma^2)))
  return (w)
}


dwnn = function(x, dataset, sigma, nClass = 1,weight_function = weightedDistance, distance_function = euclidean_distance){

  var.index = ncol(dataset)-nClass
  weights = apply(dataset,1,function(row){
    row = row[1:(length(row)-nClass)]
     (weightedDistance(x,row,sigma,distance_function))
  })
  predictions = apply(dataset[,(var.index+1):ncol(dataset),drop=F],2,function(col){
    sumD = col %*% weights
    sumDV = sum(weights)
    sumD/sumDV
  })
  
  return(predictions)
}
  