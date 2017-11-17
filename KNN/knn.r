
#Simple KNN by William A. da Rosa

euclidean_distance = function(p1,p2){
  return(sqrt(sum((p1-p2)^2)))
}

knn = function(dataset,x,distance_function = euclidean_distance,k=1){
  class_id = ncol(dataset)
  distances = apply(dataset,1,function(row){
      distance_function(
          as.numeric (row [1:(class_id-1)]),
          as.numeric (x   [1:(class_id-1)]))
  })
  
  nn_ids =  sort.list(distances,dec=F)[1:k]
  nn_classes = dataset[nn_ids,class_id]
  
  classes = unique(nn_classes)
  occurs = NULL
  
  for(i in 1:length(classes)){
    ids = nn_classes[nn_classes == classes[i]]
    occurs[i] = length(ids)
  }

  
  ret = NULL
  ret$y = classes[which.max(occurs)]
  ret$count = occurs[which.max(occurs)]

  return (ret)
}

knn.accuracy = function(examples_dataset,train_dataset,distance_function = euclidean_distance,k=1){
  y = train_dataset[,ncol(train_dataset)]

 predict_y = knn.classify(examples_dataset,train_dataset,distance_function,k)
  
  correct = length(which(y == predict_y))
  acc = correct / nrow(train_dataset)
  
  return (acc)
}

knn.classify = function(examples_dataset,train_dataset,distance_function = euclidean_distance,k=1){
  y = train_dataset[,ncol(train_dataset)]
  
  out = numeric(nrow(train_dataset));
  for(i in 1:nrow(train_dataset)){
    out[i] = knn(examples_dataset,train_dataset[i,],distance_function,k)$y;
  }
  
  return (out)
}