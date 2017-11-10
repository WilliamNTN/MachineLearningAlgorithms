#MLP by William A. Rosa

sigmoid = function(x){
  return (1/(1+exp(-x)))
}

d_sigmoid = function(f_x){
 return ((f_x) * (1 - (f_x)))
}

mlp.architecture = function(input_size,hidden_size,output_size,activation_function = sigmoid, activation_function_derivate = d_sigmoid){

  model = list()
  
  model$hidden_size = hidden_size
  model$input_size = input_size
  model$output_size = output_size
    
  model$hidden_weights = matrix(runif(n=(input_size+1) * hidden_size,min = -0.5,max = 0.5),nrow = hidden_size)
  model$output_weights = matrix(runif(n=(hidden_size+1) * output_size,min=-0.5,max=0.5),nrow=output_size)
  
  model$activation_function = activation_function
  model$activation_function_derivate = activation_function_derivate
  #             #input 1        #input 2        #bias
  # neuron 1    value           ....            ....
  # neuron 2    value
  # neuron 3    value     
  return(model)
}


mlp.forward = function(x,model){
  
    model$hidden_net = model$hidden_weights %*% c(x,1) # 1 for bias
    model$f_hidden_net = model$activation_function(model$hidden_net)
    
    model$output_net = model$output_weights %*% c(model$f_hidden_net,1)
    model$f_output_net = model$activation_function(model$output_net)
    
    return(model)
}

mlp.backpropagation = function(dataset, model, threshold = 1e-3, learning_rate = 0.1,max_iters = 500){
  sError = 2*threshold;
  iters = 0
  while( (sError > threshold) && iters < max_iters){
    sError = 0;
    
    for(i in 1:nrow(dataset)){
      X = as.numeric(dataset[i,1:model$input_size])
      Y = as.numeric(dataset[i,(model$input_size+1):ncol(dataset)])

      result = mlp.forward(X,model)
      Out = result$f_output_net
    
      error = Y - Out
      sError = sError + sum(error^2)
      
      
      

      delta_output = error * model$activation_function_derivate(result$f_output_net)
     
      output_weights = model$output_weights[,1:model$hidden_size]
      
      delta_hidden = as.numeric(model$activation_function_derivate(result$f_hidden_net)) * (as.numeric(delta_output) %*% output_weights)
      
      model$output_weights = model$output_weights + (learning_rate * (as.numeric((delta_output %*% c(result$f_hidden_net,1)))) )
      
      model$hidden_weights = model$hidden_weights + (learning_rate * (t(delta_hidden) %*% c(X,1)))
        
        
    }
    sError = sError / nrow(dataset)
    cat("SquaredError = ",sError,"\n")
    iters = iters+1
  }
  
  return (model)
}

