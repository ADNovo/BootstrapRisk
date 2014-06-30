empiricalDist = function (x, y, b, n, method){
  
  output = matrix(data = vector(length = b * n) , nrow = b, ncol = n)

  for (i in seq(n)){
    
    if (i == 1){ 
      dataset = x
    } 
    else{
      dataset = c(x[i:length(x),], y[1:(i - 1),])
    }
    
    densities = sort(method(dataset, b))
    output[,i] = densities
    
    cat(i," ") #control printing   
  }
  return (output)
}

PRRdensities = empiricalDist(ret0912, ret13, 1000, 253, PRR)