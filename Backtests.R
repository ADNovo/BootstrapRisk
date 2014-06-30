exceptions = function(var, returns){
  #Counts the number of times the negative of the returns is higher than the estimated VaR
  #var = Value-at-Risk estimates for n days
  #returns = daily returns data of the n days
  
  n = 0
  
  for (i in seq(length(var))){
    if (-returns[i] > var[i]) n = n + 1
  } 
  return(n)
}
