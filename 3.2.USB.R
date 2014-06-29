USB = function (x, b){
  #Performs the bootstrap method of Chen et al. (2011) for one-step ahead conditional squared return forecasts
  #x = data 
  #b = number of bootstrap replicates
    
  ##Step 1
  x = data.frame(x)
  armamodel = arma(x^2, order=c(1,1)) #equation 2.13
  omega = armamodel$coef[3]
  alphabeta = armamodel$coef[1]
  beta = armamodel$coef[2]
  
  ##Step 2
  #v = residual series
  v = c(0, armamodel$resid[2:length(armamodel$resid)]) #equation 2.14
  
  ##Step 3
  #vMean = average of ARMA (1,1) residuals from t = 2, ... , T
  #vCent = ARMA (1,1) centered residuals
  vMean = mean(armamodel$resid[2:length(armamodel$resid)])
  vCent = v - vMean #equation 2.15
  
  ##Steps 4 - 7
  #yForecasts = vector with the b conditional squared return forecasts
  yForecasts = vector(length = b)
    
    for (i in seq(b)){
      
      #Repeat Steps 4 to 7, until conditional squared return forecast
      yk = -1 #just to start while loop
      while (yk < 0){
        
        ##Step 4 
        #Ry = y^2 series replicate
        #RvCent = centered residual series replicate
        RvCent = sample(vCent, (length(vCent) + 151), replace = TRUE)
        Ry = vector(length = length(RvCent)-1)
        Ry[1] = omega + alphabeta * omega / (1 - alphabeta) + RvCent[1] #equation 2.16
    
        for (t in seq(2,(length(Ry)))){
          Ry[t] = omega + alphabeta * Ry[t-1] + RvCent[t] - beta * RvCent[t-1] #equation 2.16
        }
        Ry = Ry[151:length(Ry)]
    
        ##Step 5
        #Romega = ARMA(1,1) omega replicate
        #Ralphabeta = ARMA(1,1) alphabeta replicate
        #Rbeta = ARMA(1,1) beta replicate
        Rarmamodel = arma(Ry, order=c(1,1)) #equation 2.13
        Romega = Rarmamodel$coef[3]
        Ralphabeta = Rarmamodel$coef[1]
        Rbeta = Rarmamodel$coef[2]
        
        #Repeat Steps 4 and 5, until estimation of ARMA(1,1) parameters is successfull
        if (is.nan(Romega)){
          next
        }
             
      ## Step 7
      #yk = conditional squared return forecast       
      yk = (x[length(x)])^2 #last observation from x  
      yk = Romega + Ralphabeta * yk + RvCent[length(RvCent)] - Rbeta * RvCent[length(RvCent)-1] #equation 2.18
      yForecasts[i] = yk
    }
  } 
  return(yForecasts)
}
