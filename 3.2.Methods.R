### Methods ###

HS = function(x,b){
  #Empirical distribution of historical simulation is equal to the one of the past observed data
  return (as.vector(x))
}

FHS = function(x, b){
  
  #Performs the bootstrap method of the Filtered Historical Simulation for one-step ahead conditional return forecasts
  #x = data 
  #b = number of bootstrap replicates
  
  #Step 1
  x = data.frame(x)
  garch = garchFit(formula = ~garch(1,1), data = x, include.mean = FALSE, trace = FALSE)
  omega = garch@fit$coef[1]
  alpha = garch@fit$coef[2]
  beta = garch@fit$coef[3]
  
  #Step 2
  #e = standardized residual series
  e = garch@residuals / garch@sigma.t
  
  #Steps 3
  #yForecasts = vector with the b conditional return forecasts
  yForecasts = vector(length = b)
  
  for (i in seq(b)){
    
    ##Step 3   
    quotient = omega / (1 - alpha - beta) #equation 2.13
    sigmak = quotient #equation 2.13
    
    for (j in seq(0,length(e)-2)){ #equation 2.13
      sigmak = sigmak + alpha * beta^(j) * (x[length(e)-j-1,]^2 - quotient)
    }  
    
    yk = (x[length(e),]) #last observation from x  
    sigmak = omega + alpha * yk^2 + beta * sigmak #equation 2.11
    yk = sqrt(sigmak) * sample(e, 1) #equation 2.12  
    yForecasts[i] = yk
  } 
  return(yForecasts)
}

PRR = function (x, b){
  #Performs the bootstrap method of Pascual et al. (2006) for one-step ahead conditional return forecasts
  #x = data 
  #b = number of bootstrap replicates
  
  #Step 1
  x = data.frame(x)
  garch = garchFit(formula = ~garch(1,1), data = x, include.mean = FALSE, trace = FALSE)
  omega = garch@fit$coef[1]
  alpha = garch@fit$coef[2]
  beta = garch@fit$coef[3]
  
  #Step 2
  #e = standardized residual series
  e = garch@residuals / garch@sigma.t
  
  ##Steps 3 - 5
  #yForecasts = vector with the b conditional return forecasts
  yForecasts = vector(length = b)
  
  for (i in seq(b)){
    
    ##Step 3 
    #Ry = y series replicate
    #Rsigma = sigma squared series replicate
    Rsigma = vector(length = length(e)+150)
    Ry = vector(length = length(e)+150)
    Rsigma[1] = omega / (1 - alpha - beta) 
    Ry[1] = sqrt(Rsigma[1]) * sample(e, 1) #equation 2.9
    
    for (t in seq(2,length(Ry))){
      Rsigma[t] = omega + alpha * (Ry[t-1])^2 + beta * (Rsigma[t-1]) #equation 2.8
      Ry[t] = sqrt(Rsigma[t]) * sample(e, 1) #equation 2.9
    }
    
    #Discard the first 150 to reduce the starting values effect
    Rsigma = Rsigma[151:length(Rsigma)]
    Ry = Ry[151:length(Ry)]
    
    ##Step 4
    #Romega = Garch (1,1) omega replicate
    #Ralpha = Garch (1,1) alpha replicate
    #Rbeta = Garch (1,1) beta replicate
    Rgarch = garchFit(formula = ~garch(1,1), data = Ry, include.mean = FALSE, trace = FALSE)
    Romega = Rgarch@fit$coef[1]
    Ralpha = Rgarch@fit$coef[2]
    Rbeta = Rgarch@fit$coef[3]
    
    #Repeat Steps 3 and 4, until estimation of GARCH(1,1) parameters is successfull
    if (is.nan(Romega)) next
    
    ## Step 5
    #sigmak = conditional volatility forecast
    #yk = conditional return forecast   
    quotient = Romega / (1 - Ralpha - Rbeta) #equation 2.12
    sigmak = quotient #equation 2.12
    
    for (j in seq(0,length(e)-2)){ #equation 2.12
      sigmak = sigmak + Ralpha * Rbeta^(j) * (Ry[length(e)-j-1]^2 - quotient)
    }  
    
    yk = (x[length(e),]) #last observation from x  
    sigmak = Romega + Ralpha * yk^2 + Rbeta * sigmak #equation 2.10
    yk = sqrt(sigmak) * sample(e, 1) #equation 2.11  
    yForecasts[i] = yk
  } 
  return(yForecasts)
}


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
        
    ##Step 4 
    #Ry = y^2 series replicate
    #RvCent = centered residual series replicate
    RvCent = sample(vCent, (length(vCent) + 151), replace = TRUE)
    Ry = vector(length = length(RvCent)-1)
    Ry[1] = omega + alphabeta * omega / (1 - alphabeta) + RvCent[1] #equation 2.16
    for (t in seq(2,(length(Ry))))
      Ry[t] = omega + alphabeta * Ry[t-1] + RvCent[t] - beta * RvCent[t-1] #equation 2.16
    
    #Discard the first 150 to reduce the starting values effect
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
    if (is.nan(Romega)) next
             
    ## Step 7
    #yk = conditional squared return forecast       
    yk = (x[length(e),])^2 #last observation from x  
    yk = Romega + Ralphabeta * yk + RvCent[length(RvCent)] - Rbeta * RvCent[length(RvCent)-1] #equation 2.18
      
    #Repeat Steps 4 to 7, until conditional squared return forecast is positive
    if (yk < 0) next
      
    yForecasts[i] = yk
  } 
  return(yForecasts)
}