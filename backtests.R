exceptions = function(VaR, returns){
  #Generates a vector with 0's in the days in which exceptions do not occur
  #and the daily return in the ones where they do
  #VaR = Value-at-Risk estimates for n days
  #returns = daily returns data of the n days

  output = vector(mode = "numeric", length = length (VaR))
  
  for (i in seq(length(VaR))){
    if (-returns[i] > VaR[i]){
      output[i] = returns[i]
    }
  }
  return(output)
}


LRuc = function(excepts, alpha){
  #Generates Kupiec Unconditional Coverage Test statistic - equation 2.28
  #excepts = vector with 0's in the days in which exceptions do not occur
  #and the daily return in the ones where they do
  #alpha = alpha used in the VaR estimation
  
  n = length(excepts)
  m = sum(excepts != 0)
  stat = 2 * log  ((1 - m/n)^(n - m) * (m/n)^m) - 2 * log((1 - alpha)^(n - m) * alpha^m)  
  
  return (stat)
}


LRind = function(excepts){
  #Generates Christoffersen Serial Independence Test statistic - equation 2.29
  #excepts = vector with 0's in the days in which exceptions do not occur
  #and the daily return in the ones where they do
  
  n00 = 0
  n01 = 0
  n10 = 0
  n11 = 0
  
  for (i in seq(length(excepts) - 1)){
    
    if (excepts[i] == 0){
      if (excepts[i+1] == 0) n00 = n00 + 1
      
      else n01 = n01 + 1
    }
    
    if (excepts[i] != 0){
      if (excepts[i+1] == 0) n10 = n10 + 1
      
      else n11 = n11 + 1
    }
  }
  
  pi01 = n01 / (n00 +n01)
  pi11 = n11 / (n10 +n11)
  pi = (n01 + n11) / (n00 +n01 + n10 +n11)
  LR = 2 * log  ((1 - pi01)^(n00) * pi01^n01 * (1 - pi11)^(n10) * pi11^n11) - 2 * log((1 - pi)^(n00 + n10) * pi^(n01 + n11))
  
  return (LR)
}


LRcc = function(excepts, alpha){
  #Generates Christoffersen Conditional Coverage Test statistic - equation 2.30
  #exceptions = vector with 0's in the days in which exceptions do not occur
  #and the daily return in the ones where they do
  #alpha = alpha used in the VaR estimation
  
  stat = LRuc(excepts, alpha) + LRind(excepts)
  
  return (stat)
}
  

accuracyVaR = function(VaR, returns, alpha){
  #Perfoms the following accuracy backtests on the VaR estimates:
  #1-Number of exceptions as tested by Basel 
  #2-Kupiec Unconditional Coverage Test 
  #3-Christoffersen Serial Independence Test 
  #4-Christoffersen Conditional Coverage Test
  
  #VaR = Value-at-Risk estimates for n days
  #returns = daily returns data of the n days
  #alpha = alpha used in the VaR estimation
  
  output = vector(length = 7)
  names(output) = c("#Exceptions", "LRuc Statistic", "LRuc p-value", "LRind Statistic", "LRind p-value", "LRcc Statistic", "LRcc p-value")
  excepts = exceptions(VaR, returns)
  
  output[1] = sum(excepts != 0)
  output[2] = LRuc(excepts, alpha)
  output[3] = 1 - pchisq(q = output[2], df = 1)
  output[4] = LRind(excepts)
  output[5] = 1 - pchisq(q = output[4], df = 1)
  output[6] = LRcc(excepts, alpha)
  output[7] = 1 - pchisq(q = output[6], df = 2)
  
  return(output)
}


rlf = function(VaR, returns, alpha){
  #Generates a vector with Sarma et al. (2003) Cm,t for each trading day in var - equation 2.33
  #VaR = Value-at-Risk estimates for n days
  #returns = daily returns data of the n days
  #alpha = alpha used in the VaR estimation
  
  VaR = as.vector(VaR)
  returns = as.vector(returns)
  c = rep(0, length(VaR))
  
  for (i in seq(length(VaR))){
    if (-returns[i] > VaR[i]) c[i] = (returns[i] - VaR[i])^2
  }
  return(c)
}


qlf = function(VaR, returns, alpha){
  #Generates a vector with Angelidis(2004) Cm,t for each trading day in var - equation 2.35
  #VaR = Value-at-Risk estimates for n days
  #returns = daily returns data of the n days
  #alpha = alpha used in the VaR estimation
  
  VaR = as.vector(VaR)
  returns = as.vector(returns)
  c = rep(0, length(VaR))
  
  for (i in seq(length(VaR))){
    if (-returns[i] > VaR[i]) c[i] = (returns[i] - VaR[i])^2

    else c[i] = (sort(returns)[ceiling(length(returns) * alpha)] - VaR[i])^2
  }
  return(c)
}


sStatistic = function(xVaR, yVaR, returns, lf, alpha){
  #Generates the S-statistic of Sarma et al. (2003) hypothesis test for a given loss function
  #H0: both methods have equal performance
  #H1: method x has better performance than method y
  #xVaR = Value-at-Risk estimates for n days of method x
  #yVaR = Value-at-Risk estimates for n days of method y
  #returns = daily returns data of the n days
  #lf = loss function
  #alpha = alpha used in the VaR estimation
  
  Cx = lf(xVaR, returns, alpha)
  Cy = lf(yVaR, returns, alpha)
  z = Cx - Cy
  
  s = 0
  for (i in z){
    if (i >= 0) s = s + 1
  } 
  return(s)
}


compareVaR = function(varList, returns, lf, methodNames, alpha){
  #Perfoms tSarma et al. (2003) hypothesis test to compare all possible pairs of VaR estimates
  #of the methods included in varList
  #Methods in columns follow the same order as in rows
  #H0: both methods have equal performance
  #H1: method in column has better performance than method in row
  #varList = list of vectors of the VaR estimates for n days of each method
  #returns = daily returns data of the n days
  #lf = loss function
  #methodNames = vector with the names of the methods
  #alpha = alpha used in the VaR estimation
  
  dimensions = length(varList)
  output = matrix(data = vector(length = dimensions * 2) , nrow = dimensions, ncol = dimensions * 2)
  rownames(output) = methodNames
  colnames(output) = rep(c("Statistic", "p-value"), dimensions)
  l = length(returns)
  
  methods = 0
  for(i in seq(dimensions)){
    for(j in seq(dimensions)){
      s = sStatistic(varList[[i]], varList[[j]], returns, lf, alpha)
      output[j, i + methods] = s
      output[j, i + methods + 1] = pnorm((s - 0.5 * l) / sqrt(0.25 * l))
    }
    methods = methods + 1
  }
  return(output)
}


RCtest = function(riskzoo, densities, returns, alpha, b = 1000, n = 10000){
  #Performs Righi and Ceretta (2013) Expected Shortfall Backtest
  #riskzoo = zoo object with VaR and ES estimates for n days
  #densities = matrix with the empirical distributions (columns) for n trading days (rows)
  #returns = daily returns data of the n days
  #alpha = alpha used in the VaR estimation
  #b = number of bootstrap replicates
  #n = size of the bootstrap sample
  
  excepts = exceptions(riskzoo[,1], returns)
  rowsNumber = sum(excepts != 0)
  output = matrix(data = vector(length = rowsNumber * 6) , nrow = rowsNumber, ncol = 6)
  colnames(output) = c("TradingDay", "Statistic", "Critic1%", "Critic5%", "Critic10%", "p-value")
  row = 1
  for(i in seq(length(excepts))){
    if(excepts[i] != 0){
      
      ret = returns[i]
      empiricalDist = densities[,i]
      l = length(empiricalDist)
      ES = mean(empiricalDist[1:(alpha * l)])
      SD = sd(empiricalDist[1:(alpha * l)])
      testStat = (ret - ES) / SD
      
      Vpvalue = vector(length = b)
      Vtcrit10 = vector(length = b)
      Vtcrit5 = vector(length = b)
      Vtcrit1 = vector(length = b)
      
      for (k in seq(b)){
        replicateDist = sample(empiricalDist, n, replace=TRUE)
        replicateDist = sort(replicateDist)
        ESr = mean(replicateDist[1:(alpha * n)])
        SDr = sd(replicateDist[1:(alpha * n)])
        replicateDist = (replicateDist - ESr) / SDr
        
        m = 0
        for (j in seq(alpha * n)){
          if (replicateDist[j] < testStat) m = m + 1
        }
        
        Vpvalue[k] = m / (alpha * n)
        Vtcrit10[k] = replicateDist[0.1 * alpha * n]
        Vtcrit5[k] = replicateDist[0.05 * alpha * n]
        Vtcrit1[k] = replicateDist[0.01 * alpha * n]
      }
      
      output[row, 1] = i
      output[row, 2] = testStat
      output[row, 3] = median(Vtcrit1)
      output[row, 4] = median(Vtcrit5)
      output[row, 5] = median(Vtcrit10)
      output[row, 6] = median(Vpvalue)
      
      row = row + 1
    }       
  }
  return(output)  
}
