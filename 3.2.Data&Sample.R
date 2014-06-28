### Packages ###

library(PerformanceAnalytics) #chart.TimeSeries #zoo (underlying zoo package)
library(moments) #skewness #kurtosis #agostino.test #anscombe.test
library(tseries) #jarque.bera.test
library(FinTS) #AutocorTest #ArchTest



### Functions ###

descriptive.stats = function(x)
  #Returns descriptive statistics of the data set x
{
  output = vector(length = 7)
  names(output) = c("Mean", "Median", "Min", "Max", "Std. Dev.", "Skewness", "Kurtosis")
  
  output[1] = mean(x)
  output[2] = median(x)
  output[3] = min(x)
  output[4] = max(x)
  output[5] = sd(x)
  output[6] = skewness(x)
  output[7] = kurtosis(x)
  
  return (output)
}

norm.tests = function(x)
  #Returns the test statistics and the p-values of the Jarque-Bera test for normality,
  #D'Agostino test of skewness and Anscombe-Glynn test of kurtosis of the data set x
{
  output = vector(length = 6)
  names(output) = c("JB Statistic", "JB p-value", "DA Statistic", "DA p-value", "AG Statistic", "AG p-value")
  
  output[1] = jarque.bera.test(x)$statistic
  output[2] = jarque.bera.test(x)$p.value
  output[3] = agostino.test(x, alternative = "greater")$statistic[2]
  output[4] = agostino.test(x, alternative = "greater")$p.value
  output[5] = anscombe.test(x, alternative = "less")$statistic[2]
  output[6] = anscombe.test(x, alternative = "less")$p.value
  
  return (output)
}

corr.test = function(x, y = c(), n = 1){
  #1) Calculates the statistics and p-values of the Ljung-Box and LM tests on the data set x.
  #2) Calculates the statistics and p-values of the Ljung-Box and LM tests on for n rolling windows.
  #2.1) The first data set is x.
  #2.2) The subsequent ones are obtained by dropping the first observation of the previous set and 
  #adding the first observation of y not yet added.
  
  output = matrix(data = vector(length = 4 * n) , nrow = n, ncol = 4)
  colnames(output) = c("LB statistic", "LB p.value", "LM Statistic", "LM p.value")
  
  m = round(log(length(x))) 
  
  for (i in seq(n)){
    
    if (i == 1){ 
      dataset = x
    } 
    else{
      dataset = c(x[i:length(x),], y[1:(i - 1),])
    }
    
    output[i,1] = AutocorTest(data.frame(dataset), m)$statistic
    output[i,2] = AutocorTest(data.frame(dataset), m)$p.value
    output[i,3] = ArchTest (data.frame(dataset), lags = m)$statistic
    output[i,4] = ArchTest (data.frame(dataset), lags = m)$p.value    
  }
  return (output)
}



### Data and Sample Analysis ###

#input = imported dataframe with the dates in the first column and the daily index levels on the second

#returns = daily log returns of the index
#Coerce dates to date format and drop the first
#Build zoo object with returns and dates
returns = diff(log(input[,2]))*100
dates = as.Date(input[,1])[2:length(input[,1])]
rm(input)
ret.zoo = zoo(returns, dates)
rm(returns)

#Plot of the daily log returns of the index (Figure 3.1)
par(mfrow=c(1,1))
par(mar = c(3,4,2,2))
chart.TimeSeries(ret.zoo, lwd = 1.5, ylab ="", xlab ="", date.format = "%Y-%m-%d" , font.main = 1, ylim = c(-5.5,5))

#Random generated sample from a normal distribution with same mean and standard deviation
norm.dist = rnorm(1000000, mean = mean(ret.zoo), sd = sd(ret.zoo)) 

#Plot of the empirical density of the daily log returns against the one of a normal distribution with same mean and
#standard deviation (Figure 3.2 - (a))
par(mfrow=c(1,2))
plot(density(ret.zoo), type = "l", xlab = "" , ylab = "", main = "(a)", lwd=1.5, font.main = 1)
lines(density(norm.dist), type="l", lty="dotted", lwd=1.5)
abline(v=0)
rm(norm.dist)

#Normal Quantile-Quantile plot of the daily log returns (Figure 3.2 - (b))
qqnorm(ret.zoo, xlab = "" , ylab = "", main = "(b)", font.main = 1)
qqline(ret.zoo)

#Derscriptite statistics of the daily log returns (Table 3.1)
descriptive.stats(ret.zoo)
rm(descriptive.stats)

#Jarque-Bera test for normality, D'Agostino test of skewness and Anscombe-Glynn test of 
#kurtosis of the daily log returns
norm.tests(ret.zoo)
rm(norm.tests)

#Split ret.zoo in 2 zoo objects: first with return data from 2009-2012 and the second with the one of 2013
ret0912.zoo = ret.zoo[1:1008]
ret13.zoo = ret.zoo[1009:length(ret.zoo)]
rm(ret.zoo)

#Plot of the p-values of the Ljung-Box and Lagrange Multiplier tests of the daily log returns 253 sub-samples (Figure 3.3)
ret.corr = corr.test(ret0912.zoo, ret13.zoo, 253)
ret13.dates = dates[1009:length(dates)]
retlb.zoo = zoo(ret.corr[,2], ret13.dates)
retlm.zoo = zoo(ret.corr[,4], ret13.dates)
rm(dates)
rm(ret.corr)

par(mfrow=c(2,1))
chart.TimeSeries(retlb.zoo, lwd = 1.5, ylab ="(a)", xlab ="", date.format = "%Y-%m-%d" , font.main = 1, , type="p", ylim = c(0,1))
abline(a=0.05, b=0, lwd = 1.5, lty = "dashed")
rm(retlb.zoo)

chart.TimeSeries(retlm.zoo, lwd = 1.5, ylab ="(b)", xlab ="", date.format = "%Y-%m-%d" , font.main = 1, , type="p", ylim = c(0,1))
abline(a=0.05, b=0, lwd = 1.5, lty = "dashed")
rm(retlm.zoo)