library(PerformanceAnalytics) #chart.TimeSeries
library(moments) #skewness #kurtosis #agostino.test #anscombe.test
library(tseries) #jarque.bera.test

data = dados.tese[6314:7575,]
#data = imported dataframe with the dates in the first column and the daily index levels on the second

#returns = daily log returns of the index
returns = diff(log(data[,2]))*100

#Coerce dates to date format and drop the first
dates = as.Date(data[,1])[2:length(data[,1])]

#Build zoo object with returns and dates
ret.zoo = zoo(returns, dates)

#Plot of the daily log returns of the index (Figure 3.1)
par(mar = c(3,4,2,2))
chart.TimeSeries(ret.zoo, lwd = 1.5, ylab ="", xlab ="", date.format = "%Y-%m-%d" , font.main = 1, ylim = c(-5.5,5))

#Random generated sample from a normal distribution with same mean and standard deviation
norm.dist = rnorm(1000000, mean = mean(returns), sd = sd(returns)) 

#Plot the empirical density of the daily log returns against the one of a normal distribution with same mean and
#standard deviation (Figure 3.2 - (a))
par(mfrow=c(1,2))
plot(density(returns), type = "l", xlab = "" , ylab = "", main = "(a)", lwd=1.5, font.main = 1)
lines(density(norm.dist), type="l", lty="dotted", lwd=1.5)
abline(v=0)

#Normal Quantile-Quantile plot of the daily log returns (Figure 3.2 - (b))
qqnorm(returns, xlab = "" , ylab = "", main = "(b)", font.main = 1)
qqline(returns)


#Derscriptite statistics of the daily log returns (Table 3.1)

descriptive.stats = function(data)
  #Returns descriptive statistics of data
{
  table = vector(length = 7)
  names(table) = c("Mean", "Median", "Min", "Max", "Std. Dev.", "Skewness", "Kurtosis")
  
  table[1] = mean(data)
  table[2] = median(data)
  table[3] = min(data)
  table[4] = max(data)
  table[5] = sd(data)
  table[6] = skewness(data)
  table[7] = kurtosis(data)
  
  return (table)
}

descriptive.stats(returns)

#Jarque-Bera test for normality, D'Agostino test of skewness and Anscombe-Glynn test of 
#kurtosis of the daily log returns

norm.tests = function(data)
  #Returns the test statistics and the p-values of the Jarque-Bera test for normality,
  #D'Agostino test of skewness and Anscombe-Glynn test of kurtosis of data
{
  table = vector(length = 6)
  names(table) = c("JB Statistic", "JB p-value", "DA Statistic", "DA p-value", "AG Statistic", "AG p-value")
  
  table[1] = jarque.bera.test(data)$statistic
  table[2] = jarque.bera.test(data)$p.value
  table[3] = agostino.test(data, alternative = "greater")$statistic[2]
  table[4] = agostino.test(data, alternative = "greater")$p.value
  table[5] = anscombe.test(data, alternative = "less")$statistic[2]
  table[6] = anscombe.test(data, alternative = "less")$p.value
  
  return (table)
}

norm.tests(returns)
