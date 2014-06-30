### Packages and Scripts ###

library(fGarch) #garchFit
library(tseries) #arma
library(PerformanceAnalytics) #chart.TimeSeries #zoo (underlying zoo package)

setwd("C:/Users/David/BootstrapRisk")
source("Methods.R") #FHS #HS #PRR #USB #empiricalDist #risk
source("Backtests.R") #exceptions

### Implementation ###

#ret0912, ret13 and ret13Dates are obtained by running the script Data&Sample

#Run Methods
PRRdensities = empiricalDist(ret0912, ret13, 1000, 253, PRR)
USBdensities = empiricalDist(ret0912, ret13, 1000, 253, USB)
FHSdensities = empiricalDist(ret0912, ret13, 1000, 253, FHS)

ret1112 = ret0912[506:length(ret0912)]
HSdensities = empiricalDist(ret1112, ret13, length(ret1112), 253, HS)

rm(ret0912)
rm(ret1112)
rm(FHS)
rm(HS)
rm(PRR)
rm(USB)
rm(empiricalDist)

#Estimate VaR and ES
PRRrisk = risk(PRRdensities, 0.01, "PRR")
USBrisk = risk(USBdensities, 0.01, "USB")
FHSrisk = risk(FHSdensities, 0.01, "FHS")
HSrisk = risk(HSdensities, 0.01, "HS")

#Build zoo object with risk matrix for the 4 methods
PRRriskzoo = zoo(PRRrisk, ret13Dates)
USBriskzoo = zoo(USBrisk, ret13Dates)
FHSriskzoo = zoo(FHSrisk, ret13Dates)
HSriskzoo = zoo(HSrisk, ret13Dates)
rm(ret13Dates)
rm(PRRrisk)
rm(USBrisk)
rm(FHSrisk)
rm(HSrisk)
rm(risk)

#Plots of the risk measures
par(mfrow=c(2,1))
par(mar=c(3,4,2,2))
chart.TimeSeries(PRRriskzoo, lwd = 1.5, ylab = "(a)", xlab = "", main = "", date.format = "%Y-%m-%d" , font.main = 1, ylim = c(2,5.5),lty = c("solid", "dotted"), col=c("black","black"))
chart.TimeSeries(USBriskzoo, lwd = 1.5, ylab = "(b)", xlab = "", main = "", date.format = "%Y-%m-%d" , font.main = 1, ylim = c(2,5.5),lty = c("solid", "dotted"), col=c("black","black"))
chart.TimeSeries(FHSriskzoo, lwd = 1.5, ylab = "(a)", xlab = "", main = "", date.format = "%Y-%m-%d" , font.main = 1, ylim = c(1,4.5),lty = c("solid", "dotted"), col=c("black","black"))
chart.TimeSeries(HSriskzoo, lwd = 1.5, ylab = "(b)", xlab = "", main = "", date.format = "%Y-%m-%d" , font.main = 1, ylim = c(1,4.5),lty = c("solid", "dotted"), col=c("black","black"))


### Backtesting ###

exceptions(PRRriskzoo[,1], ret13)
exceptions(USBriskzoo[,1], ret13)
exceptions(FHSriskzoo[,1], ret13)
exceptions(HSriskzoo[,1], ret13)