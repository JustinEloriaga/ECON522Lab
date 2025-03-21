install.packages("vars")
library(urca)
library(vars)
library(mFilter)
library(tseries)
library(TSstudio)
library(forecast)
library(tidyverse)

mp <−read_csv(file.choose())
head(mp)

lnIP <−ts(mp$lnIP, start = c(2003,1,1), frequency = 12)
lnM1 <−ts(mp$lnM1, start = c(2003,1,1), frequency = 12)
M1 <−ts(mp$M1, start = c(2003,1,1), frequency = 12)
CPI <−ts(mp$CPI, start = c(2003,1,1), frequency = 12)
RRP <−ts(mp$RRP, start = c(2003,1,1), frequency = 12)

ts_plot(lnIP)
ts_plot(lnM1)
ts_plot(M1)
ts_plot(CPI)
ts_plot(RRP)

pp.test(lnIP)
pp.test(M1)
pp.test(lnM1)
pp.test(CPI)
pp.test(RRP)

v1 <−cbind(RRP, lnM1, CPI, lnIP)
colnames(v1) <−cbind("RRP","M1","CPI", "lnIP")

lagselect <−VARselect(v1, lag.max = 15, type = "const")
lagselect$selection

Model1 <−VAR(v1, p = 2, type = "const", season = NULL, exog = NULL)
summary(Model1)

Serial1 <−serial.test(Model1, lags.pt = 5, type = "PT.asymptotic")
Serial1

Arch1 <−arch.test(Model1, lags.multi = 15, multivariate.only = TRUE)
Arch1

Stability1 <−stability(Model1, type = "OLS-CUSUM")
plot(Stability1)

GrangerRRP<−causality(Model1, cause = "RRP")
GrangerRRP
GrangerM1 <−causality(Model1, cause = "M1")
GrangerM1
GrangerCPI <−causality(Model1, cause = "CPI")
GrangerCPI
GrangerlnIP <−causality(Model1, cause = "lnIP")
GrangerlnIP

RRPirf <−irf(Model1, impulse = "RRP", response = "RRP", n.ahead = 20, boot = TRUE)
plot(RRPirf, ylab = "RRP", main = "RRP’s shock to RRP")
M1irf <−irf(Model1, impulse = "RRP", response = "M1", n.ahead = 20, boot = TRUE)
plot(M1irf, ylab = "M1", main = "RRP’s shock to M1")
CPIirf <−irf(Model1, impulse = "RRP", response = "CPI", n.ahead = 20, boot = TRUE)
plot(CPIirf, ylab = "CPI", main = "RRP’s shock to CPI")
lnIPirf <−irf(Model1, impulse = "RRP", response = "lnIP", n.ahead = 20, boot = TRUE)
plot(lnIPirf, ylab = "lnIP", main = "RRP’s shock to lnIP")