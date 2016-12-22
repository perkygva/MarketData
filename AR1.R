source('G:/GENEVA/Code/R/Allan/Fun/Load.R')
library(caret)
library(tseries)
install.packages("urca")
library(urca)

f = "px_last"
u = "kc2 comdty"
sd = as.Date("01-01-2010", "%d-%m-%Y")
con = blpConnect()

kc = bdh(u, f, start.date = sd, end.date = Sys.Date(), include.non.trading.days = F)
dkc <- diff(kc$px_last)
plot(dkc)
adf.test(dkc)

ar1 = auto.arima(kc$px_last)
aar = auto.arima(dkc)

accuracy(ar1)

acf(kc$px_last)
acf(dkc)
pacf(kc$px_last)
pacf(dkc)

tsdiag(ar1)
tsdiag(aar)

## Returns are stationary
adf.test(dkc)
pp.test(dkc)

pred = predict(ar1, n.ahead = 5)

plot(kc$px_last, xlim = c(2500, 3500)) #xlim = c(as.Date('2016-01-01'), as.Date('2017-01-01')))
lines(pred$pred, col="blue", lwd = 5)
lines(pred$pred + 2*pred$se, col = "red", lty=3, lwd = 5)
lines(pred$pred -2*pred$se, col = "red", lty=3, lwd=5)

#Alternative
fcst = forecast(ar1, 10)
forecast(aar, 10)

df = data.frame(kc,fcst = fcst$fitted, res = fcst$residuals)
plot(kc, type = "l")
plot(fcst$fitted, col = "red")
lines(fcst$fitted-fcst$residuals)

plotforecast

### Half Life
y = kc$px_last
y.lag = lag(y, 1L)
delta.y = diff(y)

df.kc = data.frame(kc, y.lag, c(delta.y, NA))

regress.results = lm(delta.y~y.lag, df.kc[-1,])
lambda = summary(regress.results)$coefficients[2]
half.life = -log(2)/lambda

