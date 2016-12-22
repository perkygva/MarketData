source('G:/GENEVA/Code/R/Allan/Fun/Load.R')
library(caret)
library(tseries)
library(urca)
library(foreach)
library(mgcv)

#Data Download and organization / inspection
f = "PX_LAST"
u = c("c 2 comdty", "w 2 comdty")
sd = as.Date("01-01-2010", "%d-%m-%Y")
con = blpConnect()

data = bdh(u, f, start.date = sd, end.date = Sys.Date(), include.non.trading.days = F)

ggplot(df, aes(y = c.2.comdty, x = date, col = "red")) +geom_line()+ geom_line(aes(y=w.2.comdty, col = "blue"))

df = convertListToDT(data)
scatterplotMatrix(~c.2.comdty+w.2.comdty, data = df, spread = T, smoother.args = list(lty=2))
df2 = data.frame(df$date, CWarb = df$c.2.comdty-df$w.2.comdty)


#Corn-Wheat regression to review correlation then review cointegration
fit = lm(c.2.comdty~w.2.comdty, df)

#Testing Stationarity
adf.test(df2$CWarb)
pp.test(df2$CWarb)
po.test(cbind(2*df$w.2.comdty, -1.0*df$c.2.comdty))

##Centering and calculating Z scores (WIP)
centered.cwarb = scale(df2$CWarb, center = T, scale = F)
zsc.cwarb = scale(df2$CWarb,center = T, scale = T)

df3 = data.frame(cr = centered.cwarb, zsc = zsc.cwarb)

plot(scaled.cwarb)
lines(2*abs(zsc.cwarb), col = "red")


##Half life calculation
y = df2$CWarb
y.lag = lag(y, 1L)
delta.y = diff(y)

df.cw = data.frame(df2, y.lag, c(delta.y, NA))

regress.results = gam(delta.y~y.lag, df.cw[-1,])
lambda = summary(regress.results)$coefficients[2]
half.life = log(2)/lambda
