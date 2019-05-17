#Note that the correlogram of a volatile series does not differ significantly
#from white noise (Fig. 7.4b), but the series is non-stationary since the variance
#is different at different times.

visit <- file.path(getwd(), 'osvisit.dat')
visit
visit.ts <- ts(visit, start = 1977, freq = 12)
plot(log(visit.ts))
acf(log(visit.ts))
#As acf shows, the data is seasonal and possesses a trend.
visit.arima <- arima(log(visit.ts), order = c(1, 1, 0))
acf(resid(visit.arima))
# still we have a seasonal effect unremoved from the pattern

visit.arima.seasonal <- arima(log(visit.ts), order = c(1, 1, 0), seas = list(order = c(0,1,0), 12))
acf(resid(visit.arima.seasonal))

AIC(visit.arima.seasonal <- arima(log(visit.ts), order = c(1, 1, 0), seas = list(order = c(1,1,0), 12)))
AIC(visit.arima.seasonal <- arima(log(visit.ts), order = c(0, 1, 1), seas = list(order = c(0,1,1), 12)))
AIC(visit.arima.seasonal <- arima(log(visit.ts), order = c(1, 1, 0), seas = list(order = c(0,1,1), 12)))
AIC(visit.arima.seasonal <- arima(log(visit.ts), order = c(0, 1, 1), seas = list(order = c(1,1,0), 12)))
AIC(visit.arima.seasonal <- arima(log(visit.ts), order = c(1, 1, 1), seas = list(order = c(1,1,1), 12)))
AIC(visit.arima.seasonal <- arima(log(visit.ts), order = c(1, 1, 1), seas = list(order = c(1,1,0), 12)))
AIC(visit.arima.seasonal <- arima(log(visit.ts), order = c(1, 1, 1), seas = list(order = c(0,1,1), 12)))

visit.arima.best.fit <- arima(log(visit.ts), order = c(1, 1, 1), seas = list(order = c(0,1,1), 12))
acf(resid(visit.arima.best.fit))
visit.arima.best.fit$coef
# the best fitted model is stationary as the roots of characteristic function is not larger than unity
sigma <- visit.arima.best.fit$sigma
lognorm.correction.factor <- exp((1/2) * sigma^2)
ts.plot( cbind( visit.ts,
                exp(predict(visit.arima.best.fit, 12)$pred)*lognorm.correction.factor), lty = 1:3)



get.best.arima <- function(x.ts, maxord = c(1,1,1,1,1,1))
{
  best.aic <- 1e8
  n <- length(x.ts)
  for (p in 0:maxord[1]) for(d in 0:maxord[2]) for(q in 0:maxord[3])
    for (P in 0:maxord[4]) for(D in 0:maxord[5]) for(Q in 0:maxord[6])
    {
      fit <- arima(x.ts, order = c(p,d,q),
                   seas = list(order = c(P,D,Q),
                               frequency(x.ts)), method = "CSS")
      fit.aic <- -2 * fit$loglik + (log(n) + 1) * length(fit$coef)
      if (fit.aic < best.aic)
      {
        best.aic <- fit.aic
        best.fit <- fit
        best.model <- c(p,d,q,P,D,Q)
      }
    }
  list(best.aic, best.fit, best.model)
}
cbe <- "C:/Users/vahid/Documents/Introductory_Time_Series_with_R_datasets-master/cbe.dat"
cbe.table <- read.table(cbe, header = T)
choc.ts <- ts(cbe.table[,1], start = 1958, freq = 12)
plot(choc.ts)
acf(choc.ts)


best.arima.choc <- get.best.arima( choc.ts,
                                   maxord = c(2,2,2,2,2,2))
print(best.arima.choc)
best.fit.choc <- best.arima.choc[[2]]
acf( resid(best.fit.choc) )
best.arima.choc [[3]]
ts.plot( cbind( window(choc.ts,start = 1958),
                predict(best.fit.choc,12)$pred ), lty = 1:2)


stock <- "C:/Users/vahid/Documents/Introductory_Time_Series_with_R_datasets-master/stockmarket.dat"
stock.table <- read.table(stock, header = T)
print(stock.table[0:3,])
stock.ts <- ts(stock.table, start = c(1986, 6), freq = 1)
ams.ts <- stock.ts[,1]
plot(ams.ts, ylab='Amsterdam')
plot(diff(ams.ts))
acf(ams.ts, lag=60)
AIC(ams.ts.arima <- arima(ams.ts, order = c(0, 1, 0)))
AIC(ams.ts.arima <- arima(ams.ts, order = c(1, 1, 0)))
AIC(ams.ts.arima <- arima(ams.ts, order = c(0, 1, 1)))
AIC(ams.ts.arima <- arima(ams.ts, order = c(1, 1, 1)))
ams.ts.arima <- arima(ams.ts, order = c(0, 1, 0))
acf(resid(ams.ts.arima))
acf(resid(ams.ts.arima)^2)
library(tseries)
AIC(ams.garch <- garch(resid(ams.ts.arima), order = c(0, 1), trace = F))
AIC(ams.garch <- garch(resid(ams.ts.arima), order = c(1, 0), trace = F))
AIC(ams.garch <- garch(resid(ams.ts.arima), order = c(1, 1), trace = F))
AIC(ams.garch <- garch(resid(ams.ts.arima), order = c(0, 2), trace = F))
ams.garch <- garch(resid(ams.ts.arima), order = c(1, 1), trace = F)
t(confint(ams.garch))
ams.garch.res <- resid(ams.garch)[-1]
acf(ams.garch.res)
acf(ams.garch.res^2)


stemp <- scan("C:/Users/vahid/Documents/Introductory_Time_Series_with_R_datasets-master/stemp.dat")
stemp.ts <- ts(stemp, start = 1850, freq = 12)
plot(stemp.ts)
stemp.best <- get.best.arima(stemp.ts, maxord = rep(2,6))
stemp.best[[3]]
stemp.arima <- arima(stemp.ts, order = c(1,1,2),
                     seas = list(order = c(2,0,1), 12))
acf(resid(stemp.arima))
acf(resid(stemp.arima)^2)

stemp.garch <- garch(resid(stemp.arima), trace = F)
confint(stemp.garch)
stemp.garch.res <- resid(stemp.garch)[-1]
acf(stemp.garch.res)
acf(stemp.garch.res^2)

# GARCH model for 2.5% confidence intnerval
set.seed(1)
alpha0 <- confint(stemp.garch)[1,1]
alpha1 <- confint(stemp.garch)[2,1]
beta1 <- confint(stemp.garch)[3,1]
w <- rnorm(12)
a <- rep(0, 12)
h <- rep(0, 12)
for (i in 2:12) {
  h[i] <- alpha0 + alpha1 * (a[i - 1]^2) + beta1 * h[i -
                                                       1]
  a[i] <- w[i] * sqrt(h[i])
}

a_2.5 <- a[i]

set.seed(1)
alpha0 <- confint(stemp.garch)[1,2]
alpha1 <- confint(stemp.garch)[2,2]
beta1 <- confint(stemp.garch)[3,2]
w <- rnorm(12)
a <- rep(0, 12)
h <- rep(0, 12)
for (i in 2:12) {
  h[i] <- alpha0 + alpha1 * (a[i - 1]^2) + beta1 * h[i -
                                                       1]
  a[i] <- w[i] * sqrt(h[i])
}
a_97.5 <- a
stemp.ts.win <- window(stemp.ts, c(2000,1), c(2007,12))
print(length(predict(stemp.arima,12)$pred))
p <- predict(stemp.arima,12)$pred
ts.plot( cbind( stemp.ts.win,
                p, p+a_2.5, p+a_97.5), lty = 1:4)