install.packages("fpp2")
install.packages("fpp")
install.packages("urca")

library("fpp2")
library("fpp")
library("urca")

rain= scan("http://robjhyndman.com/tsdldata/hurst/precip1.dat",skip=1)
rainseries <- ts(rain, frequency=12, start=c(1987,1))

#a
plot(rainseries)

#b
fit_Exl <-ses(rainseries)

#c
lines(fitted(fit_Exl), col="blue", type="o")

#d
fit_Exl$model

#e
fcast<-forecast(fit_Exl, h=2)

#f
fc_holt <-holt(rainseries)
plot(rainseries)
lines(fitted(fc_holt), col = "blue", type ="o")

