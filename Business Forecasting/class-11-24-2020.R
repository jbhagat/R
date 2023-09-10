install.packages("fpp2")
install.packages("fpp")
install.packages("urca")

library("fpp2")
library("fpp")
library("urca")


#First dataset test
Test=ur.kpss(goog)
summary(Test)

plot(goog)

diff(goog)
Test2=ur.kpss(diff(goog))
summary(Test2)

#diff test
ndiffs(goog)

#Second dataset test
Test=ur.kpss(usmelec)
summary(Test)

diff(usmelec)
Test2=ur.kpss(diff(usmelec))
summary(Test2)

#diff test
ndiffs(usmelec)

#autoregressive of order 1
Arima(wmurders, order=c(1,0,0))
Arima(wmurders, order=c(3,0,0))