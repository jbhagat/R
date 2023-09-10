income <- c(27.8,28.5,30,35,36.4)
age<- c(22,23,26,27,35)
plot(age,income)
cov(age,income) #covariance
cor(age,income) #coefficient of correlation

age<-(40,45,55,59,65)
income<- (50.4,52.4)

cov(age,income) #covariance
cor(age,income) #coefficient of correlation


library(fpp2)
y=c(20,30,35,45)



VCR <- c(123,130,125,138,145,142,141,146,147,157,150,160)
plot(VCR,type="p") #p - point
lines(VCR) #lines connected to each point
Acf(VCR)

Acf(VCR,plot=FALSE)
ggAcf(VCR) #alternative way
upperVCR <- 1.96*1/sqrt(length(VCR))


#produces random 
set.seed(30)
y<- rnorm(50)
plot(y, type = "p")
lines(y)

rnorm(50, mean=1, sd=2)

ggACF(y)
Acf(y)

#Week 5
y<-ts(c(58,54,60,55))

#Average Method
#This is the average of the previous values/total value number
meanf(y,h=1)
meanf(y,h=2)
meanf(y,h=100)


#Naive Method
#Naive method is simply the last observation
naive(y,h=1)
rwf(y, h=1) #Alternative Way

#Seasonal Naive Method
y<-ts(c(58,54,60,55,58,54,60,55),frequency=4)
snaive(y,h=1)
snaive(y,h=4)

#Drift Method - used for trend graphs
#(54-58)/2 + (60-54)/2 + (55-60)/2 

y<-ts(c(58,54,60,55))
rwf(y, h=1, drift=TRUE)
rwf(y, h=2, drift=TRUE)


#call dataset
ausbeer

beer2 <-window(ausbeer, start=c(1992,1), end=c(2007,4))


# Plot some forecasts
autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
            series="Mean", PI=FALSE) +
  autolayer(naive(beer2, h=11),
            series="Naïve", PI=FALSE) +
  autolayer(snaive(beer2, h=11),
            series="Seasonal naïve", PI=FALSE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") 


# Plot some forecasts
autoplot(beer2) +
  autolayer(meanf(beer2, h=11),
            series="Mean", PI=TRUE) +
  autolayer(naive(beer2, h=11),
            series="Naïve", PI=TRUE) +
  autolayer(snaive(beer2, h=11),
            series="Seasonal naïve", PI=TRUE) +
  ggtitle("Forecasts for quarterly beer production") +
  xlab("Year") + ylab("Megalitres") 

#Slide 10 #shows 40 future predicted values since h=40
autoplot(goog200) +
  autolayer(meanf(goog200, h=40),
            series="Mean", PI=FALSE) +
  autolayer(rwf(goog200, h=40),
            series="Naïve", PI=FALSE) +
  autolayer(rwf(goog200, drift=TRUE, h=40),
            series="Drift", PI=FALSE) +
  ggtitle("Google stock (daily ending 6 Dec 2013)") +
  xlab("Day") + ylab("Closing Price (US$)") 


#Drift 




#Slide 14
beer2 <- window(ausbeer,start=1992,end=c(2007,4))
beerfit1 <- meanf(beer2,h=10)
beerfit2 <- rwf(beer2,h=10)
beerfit3 <- snaive(beer2,h=10)
autoplot(window(ausbeer, start=1992)) +
  autolayer(beerfit1, series="Mean", PI=FALSE) +
  autolayer(beerfit2, series="Naïve", PI=FALSE) +
  autolayer(beerfit3, series="Seasonal naïve", PI=FALSE) +
  xlab("Year") + ylab("Megalitres") +
  ggtitle("Forecasts for quarterly beer production") +
  guides(colour=guide_legend(title="Forecast"))





#The purpose is to minimize the error or "Test set" in the output
beer3 <- window(ausbeer, start=2008)
accuracy(beerfit1, beer3)
accuracy(beerfit2, beer3)
accuracy(beerfit3, beer3)

View(goog)

Train_data=window(goog,start=1, end=150)

autoplot(window(goog, start=1, end=250)) +
  autolayer(beerfit1, series="Mean", PI=FALSE) +
  autolayer(beerfit2, series="Naïve", PI=FALSE) +
  autolayer(beerfit3, series="Drift", PI=FALSE) +
  xlab("Day") + ylab("Closing Price") +
  ggtitle("Forecasts for google Stock") +
  guides(colour=guide_legend(title="Forecast"))


googfc1 <- meanf(Train_data, h=100) #Average Method
googfc2 <- rwf(Train_data, h=100)     #Naive Method
googfc3 <- rwf(Train_data, drift=TRUE, h=100) #Drift Method


Test_data = window(goog,start=151, end=250)
accuracy(googfc1, Test_data)
accuracy(googfc2, Test_data)
accuracy(googfc3, Test_data)

