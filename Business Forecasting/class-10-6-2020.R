#Simple Linear Regression#
install.packages("fpp")
library(fpp)
library(fpp2)

# Slide 11
View(fuel)

plot(jitter(fuel$City) , jitter(fuel$Carbon),xlab="City (mpg)",
     ylab="Carbon footprint (tons per year)")


plot(jitter(Carbon) ~ jitter(City),xlab="City (mpg)",
     ylab="Carbon footprint (tons per year)",data=fuel)


fit <- lm(Carbon ~ City, data=fuel)
# fit <- lm(fuel$Carbon ~ fuel$City)

abline(fit)

fit

summary(fit)

res <-residuals(fit)
plot(jitter(res)~jitter(City), ylab="Residuals", xlab="City", data=fuel)
abline(0,0)

install.packages("fpp")

library(fpp)
library(fpp2)

# Slide 11


View(fuel)

plot(jitter(fuel$City) , jitter(fuel$Carbon),xlab="City (mpg)",
     ylab="Carbon footprint (tons per year)")


plot(jitter(Carbon) ~ jitter(City),xlab="City (mpg)",
     ylab="Carbon footprint (tons per year)",data=fuel)


fit <- lm(Carbon ~ City, data=fuel)
# fit <- lm(fuel$Carbon ~ fuel$City)

abline(fit)

#Slide 12
fit
summary(fit)


#Slide 13
res <- residuals(fit)
plot(jitter(res)~jitter(City), ylab="Residuals", xlab="City", data=fuel)
abline(0,0)


#Slide 19
summary(fit)


#Slide 22
fitted(fit)[1]

fcast <- forecast(fit, newdata=data.frame(City=c(30)))
plot(fcast, xlab="City (mpg)", ylab="Carbon footprint (tons per year)")


fitted(fit)[1]
fcast <- forecast(fit, newdata=data.frame(City=c(35,45,66)))
plot(fcast, xlab="City (mpg)", ylab="Carbon footprint (tons per year)")


# Slide 23
summary(fit)


# Slide 24
confint(fit,level=0.95)


# Slide 26
#par(mfrow=c(1,2)) 
fit2 <- lm(log(Carbon) ~ log(City), data=fuel)
plot(jitter(Carbon) ~ jitter(City), xlab="City (mpg)",
     ylab="Carbon footprint (tonnes per year)", data=fuel)


plot(log(jitter(Carbon)) ~ log(jitter(City)), 
     xlab="log City mpg", ylab="log carbon footprint", data=fuel)
fit2 <- lm(log(Carbon) ~ log(City), data=fuel)

abline(fit2)



# Slide 27
res <- residuals(fit2)
plot(jitter(res, amount=0.005) ~ jitter(log(City)), 
     ylab="Residuals", xlab="log(City)", data=fuel)


#Slide 29
education <- c(10,15,20,25,30,35,40) # x values
income <- c(10^3,15^3,20^3,25^3,30^3,35^3,40^3) #y values

plot(education, income)

fit5<-lm(income ~ education)  ## lm is used for linear regression

abline(fit5) # Plot the regression line 

summary(fit5) # Find the summary of the estimated regression model

res5 <- residuals(fit5)
plot(res5 ~ education) # Plot the residuals

fit6 <- lm(log(income) ~ log(education)) # Find Nonlinear Model

plot(log(education),log(income))
abline(fit6) # Plot the model on transformed data

res6 <- residuals(fit6)
plot(res6 ~ log(education))





# Slide 30  Example 2
education <- c(12,9,18,10,18,12,18,17,14,8) # x values
income <- c(21000,18000,125000,50000,105000,35000,148000,109000,37000,33000) #y values

plot(education, income)

fit7<-lm(income ~ education)  ## lm is used for linear regression

abline(fit7)

summary(fit7)

res7 <- residuals(fit7)
plot(res7 ~ education) # Plot the residuals

fit7a <- lm(log(income) ~ log(education))

plot(log(education),log(income))
abline(fit7a) # Plot the model on transformed data

res7 <- residuals(fit7a)
plot(res7 ~ log(education))


# Slide 31

autoplot(uschange[,c("Consumption","Income")]) +
  ylab("% change") + xlab("Year")

# Slide 32

plot(Consumption ~ Income, data = uschange)
fit_uschange <-lm(Consumption ~ Income, data = uschange)
abline(fit_uschange)


uschange %>%
  as.data.frame() %>%
  ggplot(aes(x=Income, y=Consumption)) +
  ylab("Consumption (quarterly % change)") +
  xlab("Income (quarterly % change)") +
  geom_point() +
  geom_smooth(method="lm", se=FALSE)


# Slide 33
tslm(Consumption ~ Income, data=uschange)

