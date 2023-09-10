#Slide 14
library(readxl)

Salary <- read_excel("Salary.xlsx")

#Step1
fit <- lm(Income~ Education+Emplo+Age+Fam+Res, data=Salary)

#Step2
fit1 <- lm(Income~ Emplo+Age+Fam+Res, data=Salary)
fit2 <- lm(Income~ Education+Age+Fam+Res, data=Salary)
fit3 <- lm(Income~ Education+Emplo+Fam+Res, data=Salary)
fit4 <- lm(Income~ Education+Emplo+Age+Res, data=Salary)
fit5 <- lm(Income~ Education+Emplo+Age+Fam, data=Salary)

#Step3 Income~ Education+Emplo+Age+Fam, data=Salary)
fit6 <- lm(Income~ Emplo+Age+Fam, data=Salary)
fit7 <- lm(Income~ Education+Age+Fam, data=Salary)
fit8 <- lm(Income~ Education+Emplo+Fam, data=Salary)
fit9 <- lm(Income~ Education+Emplo+Age, data=Salary)



library(MASS)

# backward step regression using AIC
fit_all <- lm(Income~ Education+Emplo+Age+Fam+Res, data=Salary)
step.model <- stepAIC(fit_all, direction = "backward", 
                      trace = TRUE)

summary(step.model)


# forward step regression using AIC
fit_none <- lm(Income ~ 1, data=Salary)
library(MASS)
step.model2 <- stepAIC(fit_none, direction = "forward", 
                       trace = TRUE, scope=list(lower=fit_none, upper=fit))

summary(step.model2)

head(DATA1)

lstat=DATA1[,1]
medv=DATA1[,2]

plot(lstat, medv, col = "blue", main = "", xlab = "lstat", ylab = "medv")

plot(DATA1, col = "blue", main = "", xlab = "lstat", ylab = "medv")


abline(fit1)
#You can see other statistics
summary(fit1)


res<-residuals(fit1)
plot(jitter(res)~jitter(lstat),data=DATA1)

#4 and 5 For given x=15, what is the corresponding y?

predictions<-forecast(fit1,data.frame(lstat=c(15,20)))
predictions<-predict(fit1,data.frame(lstat=c(15,20)))

# slide 21

# Load the MASS library
library(MASS)

# Download the data called Boston
Boston
View(Boston)
# Find the dimension of the Boston data
dim(Boston)

# Figure out the names of the columns
names(Boston)

#1 Choose the first and 13th and 14th columns of the data
New_DATA3=Boston[,c(1,13,14)]
