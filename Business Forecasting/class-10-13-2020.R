library(fpp)
library(fpp2)


#Slide 6

library(readxl)
Salary <- read_excel("Salary.xlsx")
View(Salary)

fit <- lm(Income~ Education+Emplo+Age+Fam+Res, data=Salary)

# Slide 12

cv(fit)

# Slide 14
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

# slide 15
fitted(fit9)
residuals(fit9)


# slide 18
fit5 <- lm(Income~ Education+Emplo+Age+Fam, data=Salary)#0.95

forecast(fit5, newdata=data.frame(Education=c(18),Age=c(60),Emplo=c(3),Fam=c(4),Res=c(850)))

forecast(fit5,data.frame(Education=c(18,15,12),Age=c(60,45,30),Emplo=c(3,7,2),Fam=c(4,5,8),Res=c(850,750,800)))

# we can also use predict function
fcast <- predict(fit5, newdata=data.frame(Education=c(18),Age=c(60),Emplo=c(3),Fam=c(4),Res=c(850)))
