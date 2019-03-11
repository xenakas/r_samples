install.packages('car')
install.packages('lmtest')

library(car)
library(lmtest)

regr1<- read.table('Regr.txt', header=T, sep="")
boxplot(regr1$Cena)
boxplot(regr1$Comnata,regr1$Etag,regr1$Obshplosh,regr1$Jilplosh,regr1$Ploshcuh)

regr2 <- read.table('regr1.txt', header=T, sep="")
plot(regr2$Comnata,regr2$Cena)
plot(regr2$Etag,regr2$Cena)
plot(regr2$Obshplosh,regr2$Cena)
plot(regr2$Jilplosh,regr2$Cena)
plot(regr2$Ploshcuh,regr2$Cena)

regr3 <- read.table('regr2.txt', header=T, sep="")
summary(regr3)
hist(regr3$Cena, freq = FALSE, col = "blue")
hist(regr3$Obshplosh, freq = FALSE, col = "yellow")
hist(regr3$Jilplosh, freq = FALSE, col = "pink")
hist(regr3$Ploshcuh, freq = FALSE, col = "brown")
cor(regr3)

Model1 <- lm(Cena ~ Ploshcuh+Jilplosh+Obshplosh+Comnata+Etag,data = regr3)
summary(Model1)
vif(Model1) 

Model2 <- lm(Cena ~ Ploshcuh+Jilplosh+Comnata+Etag,data = regr3)
summary(Model2)
vif(Model2)

Model3 <- lm(Cena ~ Ploshcuh+Jilplosh,data = regr3)
summary(Model3)
vif(Model3)

resettest(Model3, power = 2)

mean(resid(Model3))
hist(resid(Model3))
bptest(Model3)
cov(resid(Model3),regr)

plot(resid(Model3))

durbinWatsonTest(Model3)
shapiro.test(resid(Model3))
qqPlot(Model3, labels = row.names(states), simulate = TRUE, main = 'Q-Q')

regr <- read.table('regr3.txt', header=T, sep="")
Model <- lm(lnCena ~ Ploshcuh+Jilplosh,data = regr)
summary(Model)

resettest(Model)
mean(resid(Model))
bptest(Model)
durbinWatsonTest(Model)
cov(resid(Model),regr)
shapiro.test(resid(Model))
plot(resid(Model))

qqPlot(Model, labels = row.names(states), simulate = TRUE, main = 'Q-Q')








