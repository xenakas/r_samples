library(lmtest)
library(dplyr)
library(broom)
library(ggplot2)
library(forecast)
library(strucchange)
library(tseries)
library(readxl)
library(sophisthse)
library(urca)
library(uroot)
library(TSA)
library(car)
library(zoo)
library(vars)
#########################################################

gdp1 <- sophisthse("GDP_Q_I")
gdp2 <- sophisthse("GDPEA_Q")
gdp <-  ts(data = c(gdp1[25:36, "GDP_Q_DIRI"], gdp2[1:61, "GDPEA_Q_DIRI"]), start = c(2000,1), frequency = 4 ) 
?ggtsdisplay
ggtsdisplay(gdp, main="") 

inv1 <- sophisthse("INVFC_M") #investment in K monthly
inv <- ts(data =   inv1[73:264, "INVFC_M"] , start = c(2000,1), frequency = 12 ) 
ggtsdisplay(inv, main="") 

lab1 <- sophisthse("EMPLDEC_M") #investment in K monthly
lab <- ts(data =   lab1[73:292, "EMPLDEC_M"] , start = c(2000,1), frequency = 12 ) 
ggtsdisplay(lab, main="")

summary(ur.df(gdp))
summary(ur.df(inv))
summary(ur.df(lab))

summary(ur.kpss(gdp))
summary(ur.kpss(inv))
summary(ur.kpss(lab))


inv_log <- log(inv)
ggtsdisplay(inv_log, main="")
inv_diff <- diff(inv_log)
ggtsdisplay(inv_diff, main="")
summary(ur.kpss(inv_diff))


lab_log <- log(lab)
ggtsdisplay(lab_log, main="")
lab_diff <- diff(lab_log)
ggtsdisplay(lab_diff, main="")
summary(ur.kpss(lab_diff))


ch.test(gdp)
ggseasonplot(gdp)  
gdp_diff <- diff(gdp,4)
ggtsdisplay(gdp_diff)
ggseasonplot(gdp_diff)  
ch.test(gdp_diff)


ch.test(inv_diff)
ggseasonplot(inv_diff)
inv_diff12 <- diff(inv_diff,12)
ggseasonplot(inv_diff12)  
ggtsdisplay(inv_diff12)
ch.test(inv_diff12)


ch.test(lab_diff)
ggseasonplot(lab_diff)
lab_diff12 <- diff(lab_diff,12)
ggseasonplot(lab_diff12)
ggtsdisplay(lab_diff12)
ch.test(lab_diff12)


#ARIMA(1,0,3)(0,1,0)[4] with drift 
model_gdp <- Arima(gdp, order = c(2,0,0), seasonal = c(0,1,0),  include.drift=T, include.constant = T, method = "ML")
summary(model_gdp)
checkresiduals(model_gdp)
future_gdp <- forecast(model_gdp, h = 8)
autoplot(future_gdp)


#ARIMA(2,0,0)(1,1,0)[12] with drift 
model_inv <- Arima(inv, order = c(2, 1, 0), seasonal = c(1, 1, 0), lambda = 0 )
summary(model_inv)
checkresiduals(model_inv)
future_inv <- forecast(model_inv, h = 24)
autoplot(future_inv)

#ARIMA(2,0,2)(0,1,2)[12] with drift 
model_lab <- Arima(lab, order = c(2, 0, 2), seasonal = c(0, 1, 2), lambda = 0,  include.drift = T )
summary(model_lab)
checkresiduals(model_lab)
future_lab <- forecast(model_lab, h = 24)
autoplot(future_lab)







# Тесты на гетероскедастичность
??McLeod.Li.test
McLeod.Li.test(model_gdp, gdp)
McLeod.Li.test(model_inv, inv)
McLeod.Li.test(model_lab, lab)


# Тест на автокоррелированность остатков
?Box.test
Box.test(model_gdp$residuals, type = "Ljung-Box")
Box.test(model_inv$residuals, type = "Ljung-Box")
Box.test(model_lab$residuals, type = "Ljung-Box")


# Тест на нормальность остатков
?jarque.bera.test

n <- rnorm(2500, mean=0, sd=4.58)
jarque.bera.test(n)
jarque.bera.test(model_gdp$residuals)
jarque.bera.test(model_inv$residuals)
jarque.bera.test(model_lab$residuals)




##############################################
my_df <- read.csv(file="C:/Users/Ksy/Desktop/gdp.csv", header=F, sep=",")
a <- my_df[,2]
jogdp<-  ts(data = my_df[,2] , start = c(2000,1), frequency = 12 )
#jogdp<- diff(jogdp,12)
joinv<-  ts(data = inv , start = c(2000,1), frequency = 12 ) 
#joinv <- diff(diff(log(joinv)),12)
jolab<- ts(data = lab[1:192] , start = c(2000,1), frequency = 12 ) 
#jolab <- diff(diff(log(jolab)),12)


?ca.jo
jo<-ca.jo(data.frame(jogdp,joinv,jolab), type="trace", K=2, ecdet="none", spec="longrun")
summary(jo)



###################################################
df<-cbind(jogdp,joinv,jolab)
#df[1,2]<-	  -0.0421790917
#df[1,3]<-	  -2.610411e-02



?VARselect
VARselect(df, lag.max=8, type="trend", season = 12)$selection
var <- VAR(df, p=3, type="trend", season=12, ic="SC")

?serial.test
serial.test(var, lags.pt=10, type="PT.asymptotic")
serial.test(var, lags.bg=10, type="BG")




future_var <- forecast(var,24)
plot(future_var)


summary(var)




