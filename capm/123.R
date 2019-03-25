library(lmtest)
library(dplyr)
library(broom)
library(ggplot2)
library(forecast)
library(strucchange)
library(tseries)
library(readxl)

my_df <- read.csv(file="C:/Users/Ksy/Desktop/ge_data.csv", header=TRUE, sep=",")
my_df <- mutate(my_df, Diff_p = GE_Return - Risk_Free, Diff_m = SP_Return - Risk_Free)
glimpse(my_df)
summary(my_df)
head(my_df)


R_p <- ts(my_df$GE_Return, start = c(2000,1), frequency = 252)
R_m <- ts(my_df$SP_Return, start = c(2000,1), frequency = 252)


ts.plot(ts(my_df$GE_Close, start = c(2000,1), frequency = 252),  gpars=list(xlab="????????", ylab="???????? ?????????? General Electric") )

help("ts.plot"
     )

ggtsdisplay(R_p)
checkresiduals(R_p)
auto.arima(R_p)


ggtsdisplay(R_m)
checkresiduals(R_m)
auto.arima(R_m)


adf.test(my_df$Diff_p, alternative = 'stationary')
adf.test(my_df$Diff_m, alternative = 'stationary')


camp <- lm(data=my_df, Diff_p ~ Diff_m)
summary(camp)



plot(efp(capm,   data=my_df)) 


apt <- lm(data=my_df, GE_Return ~ SP_Return + Risk_Free +
                  CPI + ConsExp + DJ + Capitalization)
summary(apt_model)


ggplot(schneider, aes(x=rm, y=ri)) + geom_point() + geom_smooth()


