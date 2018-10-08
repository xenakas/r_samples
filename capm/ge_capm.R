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





ts.plot(ts(my_df$GE_Close, start = c(2000,1), frequency = 252))



ggtsdisplay(R_p)
auto.arima(R_p)


ggtsdisplay(R_m)
auto.arima(R_m)


adf.test(my_df$Diff_p, alternative = 'stationary')
adf.test(my_df$Diff_m, alternative = 'stationary')



ggplot(my_df, aes(x=R_m, y=R_p)) + geom_point() + geom_abline(intercept = 0.006*(1-1.1116800)+0.0004171 , slope = 1.1116800 )     



capm <- lm(data=my_df, Diff_p ~ Diff_m)
summary(capm)

AIC(capm)


capm_0 <- lm(data = my_df, Diff_p ~ Diff_m + 0)
summary(capm_0)


AIC(capm_0)

sctest(capm, type = "Chow")


# cusum
plot(efp(capm,   data=my_df))

#APT model

apt_model <- lm(data=my_df, R_p ~ R_m + Risk_Free + GE_DTVolume +
                  CPI + Oil_Change + GDP_Growth + SMB + HML)
summary(apt_model)




ggplot(my_df, aes(x=R_m, y=R_p)) + geom_point() + geom_smooth()


