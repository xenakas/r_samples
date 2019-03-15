library(car)
library(lmtest)
library(normtest)

my_df <- read.csv(file="savings.csv", header=TRUE, sep=",")

boxplot(my_df[1:4])
boxplot(my_df[c(5,6,11,12,13)])

outlier_values <- boxplot.stats(my_df$sav)$out  
my_df <- my_df[-which(my_df$sav %in% c(outlier_values, 34684.9, 26796.9)),]

summary(my_df)
cor(my_df)

Model1 <- lm(sav ~ grp+cons+inc+unemp+cpi+debt+ deprub  +   depcur    +     inv     +    emp    +     dem       +  migr,data = my_df)
summary(Model1)
vif(Model1) 

Model2 <- lm(sav ~ grp+unemp+cpi+ emp + dem +  migr,data = my_df)
summary(Model2)
vif(Model2) 
resettest(Model2, power = 2)
durbinWatsonTest(Model2)

Model <-  lm(log(sav) ~ log(grp) + cpi , data = my_df)
summary(Model)
vif(Model)
resettest(Model, power = 2)
mean(resid(Model))
bptest(Model)
df_check <-  data.frame(grp = log(my_df$grp),  cpi=my_df$cpi)
cov(resid(Model),df_check)
durbinWatsonTest(Model)
jb.norm.test(resid(Model))
plot(resid(Model))
hist(resid(Model))




