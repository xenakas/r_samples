library(car)
library(lmtest)
library(reticulate)
library(normtest)

my_df <- read.csv(file="cs_savings.csv", header=TRUE, sep=",")

boxplot(my_df[1:4])
boxplot(my_df[c(5,6,11,12,13)])

sns <- import('seaborn')
plt <- import('matplotlib.pyplot')
pd <- import('pandas')
style <- import('matplotlib.style')

outlier_values <- boxplot.stats(my_df$sav)$out  
my_df <- my_df[-which(my_df$sav %in% c(outlier_values, 34684.9, 26796.9)),]

style$use('ggplot')
sns$pairplot(r_to_py(my_df))
plt$show()

summary(my_df)
cor(my_df)

Model1 <- lm(sav ~ grp+cons+inc+unemp+cpi+debt+ deprub  +   depcur    +     inv     +    emp    +     dem       +  migr,data = my_df)
summary(Model1)
vif(Model1) 

Model2 <- lm(sav ~ grp + cpi+    emp    +     dem +    migr,data = my_df)
summary(Model2)
vif(Model2)

resettest(Model2, power = 2)
bptest(Model2)
durbinWatsonTest(Model2)


Model <-  lm(log(sav) ~ log(grp) + cpi , data = my_df)
summary(Model)

vif(Model)

resettest(Model, power = 2)

bptest(Model)
durbinWatsonTest(Model)
df_check <-  data.frame(grp = log(my_df$grp),  cpi=my_df$cpi)
cov(resid(Model),df_check)

mean(resid(Model))
hist(resid(Model))
plot(resid(Model))

library(boot)
Modelboot <- Boot(Model, R=1999)
hist(Modelboot, legend="separate")

Confint(Modelboot, level=.95, type="norm")
confint(Model)


jb.norm.test(resid(Model))

library(forecast)
checkresiduals(resid(Model))


