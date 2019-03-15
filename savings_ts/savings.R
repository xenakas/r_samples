
my_df <- read.csv(file="savings.csv", header=TRUE, sep=",")

sav <-  ts(data = my_df[4], start = c(1995,1), frequency = 4 ) 
ggtsdisplay(sav, main="") 
ggtsdisplay(diff(sav), main="") 
ggtsdisplay(diff(diff(sav),4), main="") 
ggtsdisplay(diff(diff(log(sav)),4), main="") 

plot(decompose(sav))

gdp <-  ts(data = my_df[2], start = c(1995,1), frequency = 4 ) 
ggtsdisplay(gdp, main="") 
ggtsdisplay(diff(gdp), main="") 
ggtsdisplay(diff(diff(gdp),4), main="") 
ggtsdisplay(diff(diff(log(gdp)),4), main="") 

spen <-  ts(data = my_df[3], start = c(1995,1), frequency = 4 ) 
ggtsdisplay(spen, main="") 
ggtsdisplay(diff(spen), main="") 
ggtsdisplay(diff(diff(spen),4), main="") 
ggtsdisplay(diff(diff(log(spen)),4), main="") 

inc <-  ts(data = my_df[5], start = c(1995,1), frequency = 4 ) 
ggtsdisplay(inc, main="") 
ggtsdisplay(diff(inc), main="") 
ggtsdisplay(diff(diff(inc),4), main="") 
ggtsdisplay(diff(diff(log(inc)),4), main="") 

wage <-  ts(data = my_df[6], start = c(1995,1), frequency = 4 ) 
ggtsdisplay(wage, main="") 
ggtsdisplay(diff(wage), main="") 
ggtsdisplay(diff(diff(wage),4), main="") 
ggtsdisplay(diff(diff(log(wage)),4), main="") 

cpi <-  ts(data = my_df[7], start = c(1995,1), frequency = 4 ) 
ggtsdisplay(cpi, main="") 
ggtsdisplay(diff(cpi), main="") 

exp <-  ts(data = my_df[8], start = c(1995,1), frequency = 4 ) 
ggtsdisplay(exp, main="") 
ggtsdisplay(diff(exp), main="") 

cor(my_df[2:8])

clean_df <- data.frame(diff(sav), diff(gdp), diff(spen), diff(inc), diff(wage), cpi[2:95], diff(exp)) 
cor(clean_df)

dflist <- c("spen", "inc", "wage", "cpi", "exp")

adf_list = list()
k=1

for (i in dflist){
  v <-get(i)
  adf_stat <- c()
  adf_stat <- c(ur.df(v)@teststat[1], ur.df(v)@cval[1], ur.df(v)@cval[2],ur.df(v)@cval[3])
  adf_list[[k]] <- adf_stat
  k <- k+1
}

for (i in dflist){
  v <-get(i)
  adf_stat <- c()
  adf_stat <- c(ur.df(diff(v))@teststat[1], ur.df(diff(v))@cval[1], ur.df(diff(v))@cval[2],ur.df(diff(v))@cval[3])
  adf_list[[k]] <- adf_stat
  k <- k+1
}

df_adf <- data.frame(matrix(unlist(adf_list), nrow=length(adf_list), byrow=T))

summary(ur.df(diff(sav)))
a <-  ur.df(diff(sav))
a@testreg$coefficients[1,4]
ur.df(sav)@teststat[1]
ur.df(sav)@cval[1]




