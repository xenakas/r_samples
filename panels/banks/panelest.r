library(gplots)
library(plm)
library(dplyr)
library(ggplot2)
library(car)  
library(tseries)  
library(lmtest)

df_all  <-  read.csv(file="dataframe.csv", header=TRUE, sep=",")

df_all['year'][df_all['year'] == 2018, ] = 1 
df_all['year'][df_all['year'] == 2019, ] = 2

df_all['pok_type'] = paste(df_all[['pok']], df_all[['type']])
df_all['group'] = paste(df_all[['sex']], df_all[['age']]) 

list_of_pok  <-  list()
un_pok  <- unique(df_all['pok_type'])  
for (i in 1:dim(un_pok)[1]){
    list_of_pok[[i]]  <-  df_all  %>%  filter(pok_type == un_pok[i,1])    
}

df  <-  list_of_pok[[1]]
df = pdata.frame(df, index = c('group','date'))
df['value_lagged'] = lag(df$value, 1)
df['value_diff'] = diff(df$value, 1)
head(df)

coplot(value ~ date|group, type="b", data=df)

coplot(value_diff ~ date|group, type="b", data=df)

plotmeans(value ~ date, data = df)

m_pooled <- plm(value~sex+age+age_sq+inc+year, data = df, model='pooling')
summary(m_pooled)

m_fixed <- plm(value~sex+age+age_sq+inc+year, data = df, model='within')
summary(m_fixed)

summary(fixef(m_fixed, type = "dmean"))

m_random <- plm(value~sex+age+age_sq+inc+year, data = df, model='random')
summary(m_random)

pFtest(m_fixed, m_pooled)

phtest(m_fixed, m_random)

plmtest(m_random, effect="individual", type="bp")
# plmtest(m_random, effect="time", type="bp")

# pcdtest(m_random, test = c("lm"))
# pcdtest(m_random, test = "cd")
# pbgtest(m_random, order=3)
# adf.test(df$value, k=1)

bptest(m_random, data = df, studentize=F)

coeftest(m_random) 

# coeftest(m_random, vcovHC(m_random, method="white2", type="HC3"), df = Inf) 
coeftest(m_random, vcovHC(m_random, method="white2", type="HC3")) 

test_df  <-  data.matrix(matrix(NA,ncol = 5, nrow = length(list_of_pok)))
colnames(test_df)  <-  c('Показатель', 'PvsFE', 'REvsFE', 'PvsRE', 'BPforHC')
for (i in 1:length(list_of_pok)){
    df  <-  list_of_pok[[i]]
    test_df[i,1]  <- df[1,'pok_type']
    df  <-  pdata.frame(df, index = c('group','date'))
    m_pooled <- plm(value~sex+age+age_sq+inc+year, data = df, model='pooling')
    m_fixed <- plm(value~sex+age+age_sq+inc+year, data = df, model='within')
    m_random <- plm(value~sex+age+age_sq+inc+year, data = df, model='random')
    t1  <-  pFtest(m_fixed, m_pooled)
    test_df[i,2]  <- t1$p.value
    t2  <-  phtest(m_fixed, m_random)
    test_df[i,3]  <- t2$p.value
    t3  <-  plmtest(m_random, effect="individual", type="bp")
    test_df[i,4]  <- t3$p.value
    t4  <- bptest(m_random, data = df, studentize=F)
    test_df[i,5]  <- t4$p.value

}


test_df

cf1  <- coeftest(m_random) 
coef_df  <-  data.matrix(matrix(NA,nrow = 6, ncol = length(list_of_pok)))
pvalue_df  <-  data.matrix(matrix(NA,nrow = 6, ncol = length(list_of_pok)))
pvalue_hc_df  <-  data.matrix(matrix(NA,nrow = 6, ncol = length(list_of_pok)))
colnames(coef_df)  <- colnames(pvalue_df)  <-  colnames(pvalue_hc_df)  <-  test_df[,1]
rownames(coef_df)  <- rownames(pvalue_df)  <-  rownames(pvalue_hc_df)  <-  rownames(cf1)

for (i in 1:length(list_of_pok)){
    df  <-  list_of_pok[[i]]
    test_df[i,1]  <- df[1,'pok_type']
    df  <-  pdata.frame(df, index = c('group','date'))
    m_random <- plm(value~sex+age+age_sq+inc+year, data = df, model='random')
    cf1  <- coeftest(m_random) 
    coef_df[,i]  <- cf1[,1]
    pvalue_df[,i]  <- cf1[,4]
    cf2  <- coeftest(m_random, vcovHC(m_random, method="white2", type="HC3")) 
    pvalue_hc_df[,i]  <- cf2[,4]
}

coef_df

pv  <- pvalue_df
pv[pvalue_df>=0.1]  <-  ''
pv[pvalue_df<0.1] <-  '.'
pv[pvalue_df<0.05] <-  '*'
pv[pvalue_df<0.01] <-  '**'
pv[pvalue_df<0.001] <-  '***'

pv1  <- pvalue_hc_df
pv1[pvalue_hc_df>=0.1]  <-  ''
pv1[pvalue_hc_df<0.1] <-  '.'
pv1[pvalue_hc_df<0.05] <-  '*'
pv1[pvalue_hc_df<0.01] <-  '**'
pv1[pvalue_hc_df<0.001] <-  '***'

cpv  <-  rbind(coef_df, pv)
cpv_hc <-  rbind(coef_df, pv1)

cpv[c(1,7,2,8,3,9,4,10,5,11,6,12),]
cpv_hc[c(1,7,2,8,3,9,4,10,5,11,6,12),]
