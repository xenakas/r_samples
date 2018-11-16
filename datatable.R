library(data.table) 
library(dplyr) 


?read.csv
csv <- read.csv('/home/xenakas/Desktop/Git/r_projects/boxjenkins/gdp.csv', stringsAsFactors = FALSE,  na.strings = "#N/A", header = FALSE) 

csv$date_of_birth = sample(seq(as.Date('1920/01/01'), as.Date('2000/01/01'), by="day"), 192)
csv$gender = sample(c('male', 'female'), 192, replace=TRUE)
csv$exp_by_1996 = sample.int(50, 192, replace = TRUE)


#1 Calculate missing values
#loops 
nas <- c()
for (col in 1:length(names(csv))){ 
  nas <- c(nas, length(csv[is.na(csv[,col]),col]) )
  cat(c('There are', nas[col], 'missing values in column ', col, '\n' ))
} 
nas

#lapply 
sapply(names(csv), function(x) length(csv[is.na(csv[,x]),x])) 

#data.table 
csv_dt =  as.data.table(csv)
csv_dt[, lapply(.SD, function(x) sum(is.na(x))), .SDcols = 1:length(names(csv_dt))] 


#2 Calculate mean grouped by 
#loop
means = c()
csv$year_of_birth =format(as.Date(csv$date_of_birth, format="%d/%m/%Y"),"%Y")
for (year in unique(csv$year_of_birth)) { 
  for (gend in unique(csv$gender)){ 
    exper = csv %>% filter((csv$gender == gend) & (csv$year_of_birth == year))
    means = rbind(means, c(gend, year, mean(exper$exp_by_1996, na.rm = TRUE))) 
    } 
} 
means

#tapply 
mean <-tapply(csv$exp_by_1996, list(csv$gender, csv$date_of_birth), mean) 


# data.table 
csv_dt[ , year_of_birth := format(as.Date(date_of_birth, format="%d/%m/%Y"),"%Y") ] 
csv_dt[ , mean_exp := mean(exp_by_1996), by = c('gender', 'date_of_birth')] 

