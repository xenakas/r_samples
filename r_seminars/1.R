mean<-function(x){ 
  return(sum(x)/length(x))
} 

median<-function(x){ 
  x<-sort(x) 
  n<-length(x)
  if(n%%2=0){
  return((x[n/2]+x[n/2+1])/2)
    }else{
      return(x[ceiling(n/2)])
    }
  } 

dispersion<-function(x){ 
  p<-0 
  d<-0
  for(i in 1:length(x)){ 
    p<-(x[i]-mean(x))^2/(length(x)-1)
    d<-(d+p)
  } 
  return(d) 
} 

deviance<-function(x){ 
  return(dispersion(x)^(1/2)) 
} 

firstQu<-function(x){ 
  x<-sort(x) 
  firstQu<-median(x[1:median(x)]) 
  return(firstQu)
} 

thirdQu<-function(x){ 
  x<-sort(x) 
  n<-length(x)
  thirdQu<-median(x[median(x):n]) 
  return(thirdQu)
} 

summary<-function(x){ 
  a<-paste("NA's:",length(x)-length(na.omit(x))) 
  x<-na.omit(x) 
  b<-paste("Mean:",mean(x))  
  c<-paste("Median:",median(x))  
  d<-paste("Dispersion:",dispersion(x)) 
  e<-paste("Deviance:",deviance(x)) 
  f<-paste("1st Qu:",firstQu(x))
  g<-paste("3rd Qu:",thirdQu(x))
  h<-paste("Min:",min(x))
  i<-paste("Max:",max(x)) 
  return(c(a,b,c,d,e,f,g,h,i))
} 

correlation<-function(x,y){ 
  a<-0 
  b<-0 
  c<-0
  for(i in 1:length(y)){ 
    a<-a+(x[i]-mean(x))*(y[i]-mean(y))
    c<-c+(y[i]-mean(y))^2
  } 
  for(i in 1:length(x)){ 
    b<-b+(x[i]-mean(x))^2 
  } 
  d<-a/((b*c)^(1/2))
  return(d) 
}

getmonitor <- function(id, directory, summarize = FALSE) { 
  id<-as.integer(id) 
  if (id < 100) {
    if (id < 10) {
      id <- paste('00', id, sep = '')    
    } else {
      id <- paste('0', id, sep = '')    
    }
  }
  n<-length(id)
  x<-read.csv(file=paste("./specdata/",id,".csv",sep = ''), header=T, sep=",", dec='.')
  if (summarize==TRUE) { 
    a <- data.frame(sulfate=summary(x[,2]),nitrate=summary(x[,3])) 
    return(a)
  }else{ 
    return(x) }
}

complete <- function(directory, id = 1:332) {          #получение сведений о количестве полных наблюдений для заданного списка
  x <- data.frame('id' = id, 'nobs' = c(1:length(id)))
  y <- 1
  for (i in id) {
    z <- getmonitor(i, directory)
    not_na <- sum(!is.na(z[,2]) & !is.na(z[,3]))
    x[y,] <- c(i, not_na)
    y <- y + 1
  }
  return(x)
}

corr <- function(directory, threshold = 0){            #threshold - вектор длины 1 - пороговое значение для требуемого количества полных наблюдений
  x <- c() 
  for (i in 1:332){ 
    y <- getmonitor(i, directory)
    z <- complete(directory,i)
    if(z[1,2]>=threshold){ 
      u <- c(!is.na(y[,2]) & !is.na(y[,3]))
      x <- c(x, correlation(y[u,2], y[u,3]))
    } 
  }
  return(x) 
}   

setwd("C:/Users/Ksy/Desktop")

data <- getmonitor(101.00, "specdata", TRUE)
otvet <- corr("specdata") 

complete("specdata", 30:25)
print(data)
summary(otvet)
