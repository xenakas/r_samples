x <- 0 
optperebor <- 0 
bil<-c()

for(a in 0:9){ 
  for(b in 0:9){ 
    for(c in 0:9){ 
      for(d in 0:min((a+b+c),9)){ 
        for(e in 0:min((a+b+c-d),9)){ 
          optperebor <- optperebor+1 
          f <- a+b+c-d-e 
          if(f<=9){ 
            x <- x+1 
            bil<-c(bil,a*100000+b*10000+c*1000+d*100+e*10+f)
            } 
          }
       }
     }
  }
}

x 
optperebor

print(paste('—частливый билет є', 30331, ' - ', bil[30331] ))