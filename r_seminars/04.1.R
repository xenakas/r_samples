x<-c(3,3,2,2,-1,0,1,0,0,1,-1,0)
c<-c()
z<-x[1]
j<-1
b<-length(x)
for (i in 1:b) {
  if (z>x[i]){
    z<-x[i]
    c<-c(i)
    }else{
      if (z==x[i]) {
        c<-c(c,i)
      }
    }
  }
print (x)
print (z)
print (c)

