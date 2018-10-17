x<--sample(1:100,10)
print(x)
b<-length(x)
for (j in 0:(b-1)) {
  z<-x[1]
  for (i in 1:(b-j)) {
    if (z<=x[i]){
      z<-x[i]
      c<-i
      }
    }
  k<-x[b-j]
  x[b-j]<-x[c]
  x[c]<-k
  }
print(x)


