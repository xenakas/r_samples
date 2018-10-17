x<-sample(1:200,20)
n<-length(x)
print(x)
for (j in 2:n) {
  for (i in 2:n) {
    if (x[i-1]>x[i]) {
      z<-x[i-1]
      x[i-1]<-x[i]
      x[i]<-z
    }
  }
}
print(x)