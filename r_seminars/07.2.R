s<-matrix(sample(1:100), ncol=4, nrow=4)
print(s)
n<-nrow(s)
m<-ncol(s)

for (k in 1:(m-2)) {
  x<-s[,k]
  for (j in 2:n) {
    for (i in 2:n) {
      if (x[i-1]>x[i]) {
        z<-x[i-1]
        x[i-1]<-x[i]
        x[i]<-z
      }
    }
  }
  s[,k]<-x
}

k<-(m-1)
x<-s[,k]
y<-s[,m]

for (j in 2:n) {
  for (i in 2:n) {
    if (x[i-1]>x[i]) {
      z<-x[i-1]
      x[i-1]<-x[i]
      x[i]<-z
      z<-y[i-1]
      y[i-1]<-y[i]
      y[i]<-z
    }
  }
}
s[,k]<-x
s[,m]<-y

print(s)
