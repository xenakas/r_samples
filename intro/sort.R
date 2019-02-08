#### sort1

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

# sort2

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


#### sort by column

s1<-s<-matrix(sample(1:100), ncol=4, nrow=4)
print(s)
n<-nrow(s)
m<-ncol(s)

for (k in 1:m) {
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
print(s)
print(s1)


