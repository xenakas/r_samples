n<-20
a<-sample(1:100,n)
b<-sample(1:100,n)
print(a)
print(b)

for (j in 2:n) {
  for (i in 2:n) {
    if (a[i-1]>a[i]) {
      z<-a[i-1]
      a[i-1]<-a[i]
      a[i]<-z
      }
    if (b[i-1]>b[i]) {
      z<-b[i-1]
      b[i-1]<-b[i]
      b[i]<-z
      }
    }
}


na<-matrix(, ncol=2, nrow=n)
na[,1]<-a
na[,2]<-b

ni<-matrix(, ncol=2, nrow=n)
ni[,1]<-a

sa<-0
si<-0
for (i in 1:n) {
ni[i,2]<-b[n-i+1]
pa<-a[i]*b[i]
pi<-a[i]*b[n-i+1]
sa<-sa+pa
si<-si+pi
}

print(na)
print(sa)
print(ni)
print(si)
