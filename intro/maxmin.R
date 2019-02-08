### max/min

x<-sample(1:100,16)
imx<-1
mx<-x[1]
imn<-1
mn<-x[1]
b<-length(x)
for (i in 1:b) {
  if (mx<=x[i]){
    mx<-x[i]
    imx<-i
  } else {
    if (mn>=x[i]){
      mn<-x[i]
      imn<-i}
  }
} 
print (x)
print (mx)
print (imx)
print (mn)
print (imn)

### all min

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

