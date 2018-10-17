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

