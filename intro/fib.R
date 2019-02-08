fib<-function(k){
  x=numeric(k)
  x[1:2]=c(1,1)
  for(i in 3:k){
    x[i]=x[i-1]+x[i-2]
    }
  return(x)
  }

k<-100
m<-fib(k)
s<-(m[k]/m[k-1])
print(s) 

gold<-function(a,b,e){
  x<-b-(b-a)/s
  y<-a+(b-a)/s
  while(abs(b-a)>=e){
    if(f(x)>=f(y)){
      a<-x
      x<-y
      y<-a+(b-a)/s
    }else{
    b<-y
    y<-x
    x<-b-(b-a)/s
    } 
  }
  x<-(a+b)/2
  return(x)
}

f<-function(x){
  y<-(x-40)^8
  return(y)
}
a<--34
b<-200
e<-0.0001

print(gold(a,b,e))
 
