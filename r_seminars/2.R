del<-function(a,id,jd){
  b<-matrix(0, nrow=nrow(a)-1, ncol=ncol(a)-1)
  for(i in 1:ncol(b)){
    for(j in 1:ncol(b)){
      if(i<id){
        if(j<jd){
          b[i,j]<-a[i,j]
          }else{
            b[i,j]<-a[i,j+1]
            }
        }else{
          if(j<jd){
            b[i,j]<-a[i+1,j]
            }else{
              b[i,j]<-a[i+1,j+1]
            }
         }
      }
    }
  return(b)
}

deter<-function(x) {
  d<-0 
  if(dim(x)[1]==1) { 
    return(x[1,1]) 
    }else{ 
      if(dim(x)[1]==2) { 
        return(x[1,1]*x[2,2]-x[1,2]*x[2,1])
        }else{
          for (i in 1:(dim(x)[1])) { 
            mat<-del(x,1,i)
            d<-d+(x[1,i]*((-1)^(1+i))*deter(mat)) 
          }
        }
     } 
  return(d)
  } 

equation<-function(x,b){ 
  if(dim(x)[1]==dim(x)[2]) {
    detX<-deter(x)
    d<-c()
    if (detX==0){
      d<-"Нет решений или бесконечное множество решений"
      }else{
        for (j in 1:ncol(x)){
          matb<-x
          matb[,j] <- b
          k<-(deter(matb))/detX
          d<-c(d, k)
          }
        return(d)
        }
    }else{
    return("Матрица не квадратная")
  }
  
}


x<-matrix(c(3,2,1,2,-1,2,-5,3,-1), ncol=3, nrow=3)
b<-matrix(c(-1,13,9), ncol=1, nrow=3)
mat<-matrix(c(3,2,1,2,-1,2,-5,3,-1,-1,13,9), ncol=4, nrow=3)
d<-deter(x)
otv<-equation(x,b)

print(mat)
print(d)
print(otv)