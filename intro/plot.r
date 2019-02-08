fun<-function(x) {
  return(sin(sqrt(x[1]^2+x[2]^2))/sqrt(x[1]^2+x[2]^2))
  }

fungraph<-function(x,y) {
  return(sin(sqrt(x^2+y^2))/sqrt(x^2+y^2))
  }

hg<-function(f,graph,a,e,d) {
  
  par(mfcol=c(1,2))
  n<-50
  x<-y<-seq(-10,10,len=n)
  z<-outer(x,y,graph)
  
  persp(x,y,z)
  contour(x,y,z)
  points(a[1],a[2],col="red",pch=19)
  
  
  while(d>e) {
    dy<-c(0,d)
    dx<-c(d,0)
    a1<-a
    
    if (f(a)>f(a+dy)) {
      a1<-a+dy
      }
    else {
      if (f(a)>f(a-dy)) {
        a1<-a-dy
        } 
      else {
        if (f(a)>f(a-dx)) {
          a1<-a-dx
          } 
        else
          if (f(a)>f(a+dx)) {
            a1<-a+dx
          }        
        }
      }
    
    if (all(a1==a)) {
      d<-d/2
      } 
    else {
      points(a1[1],a1[2],pch=19);
      segments(a[1],a[2],a1[1],a1[2]);
      a2<-2*a1-a
      
      while (f(a2)<f(a1)){
        points(a2[1],a2[2],pch=19);
        segments(a1[1],a1[2],a2[1],a2[2]);
        a<-a1;
        a1<-a2;
        a2<-2*a1-a
      }
     a<-a1 
    }
  }
  return(c(a,f(a)))
}

hg(fun,fungraph,c(1,2),0.0001,1)