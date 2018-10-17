xt<-read.table('http://leonovmx.github.io/info/s1/fx.csv',header=T,sep=";")
d<-10
cmi<-1
cma<-1
xt[,1]<-1:3736
x<-xt[,2]
zmi<-x[1]
zma<-x[1]  
plot(xt,type="l")
for (i in 1:3736) {
  if (i>d) {
    for (j in (i-d):(i+d)) {
    if (zma<x[j]){
      zma<-x[j]
      cma<-j
    } else {
      if (zmi>x[j]){
        zmi<-x[j]
        cmi<-j}
    }
      if (cma==i) {abline(v = cma, col = "green")} 
      if (cmi==i) {abline(v = cmi, col = "red", lty = 2)}
    }
  }
}
