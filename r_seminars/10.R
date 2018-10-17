
newGraph<-function(n){                              #создание графа   
  G <- matrix(0,n,n) 
  return(G)  
  } 

removeEdge<-function(G,a1,a2){                      #удаление дуги    
  if((a1<=n)&(a2<=n)&(a1>0)&(a1>0)) { 
    G[a1,a2]<-0 
    G[a2,a1]<-0 
    } else { 
    print("ƒуга не принадлежит графу") 
    } 
  return(G)
  } 

addEdge<-function(G,a1,a2){                        #добавление дуги
  if((a1<=n)&(a2<=n)&(a1>0)&(a1>0)) { 
      G[a1,a2]<-1 
      G[a2,a1]<-1 
      }else{
      print("ƒуга не принадлежит графу") 
      }
  return(G)
  } 

wave<-function(G,k){                               #уровни по волновому алгоритму дл€ вершины k
  w<-rep(-1,n)
  if(G[k,k]!=0){
    w[k]<-1
      }else{
        w[k]<-0
        }
  L<-c(k)                                          #массив с предыдущим уровнем
  N<-c()                                           #массив нового уровн€
  S<-1                                             #начальный уровень 1
  while (length(L)>0){                             
    for(i in 1:length(L)){
      for(j in 1:length(w)){                      
        if (G[L[i],j]>0 & w[j]==-1){               #если существует ребро, в которое можно перейти из i, и конец не зад.вершина
          w[j]<-S                                  #присваиваем уровень
          N<-c(N,j)                                #добавл€ем конечное ребро в массив уровн€
          }
        }
      }
    if (length(N)==0){                             #если не куда больше переходить, выдать w
      return(w)
      }
    L<-N                                           #L - новое конечное ребро 
    N<-c()                                         
    S<-S+1                                         #переходим на следующий уровень
    }
  return(w)
}

adj<-function(G,v1){                               #поиск смежных вершин
  A<-c() 
  for(i in 1:n){                                   #если существует ребро, добавл€ем смежную вершину в пустой массив 
    if(G[v1,i]>0){ 
      A<-c(A,i) 
      }
    } 
  return(A) 
  }

twotops<-function(G,v1,v2){                        #кратчайший путь по волновому алгоритму
  L<-wave(G,v1)                                    #уровни дл€ начальной вершины
  L[v1]<-0
  K<-c(v2)                                         #конечна€ вершина
  k<-n
  for (i in 1:(n-1)){
    for (j in 1:n){
      if (L[j]==(L[v2]-1)){
        if (G[j,v2]>0){
          K[i+1]<-j
          k<-j
          }
        }
      }
    v2<-k
  }
  N<-which(is.na(K))
  if(length(N)==0 & length(K)!=1){
  return(K)}else{return(Inf)}
}

cycle<-function(G,k1,k2,l1,l2){                     #поиск цикла 
  if(G[k1,k2]>0 & G[l1,l2]>0){                      #если оба ребра существуют 
    G<-removeEdge(G,k1,k2)                              
    G<-removeEdge(G,l1,l2)                          #удал€ем оба ребра      
    D<-wave(G,l1)                                   #уровни по волновому алгоритму в графе без заданных ребер
    s<-c()
    for(i in 1:length(D)){                          #поиск разрывов
    if(D[i]==-1){
      s=c(s,i)
      }
    }
    if(k1==l1){ 
      A1<-twotops(G,k2,l2)
      return(c(A1,l1))
      }else{
        if(k1==l2){
          A1<-twotops(G,k2,l1)
          return(c(A1,l2))
            }else{
              if(k2==l1){
              A1<-twotops(G,k1,l2)
              return(c(A1,l1))
                }else{
                if(k2==l2){
                  A1<-twotops(G,k1,l1)
                  return(c(A1,l2))
                    }else{
                    if(length(s)>0){   
                       G<-addEdge(G,k1,k2)
                       A1<-twotops(G,l1,l2)
                       B1<-c(which(A1[]==k1),which(A1[]==k2))
                       if(length(B1)==2){
                         return(A1)
                          }else{
                            print("Ќевозможно создать минимальный цикл, содержащий два заданных ребра")
                            }
                       }else{                         #если разрывов нет, определим кратчайший из циклов
                          A1<-twotops(G,k1,l2)
                          A2<-twotops(G,k1,l1)
                          A3<-twotops(G,l1,k2)
                          A4<-twotops(G,l2,k2)
                          B1<-c(A1,A3)
                          B2<-c(A2,A4)
                            if(length(B1)==length(B2)){ 
                            return(B1)
                              }else{
                                if(length(B1)>length(B2)){
                                return(B2)
                                  }else{ 
                                    return(B1)
                                  }
                             }
                         }
                    }
                }
            }
        }
    }else{
      print("ќдно из ребер или оба заданных ребра отсутствуют")        
    }
}

G<-newGraph(10)
n<-dim(G)[1]

G<-addEdge(G,1,2)
G<-addEdge(G,1,3)
G<-addEdge(G,1,4)
G<-addEdge(G,1,6)
G<-addEdge(G,2,6)
G<-addEdge(G,4,3)
G<-addEdge(G,3,5)
G<-addEdge(G,3,6)
G<-addEdge(G,4,5)
G<-addEdge(G,4,6)
G<-addEdge(G,4,7)
G<-addEdge(G,10,6)
G<-addEdge(G,7,9)
G<-addEdge(G,8,9)

library(igraph)
G<-igraph::graph.adjacency(adjmatrix=G,mode="undirected")
plot(G)

cycle(G,1,2,4,3)
cycle(G,8,9,5,3)
cycle(G,5,10,1,6)
cycle(G,4,6,6,2)

wave(G,5)
twotops(G,5,10)
