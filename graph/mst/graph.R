newGraph<-function(n){                              #создание графа   
  G <- matrix(0,n,n) 
  return(G)  
} 

addEdge<-function(G,a1,a2, a3){                        #добавление дуги
  if((a1<=n)&(a2<=n)&(a1>0)&(a1>0)) { 
    G[a1,a2]<-a3 
  }else{
    print("Дуга не принадлежит графу") 
  }
  return(G)
} 

G<-newGraph(16)
n<-dim(G)[1]

G<-addEdge(G,1,2,2) 
G<-addEdge(G,1,5,7)
G<-addEdge(G,2,3,2)
G<-addEdge(G,2,5,7)
G<-addEdge(G,2,10,10)
G<-addEdge(G,3,4,2)
G<-addEdge(G,3,5,5)
G<-addEdge(G,3,7,5)
G<-addEdge(G,4,5,5)
G<-addEdge(G,4,8,9)
G<-addEdge(G,4,11,6)
G<-addEdge(G,4,13,10)
G<-addEdge(G,4,14,4)
G<-addEdge(G,5,6,4)
G<-addEdge(G,5,11,9)
G<-addEdge(G,5,15,6)
G<-addEdge(G,6,12,3)
G<-addEdge(G,6,15,3)
G<-addEdge(G,7,9,3)
G<-addEdge(G,7,10,5)
G<-addEdge(G,8,9,3)
G<-addEdge(G,8,14,9)
G<-addEdge(G,9,10,6)
G<-addEdge(G,9,16,3)
G<-addEdge(G,10,16,4)
G<-addEdge(G,11,12,6)
G<-addEdge(G,12,13,3)
G<-addEdge(G,13,14,9)

# 28 ребер

adj<-function(G,v1){                               #поиск смежных вершин
  A<-c() 
  for(i in 1:n){                                   #если существует ребро, добавляем смежную вершину в пустой массив 
    if(G[v1,i]>0){ 
      A<-c(A,i) 
    }
  } 
  return(A) 
}

adj(G,4)

min_el = which(G == min(G) , arr.ind = TRUE)
min_el[1,]

g_sort = G

mat_sort <- matrix(Inf,28,3)

text <- c()

k = 28

while ( sum(g_sort)  != 0 ) {
  
  max_el = which(g_sort == max(g_sort) , arr.ind = TRUE)
  
  
  mat_sort[k,1] <- max_el[dim(max_el)[1],1]
  mat_sort[k,2] <- max_el[dim(max_el)[1],2] 
  mat_sort[k,3] <- G[max_el[dim(max_el)[1],1], max_el[dim(max_el)[1],2]]
  
  vector<-c( "(" , max_el[dim(max_el)[1],1] , "," ,max_el[dim(max_el)[1],2] , "," ,  G[max_el[dim(max_el)[1],1], max_el[dim(max_el)[1],2]], "), " )
  text = c(vector, text)
  
  g_sort[max_el[dim(max_el)[1],1],max_el[dim(max_el)[1],2]] = 0
  k=k-1
  }


sorted = mat_sort
T = c(sorted[1, ])                            # SpanningTree = {};
V = c(sorted[1,1], sorted[1,2]  )             # ReachSet = {0};                    # You can use any node...
U = seq(1,16)                                 # UnReachSet = {1, 2, ..., N-1};
U = U [! U %in% V]

T_text = c("(",      sorted[1,1], ", " ,  sorted[1,2], ", ",  sorted[1,3] , "), " )


sorted = sorted[-1, ]
k=1 

i=1

text = c()
V_text = c()
U_text = c()

while (sum(U) != 0 ) {                                        # while ( UnReachSet ≠ empty )
    if (sorted[k,1]  %in%  V & sorted[k,2]  %in%  U    ) {       #  x ∈ ReachSet   and  y  ∈ UnReachSet   and e has smallest cost
      T =   c(T, sorted[k, ])                       # SpanningTree = SpanningTree ∪ {e};
      V = c(V, sorted[k,2])                                              # ReachSet   = ReachSet ∪ {y};
      U = U [! U %in% V]                          #   UnReachSet = UnReachSet - {y};
      i = i+1 
      
      for (j in 1:length(V))  {
      V_text = c(V_text, V[j], ", ") 
      }
      
      for (j in 1:length(U))  {
        U_text = c(U_text, U[j], ", ") 
      }
      
      T_text = c(T_text,  "(",      sorted[k,1], ", " ,  sorted[k,2], ", ", sorted[k,3], "), " )
      
      text = c(text, "\n\n Шаг  ",  i,   "\n\n  Находим ребро с минимальным   весом, смежное  с ребрами  дерева $T$:  \n\n  Минимальный элемент: $(",                 sorted[k,1], ", " ,  sorted[k,2], ", ", sorted[k,3], ")$  \n\n  Добавляем  конечную вершину ребра в множество пройденных вершин $V$ и удаляем ее из множества непройденных вершин $U$:  \n\n  " ,                  "$V = пскобка" , V_text,    "лскобка$  \n\n     $U = пскобка", U_text,   "лскобка$   \n\n      Добавляем ребро в множество  $T$:  $T = пскобка ",  T_text, "лскобка$   \n\n  " ) 
      
        sorted = sorted[-k, ] 
      
        k = 1
        V_text = c()
        U_text = c()
    } else {
          k = k+1
          }
}

print(T)

ttt = matrix(T, ncol = 3,  byrow = TRUE) 

sum(ttt[,3])  

sum(mat_sort[,3])  

cat(text)
























