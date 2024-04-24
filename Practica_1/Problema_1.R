### PROBLEMA 1:

### 

rep=10**5 # nombre de repeticions

count.no.rep<-0

for (i in 1:rep){
  x<-sample(1:10,5,replace=TRUE) # guarde els 5 valors aleatoris en un vector de longuitud 5
  xx<- unique(x) 
  if(length(xx)==5){
    count.no.rep<-count.no.rep+1
  }
}

count.no.rep/rep