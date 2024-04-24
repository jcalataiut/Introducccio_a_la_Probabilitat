# PROBLEMA 2

rep <- 100000
premi1 <- sample(1:10,4,replace=FALSE)
count_no_premi1 <- 0
count_no_premi2 <- 0

for (i in 1:rep){
  x <- sample(1:10,4,replace=FALSE)
  marcador<-0
  for (j in 1:4){
    for(k in 1:4){
      if(x[k]==premi1[j]){
        marcador=marcador+1
      }
    }}
    if(marcador==4){
      count_no_premi1=count_no_premi1+1}
    else if(marcador==3){
      count_no_premi2=count_no_premi2+1}
  }



probA <- count_no_premi1/rep; probA
probB <- count_no_premi2/rep; probB