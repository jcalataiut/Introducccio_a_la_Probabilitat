# PROBLEMA 2 (Ampliació: Lotto 6/49)

rep <- 10^8
premi1 <- sample(1:49,6,replace=FALSE)
count_no_premi1 <- 0
count_no_premi2 <- 0

for (i in 1:rep){
  x <- sample(1:49,6,replace=FALSE)
  marcador<-0
  for (j in 1:6){
    for(k in 1:6){
      if(x[k]==premi1[j]){
        marcador=marcador+1
      }
    }}
    if(marcador==6){
      count_no_premi1=count_no_premi1+1}
    else if(marcador==5){
      count_no_premi2=count_no_premi2+1}
  }


probA <- count_no_premi1/rep; probA
probB <- count_no_premi2/rep; probB