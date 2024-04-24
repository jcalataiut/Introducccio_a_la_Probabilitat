## PROBLEMA 1. Pràctica 2

set.seed(99)

#Primer extraiem una bombeta de la caixa 1.
#Podem suposar, sense perdua de generalitat que 
#que la bombeta número 1 és defectuosa.

caixa.1<-1:10
caixa.2<-11:22
cont<-0
rep<-100000

for(i in 1:rep){
  x<-sample(caixa.1,1)
  a<-c(caixa.2,x)
  xx<-sample(a,1)
  if(xx==1|xx==11){
    cont<-cont+1
  } 
}

P_Def<-cont/rep; P_Def