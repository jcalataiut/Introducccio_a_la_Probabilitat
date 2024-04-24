##PROBLEMA 2

rep<-100000
cont.cara<-0
cont.truc_cara<-0
monedes<-1:3
#moneda1=1-> dues cares, moneda2=2->normal, moneda3=3->75%

for(i in 1:rep){
  x<-sample(monedes,1)
  if(x==1){
    cont.cara<-cont.cara+1
  } else if(x==2){
    xx<-sample(1:2,1) #cara 1 i creu 2
    if(xx==1){cont.cara<-cont.cara+1}
  } else {
    xxx<-sample(1:2,1,prob=c(0.75,0.25))
    if(xxx==1){
      cont.cara<-cont.cara+1
      cont.truc_cara<-cont.truc_cara+1
    }
  }
}

prob_truc.cara=cont.truc_cara/cont.cara; prob_truc.cara