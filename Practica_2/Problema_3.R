##PROBLEMA 3

rep<-100000

count.A<-0 #contador de vegades que guanya A
count.B<-0 #contador de vegades que guanya B
count.C<-0 #contador de vegades que guanya C

for(i in 1:rep){
  
  bolas<-c(1:20)
  A=0
  B=0
  C=0
  
  while(A+B+C==0)
    {
    a<-sample(bolas,1)
    if(a %in% c(1:10))
    {A<-1} else {
        bolas<-bolas[which(bolas!=a)]
        b<-sample(bolas,1)
        if(b %in% c(1:10))
        {B<-1} else {
          bolas<-bolas[which(bolas!=b)]
          c<-sample(bolas,1)
          if(c %in% c(1:10))
          {C<-1} else {bolas<-bolas[which(bolas!=c)]}
        }
      }
  }
  
  if(A==1)
  {count.A<-count.A+1}
  
  if(B==1)
  {count.B<-count.B+1}
  
  if(C==1)
  {count.C<-count.C+1}
}

prob.j1<-count.A/rep; prob.j1
prob.j2<-count.B/rep; prob.j2
prob.j3<-count.C/rep; prob.j3

