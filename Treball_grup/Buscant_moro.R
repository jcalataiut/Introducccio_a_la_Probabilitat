rep<-10000
moro<-sample(conj,1)
cont<-0

for(i in 1:rep){
  conj<-1:36
  moro<-sample(conj,1)
  policia<-sample(conj,30,replace=F)
  if(moro %in% policia){cont<-cont+1}
}

prop<-cont/rep; prop



M<-matrix(1:36,nrow=6,byrow=T); M
m1<-matrix(rep(99,6),nrow=1)
m2<-matrix(rep(99,8),ncol=1)
M<-rbind(m1,M,m1); M
M<-cbind(m2,M,m2);M


##Funció que elimina un element d'un vector
elimina<-function(conj,x){
  conj.elimina<-c()
  for(i in 1:length(conj)){
    if(x!=conj[i]){conj.elimina<-c(conj.elimina,conj[i])}
  }
  return(conj.elimina)
}

##Funció que calcula les 4 caselles adjacents a una donada
caselles4<-function(x){
  if(x%%6==1){return(c(x-6,x+1,x+6,99))}
  if(x%%6==0){return(c(x-6,99,x+6,x-1))}
  else{return(c(x-6,x+1,x+6,x-1))}
}

conj.bores<-c(as.vector(M[2,2:7]),as.vector(M[7,2:7]),as.vector(M[3:6,2]),as.vector(M[3:6,7])); conj.bores


for(i in 1:rep){
  trobat<-"FALSE"
  acum_cas<-c()
  conj<-1:36
  for(j in 1:10){
    if(j==1){
      for (k in 1:3) {
        if(k==1){
          casella<-sample(conj.bores,1)
          acum_cas<-c(acum_cas,casella)
          conj<-elimina(conj,casella)
        } else{
          marcador<-TRUE
          while(marcador){
            casella<-sample(caselles4(casella),1)
            if(casella%in%conj){
              acum_cas<-c(acum_cas,casella)
              conj<-elimina(conj,casella)
              marcador<-FALSE
            }
          }
        }
      }
    } else{
      if(!any(c(caselles4(casella),conj.bores)%in%conj)){break} #és el cas en que l'última casella
      for(n in 1:3){                                            #del bomber NO ens permeteixca 
        if(n==1){
          marcador<-TRUE
          while(marcador){
            casella<-sample(c(caselles4(casella),conj.bores),1)
            if(!any(casella4(casella)%in%conj))
            if(casella%in%conj){
              acum_cas<-c(acum_cas,casella)
              conj<-elimina(conj,casella)
              marcador<-FALSE
              
            }
          }
        } else{
          marcador<-TRUE
          
        }
      }
    }
    
  }
  if(trobat=="TRUE"){cont<-cont+1}
}



if(!any(c(caselles4(casella),conj.bores)%in%conj)){next} #hi ha recolocar-ho



## PROVES 

acum_cas<-c()
conj<-1:36
for (k in 1:3) {
  if(k==1){
    casella<-sample(conj.bores,1)
    acum_cas<-c(acum_cas,casella)
    conj<-elimina(conj,casella)
  } else{
    marcador<-TRUE
    while(marcador){
      casella<-sample(caselles4(casella),1)
      if(casella%in%conj){
        acum_cas<-c(acum_cas,casella)
        conj<-elimina(conj,casella)
        marcador<-FALSE
      }
    }
  }
}
trobat<-FALSE
if(moro%in%acum_cas){trobat<-TRUE}
trobat


# CODI 1 (codi principal)
cont<-0
for(i in 1:rep){
  acum_cas<-c()
  conj<-1:36
  jove<-sample(conj,1)
  for(j in 1:10){ # comcençem el bucle de 10 bombers
    if(j==1){ # el primer és especial perque només pot entrar per una de les caselles que bordexen el recinte
      for (k in 1:3){
        if(k==1){
          casella<-sample(conj.bores,1)
          acum_cas<-c(acum_cas,casella)
          conj<-elimina(conj,casella)
        } else{
          casella<-nca(conj,casella)
          acum_cas<-c(acum_cas,casella)
          conj<-elimina(conj,casella)
        }
      }
    } else{
      if(!nca_inici(conj,casella)){break} #és el cas en que l'última casella
      for(n in 1:3){                              #del bomber NO ens permeteixca desplaçar-se
        if(n==1){
          casella<-nca_inici(conj,casella)
          acum_cas<-c(acum_cas,casella)
          conj<-elimina(conj,casella)
        } else{
          if(!nca(conj,casella)){break}
          else{
              casella<-nca(conj,casella)
              acum_cas<-c(acum_cas,casella)
              conj<-elimina(conj,casella)
            }
        }
      }
    }
  }
  if(jove%in%acum_cas){cont<-cont+1}
}

prop.jove<-cont/rep
prop.jove



### Funció caselles lliures continuació del bomber
nca<-function(conj,x){
  if(any(caselles4(x)%in%conj)){ #1er if
    cas.lliures<-caselles4(x)[caselles4(x)%in%conj]
    if(length(cas.lliures)==1){
      return(cas.lliures)
    } else {
      return(sample(cas.lliures,1))
      }
  } else{
    return(0)
  }
}

### Funció caselles lliures bombers nous 
nca_inici<-function(conj,x){
  cas_pos<-c(conj.bores,caselles4(x))
  cas_lliures<-cas_pos[cas_pos%in%conj]
  if(length(cas_lliures)!=0){
    return(sample(cas_lliures,1))
  } else{return(0)}
}



######## PREGUNTES:
# (1) Quantes caselles he recorregut per trobar-lo
# (2) Quants bombers han calgut per trobar-lo
# (3) Quantes vegades l'han trobat
