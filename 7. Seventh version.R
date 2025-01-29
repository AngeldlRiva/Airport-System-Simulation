
exp_inversa=function(n,lambda){ 
  #lambda=0.7875=189 pasajeros/240 minutos
  u=runif(n,0,1)
  #La inversa de la exponencial
  x= -log(1-u)/lambda
  return(x) }

norm_truncada=function(media,desviacion_tipica,tr_ab,tr_arr,n){ 
  #GENERAMOS NORMAL MEDIA 3 DESVIACION TIPICA 2 y MEDIA 10 Y DESVIACION TIPICA 5, COGEMOS COMO C=1.1 YA QUE CUMPLE LAS CONDICIONES DEL TEOREMA
  suma=c()
  c=1.1
  cont=0
  while(length(suma)<n)
  { u=runif(1,0,1)
  y=runif(1,tr_ab,tr_arr)
  if(u<4*exp(-((y-media)/(desviacion_tipica*4))^2)/(0.6827*desviacion_tipica*c*sqrt(2*pi))){ 
    suma=c(suma,y)
  }
  cont=cont+1
  }
  return(suma)
}

Llegada=function(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                 Ncheckin,Nseguridad,Npasaporte,Nembarque,xS){
  # distribucion de llegada
  DL=exp_inversa(1,189/240)
  TL=TM+DL
  u=runif(1)
  if (u<0.3){ # porcentaje de personas que no hace el checkin online, van a checkin
    # van al checkin con menos cola
    j= which.min(Ncheckin)
    Ncheckin[j]=Ncheckin[j]+1
    if(Ncheckin[j]==1){ # genera siguiente fin de checkin_j si no habia nadie
      DScheckin=norm_truncada(3,2,1,5,1)
      TScheckin[j]=TM+DScheckin
    }
    # actualizacion de sumas
    SUMAcheckin=SUMAcheckin+(sum(Ncheckin)-1)*(TM-TANT)
    SUMAseguridad=SUMAseguridad+sum(Nseguridad)*(TM-TANT)
    SUMApasaporte=SUMApasaporte+Npasaporte*(TM-TANT)
    SUMAembarque=SUMAembarque+sum(Nembarque)*(TM-TANT)
    TANT=TM
  }else{ #van a seguridad directamente
    #mandamos a la cola mas vacia
    if (sum(xS)!=0){ # si no estan todas las maquinas rotas
      #i sera el indice de la cola mas vacia cuya maquina esté operativa
      ind_maquinas_operativas=which(xS==1)
      i_filtrada=which.min(Nseguridad[ind_maquinas_operativas])
      i=ind_maquinas_operativas[i_filtrada]
    }else{
      i=which.min(Nseguridad)
    }
    Nseguridad[i]=Nseguridad[i]+1
    if(Nseguridad[i]==1){
      DSseguridad=norm_truncada(10,5,5,15,1)
      TSseguridad[i]=TM+DSseguridad
    }
    # actualizacion de sumas
    SUMAcheckin=SUMAcheckin+sum(Ncheckin)*(TM-TANT)
    SUMAseguridad=SUMAseguridad+(sum(Nseguridad)-1)*(TM-TANT)
    SUMApasaporte=SUMApasaporte+Npasaporte*(TM-TANT)
    SUMAembarque=SUMAembarque+sum(Nembarque)*(TM-TANT)
    TANT=TM
  }
  return(list(TL = TL,
              TScheckin = TScheckin,
              TSseguridad = TSseguridad,
              SUMAcheckin = SUMAcheckin,
              SUMAseguridad = SUMAseguridad,
              SUMApasaporte = SUMApasaporte,
              SUMAembarque = SUMAembarque,
              Ncheckin = Ncheckin,
              Nseguridad = Nseguridad,
              Npasaporte = Npasaporte,
              Nembarque = Nembarque,
              TANT = TANT))
}

Checkin=function(j,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                 Ncheckin,Nseguridad,Npasaporte,Nembarque,xS){
  # finalizacion de una persona en este servicio
  Ncheckin[j]=Ncheckin[j]-1
  DScheckin=norm_truncada(3,2,1,5,1)
  TScheckin[j]=ifelse(Ncheckin[j]==0,Inf,TM+DScheckin)
  # añade una persona a la cola de la siguiente etapa (seguridad)
  #mandamos a la cola mas vacia con maquina operativa
  if (sum(xS)!=0){ # si no estan todas las maquinas rotas
    #i sera el indice de la cola mas vacia cuya maquina esté operativa
    ind_maquinas_operativas=which(xS==1)
    i_filtrada=which.min(Nseguridad[ind_maquinas_operativas])
    i=ind_maquinas_operativas[i_filtrada]
  }else{
    i=which.min(Nseguridad)
  }
  Nseguridad[i]=Nseguridad[i]+1
  if(Nseguridad[i]==1){ # genera siguiente fin de seguridad_i si no habia nadie
    DSseguridad=norm_truncada(10,5,5,15,1)
    TSseguridad[i]=TM+DSseguridad
  }
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+(sum(Ncheckin)+1)*(TM-TANT)
  SUMAseguridad=SUMAseguridad+(sum(Nseguridad)-1)*(TM-TANT)
  SUMApasaporte=SUMApasaporte+Npasaporte*(TM-TANT)
  SUMAembarque=SUMAembarque+sum(Nembarque)*(TM-TANT)
  TANT=TM
  return(list(TScheckin = TScheckin,
              TSseguridad = TSseguridad,
              SUMAcheckin = SUMAcheckin,
              SUMAseguridad = SUMAseguridad,
              SUMApasaporte = SUMApasaporte,
              SUMAembarque = SUMAembarque,
              Ncheckin = Ncheckin,
              Nseguridad = Nseguridad,
              Npasaporte = Npasaporte,
              Nembarque = Nembarque,
              TANT = TANT))
}

Seguridad=function(i,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                   Ncheckin,Nseguridad,Npasaporte,Nembarque,xS){
  if (xS[i]==1){ # si xS[i] era 1 es poque la maquina funcionaba y ha habido una finalizacion
    Nseguridad[i]=Nseguridad[i]-1
    u = runif(1)
    if (u<0.5){#añadimos que hay gente que va a pasaporte (vuelo 2 fuera de la UE)
      Npasaporte=Npasaporte+1
      if(Npasaporte==1){
        DSpasaporte=norm_truncada(3,2,1,5,1)
        TSpasaporte=TM+DSpasaporte
      }
    }else{#añadimos que hay gente que va directamente a vuelo 1 (dentro de la UE)
      Nembarque[1]=Nembarque[1]+1
      if(Nembarque[1]==1){
        DSembarque=norm_truncada(10,5,5,15,1)
        TSembarque[1]=TM+DSembarque
      }
    }
    u=runif(1)
    if(u<0.04){ # probabilidad de rotura de la máquina
      #print(c("SE HA ROTO LA MAQUINA",i))
      xS[i]=0
      a=runif(1,10,20)
      TSseguridad[i]=TM+a # el siguiente tiempo de cambio sera cuando se arregle la maquina
      #repartimos esta cola entre el resto de maquinas operativas
      if(sum(xS)!=0){
        while (Nseguridad[i]>0.5){
          ind_maquinas_operativas=which(xS==1)
          j_filtrada=which.min(Nseguridad[ind_maquinas_operativas])
          j=ind_maquinas_operativas[j_filtrada]
          Nseguridad[i]=Nseguridad[i]-1
          Nseguridad[j]=Nseguridad[j]+1
        }
      }
    }else{ # si no se ha roto genero el siguiente tiempo de cambio de la manera usual
      DSseguridad=norm_truncada(3,2,1,5,1)
      TSseguridad[i]=ifelse(Nseguridad[i]==0,Inf,TM+DSseguridad)
    }
  }else{ # si xS[i] era 0 y ha habido una actualizacion es poque la maquina acaba de ser arreglada
    xS[i]=1
    #print(c("SE HA ARREGLADO LA MAQUINA",i))
  }
  #print(Nseguridad)
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+sum(Ncheckin)*(TM-TANT)
  SUMAseguridad=SUMAseguridad+(sum(Nseguridad)+1)*(TM-TANT)
  SUMApasaporte=SUMApasaporte+(Npasaporte-1)*(TM-TANT)
  SUMAembarque=SUMAembarque+sum(Nembarque)*(TM-TANT)
  TANT=TM
  return(list(TSseguridad = TSseguridad,
              TSpasaporte = TSpasaporte,
              TSembarque = TSembarque,
              SUMAcheckin = SUMAcheckin,
              SUMAseguridad = SUMAseguridad,
              SUMApasaporte = SUMApasaporte,
              SUMAembarque = SUMAembarque,
              Ncheckin = Ncheckin,
              Nseguridad = Nseguridad,
              Npasaporte = Npasaporte,
              Nembarque = Nembarque,
              TANT = TANT,
              xS = xS ))
}

Pasaporte=function(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                   Ncheckin,Nseguridad,Npasaporte,Nembarque){
  # finalizacion de una persona en este servicio
  Npasaporte=Npasaporte-1
  DSpasaporte=norm_truncada(3,2,1,5,1)
  TSpasaporte=ifelse(Npasaporte==0,Inf,TM+DSpasaporte)
  # añade una persona a la cola de la siguiente etapa (embarque vuelo 2)
  Nembarque[2]=Nembarque[2]+1
  if(Nembarque[2]==1){
    DSembarque=norm_truncada(10,5,5,15,1)
    TSembarque[2]=TM+DSembarque
  }
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+sum(Ncheckin)*(TM-TANT)
  SUMAseguridad=SUMAseguridad+sum(Nseguridad)*(TM-TANT)
  SUMApasaporte=SUMApasaporte+(Npasaporte+1)*(TM-TANT)
  SUMAembarque=SUMAembarque+(sum(Nembarque)-1)*(TM-TANT)
  TANT=TM
  return(list(TSpasaporte = TSpasaporte,
              TSembarque = TSembarque,
              SUMAcheckin = SUMAcheckin,
              SUMAseguridad = SUMAseguridad,
              SUMApasaporte = SUMApasaporte,
              SUMAembarque = SUMAembarque,
              Ncheckin = Ncheckin,
              Nseguridad = Nseguridad,
              Npasaporte = Npasaporte,
              Nembarque = Nembarque,
              TANT = TANT))
}

Embarque=function(i,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                  Ncheckin,Nseguridad,Npasaporte,Nembarque){
  # finalizacion de una persona en este servicio
  Nembarque[i]=Nembarque[i]-1
  DSembarque=norm_truncada(10,5,5,15,1)
  TSembarque[i]=ifelse(Nembarque[i]==0,Inf,TM+DSembarque)
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+sum(Ncheckin)*(TM-TANT)
  SUMAseguridad=SUMAseguridad+sum(Nseguridad)*(TM-TANT)
  SUMApasaporte=SUMApasaporte+(Npasaporte)*(TM-TANT)
  SUMAembarque=SUMAembarque+(sum(Nembarque)+1)*(TM-TANT)
  TANT=TM
  return(list(TSembarque = TSembarque,
              SUMAcheckin = SUMAcheckin,
              SUMAseguridad = SUMAseguridad,
              SUMApasaporte = SUMApasaporte,
              SUMAembarque = SUMAembarque,
              Ncheckin = Ncheckin,
              Nseguridad = Nseguridad,
              Npasaporte = Npasaporte,
              Nembarque = Nembarque,
              TANT = TANT))
}

#####################################################################
#Notación
#Ncheckin=número de personas en la checkin
#Nseguridad=número de personas en la seguridad
#Ncheckin=número de personas en la checkin
#Nseguridad=número de personas en la seguridad
#TM=reloj de simulación
#TL=instante de la próxima llegada
#TScheckin=instante de la próxima finalización del checkin
#TSseguridad=instante de la próxima finalización del seguridad
#TSpasaporte=instante de la próxima finalización del pasaporte
#TSembarque=instante de la próxima finalización del embarque
#Tmax=tiempo máximo de simulación (en min)
#DL=tiempo entre llegadas consecutivas
#DScheckin=duración de servicio checkin
#DSseguridad=duración de servicio seguridad
#DSpasaporte=duración de servicio pasaporte
#DSembarque=duración de servicio embarque
#SUMAcheckin=contador suma acumulada de áreas asociada a checkin
#SUMAseguridad=contador suma acumulada de áreas asociada a seguridad
#SUMApasaporte=contador suma acumulada de áreas asociada a pasaporte
#SUMAembarque=contador suma acumulada de áreas asociada a embarque

#xS[i]=1 si la seguridad_i funciona, xS[i]=0 si no
#####################################################################

#PROGRAMA PRINCIPAL MONTECARLO
#Inicializamos las variables
set.seed(1234)
num_segur = 4 #este es el numero de controles de seguridad (con sus respectivas colas) en paralelo
num_checkin = 2 #este es el numero de checkins (con sus respectivas colas) en paralelo
MC=1000
v_SUMAcheckin=numeric(MC)
v_SUMAseguridad=numeric(MC)
v_SUMApasaporte=numeric(MC)
v_SUMAembarque=numeric(MC)
for (t in 1:MC){
  Ncheckin=rep(0,num_checkin)
  Nseguridad=rep(0,num_segur)
  Npasaporte=0
  Nembarque=c(0,0) # dos vuelos
  TM=0
  TANT=0
  TScheckin=rep(Inf,num_checkin)
  TSseguridad=rep(Inf,num_segur)
  TSpasaporte=Inf
  TSembarque=c(Inf,Inf)
  Tmax=240
  xS=rep(1,num_segur)
  
  SUMAcheckin=0
  SUMAseguridad=0
  SUMApasaporte=0
  SUMAembarque=0
  
  #Genero la primera llegada, va a checkin
  DL=exp_inversa(1,189/240)
  TL=DL
  while(TM<Tmax){
    #Identificamos el evento que tiene lugar antes
    TM=min(TL,min(TScheckin),min(TSseguridad),TSpasaporte,min(TSembarque))
    Estado=1*(TM==TL)+2*(TM==min(TScheckin))+3*(TM==min(TSseguridad))+4*(TM==TSpasaporte)+5*(TM==min(TSembarque))
    if(Estado==1){
      k=Llegada(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                Ncheckin,Nseguridad,Npasaporte,Nembarque,xS)
      TL = k$TL
      TScheckin <- k$TScheckin
      TSseguridad <- k$TSseguridad
      SUMAcheckin <- k$SUMAcheckin
      SUMAseguridad <- k$SUMAseguridad
      SUMApasaporte <- k$SUMApasaporte
      SUMAembarque <- k$SUMAembarque
      Ncheckin <- k$Ncheckin
      Nseguridad <- k$Nseguridad
      Npasaporte <- k$Npasaporte
      Nembarque <- k$Nembarque
      TANT <- k$TANT
    }else if(Estado==2){
      j=which.min(TScheckin)
      k=Checkin(j,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                Ncheckin,Nseguridad,Npasaporte,Nembarque,xS)
      TScheckin <- k$TScheckin
      TSseguridad <- k$TSseguridad
      SUMAcheckin <- k$SUMAcheckin
      SUMAseguridad <- k$SUMAseguridad
      SUMApasaporte <- k$SUMApasaporte
      SUMAembarque <- k$SUMAembarque
      Ncheckin <- k$Ncheckin
      Nseguridad <- k$Nseguridad
      Npasaporte <- k$Npasaporte
      Nembarque <- k$Nembarque
      TANT <- k$TANT
    }else if(Estado==3){
      i=which.min(TSseguridad)
      k <- Seguridad(i,TM, SUMAcheckin, SUMAseguridad, SUMApasaporte, SUMAembarque, 
                     Ncheckin, Nseguridad, Npasaporte, Nembarque,xS)
      TSseguridad <- k$TSseguridad
      TSpasaporte <- k$TSpasaporte
      TSembarque <- k$TSembarque
      SUMAcheckin <- k$SUMAcheckin
      SUMAseguridad <- k$SUMAseguridad
      SUMApasaporte <- k$SUMApasaporte
      SUMAembarque <- k$SUMAembarque
      Ncheckin <- k$Ncheckin
      Nseguridad <- k$Nseguridad
      Npasaporte <- k$Npasaporte
      Nembarque <- k$Nembarque
      TANT <- k$TANT
      xS <- k$xS
    }else if(Estado==4){
      k=Pasaporte(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                  Ncheckin,Nseguridad,Npasaporte,Nembarque)
      TSpasaporte <- k$TSpasaporte
      TSembarque <- k$TSembarque
      SUMAcheckin <- k$SUMAcheckin
      SUMAseguridad <- k$SUMAseguridad
      SUMApasaporte <- k$SUMApasaporte
      SUMAembarque <- k$SUMAembarque
      Ncheckin <- k$Ncheckin
      Nseguridad <- k$Nseguridad
      Npasaporte <- k$Npasaporte
      Nembarque <- k$Nembarque
      TANT <- k$TANT
    }else if(Estado==5){
      i=which.min(TSembarque)
      k=Embarque(i,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                 Ncheckin,Nseguridad,Npasaporte,Nembarque)
      TSembarque <- k$TSembarque
      SUMAcheckin <- k$SUMAcheckin
      SUMAseguridad <- k$SUMAseguridad
      SUMApasaporte <- k$SUMApasaporte
      SUMAembarque <- k$SUMAembarque
      Ncheckin <- k$Ncheckin
      Nseguridad <- k$Nseguridad
      Npasaporte <- k$Npasaporte
      Nembarque <- k$Nembarque
      TANT <- k$TANT
    }
  }
  v_SUMAcheckin[t]=SUMAcheckin
  v_SUMAseguridad[t]=SUMAseguridad
  v_SUMApasaporte[t]=SUMApasaporte
  v_SUMAembarque[t]=SUMAembarque
}
mean(v_SUMAcheckin)
mean(v_SUMAseguridad)
mean(v_SUMApasaporte)
mean(v_SUMAembarque)
sd(v_SUMAcheckin)
sd(v_SUMAseguridad)
sd(v_SUMApasaporte)
sd(v_SUMAembarque)

#VAMOS A COMPARAR DISTINTOS NUMEROS DE CHECKINS, PARA VER CUANTOS SERIAN SUFICIENTES
v_media_SUMAcheckin=c()
for (num_checkin in 1:10){
  num_segur = 4 #este es el numero de controles de seguridad (con sus respectivas colas) en paralelo
  MC=500
  v_SUMAcheckin=numeric(MC)
  for (t in 1:MC){
    Ncheckin=rep(0,num_checkin)
    Nseguridad=rep(0,num_segur)
    Npasaporte=0
    Nembarque=c(0,0) # dos vuelos
    TM=0
    TANT=0
    TScheckin=rep(Inf,num_checkin)
    TSseguridad=rep(Inf,num_segur)
    TSpasaporte=Inf
    TSembarque=c(Inf,Inf)
    Tmax=240
    xS=rep(1,num_segur)
    
    SUMAcheckin=0
    SUMAseguridad=0
    SUMApasaporte=0
    SUMAembarque=0
    
    #Genero la primera llegada, va a checkin
    DL=exp_inversa(1,189/240)
    TL=DL
    while(TM<Tmax){
      #Identificamos el evento que tiene lugar antes
      TM=min(TL,min(TScheckin),min(TSseguridad),TSpasaporte,min(TSembarque))
      Estado=1*(TM==TL)+2*(TM==min(TScheckin))+3*(TM==min(TSseguridad))+4*(TM==TSpasaporte)+5*(TM==min(TSembarque))
      if(Estado==1){
        k=Llegada(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                  Ncheckin,Nseguridad,Npasaporte,Nembarque,xS)
        TL = k$TL
        TScheckin <- k$TScheckin
        TSseguridad <- k$TSseguridad
        SUMAcheckin <- k$SUMAcheckin
        SUMAseguridad <- k$SUMAseguridad
        SUMApasaporte <- k$SUMApasaporte
        SUMAembarque <- k$SUMAembarque
        Ncheckin <- k$Ncheckin
        Nseguridad <- k$Nseguridad
        Npasaporte <- k$Npasaporte
        Nembarque <- k$Nembarque
        TANT <- k$TANT
      }else if(Estado==2){
        j=which.min(TScheckin)
        k=Checkin(j,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                  Ncheckin,Nseguridad,Npasaporte,Nembarque,xS)
        TScheckin <- k$TScheckin
        TSseguridad <- k$TSseguridad
        SUMAcheckin <- k$SUMAcheckin
        SUMAseguridad <- k$SUMAseguridad
        SUMApasaporte <- k$SUMApasaporte
        SUMAembarque <- k$SUMAembarque
        Ncheckin <- k$Ncheckin
        Nseguridad <- k$Nseguridad
        Npasaporte <- k$Npasaporte
        Nembarque <- k$Nembarque
        TANT <- k$TANT
      }else if(Estado==3){
        i=which.min(TSseguridad)
        k <- Seguridad(i,TM, SUMAcheckin, SUMAseguridad, SUMApasaporte, SUMAembarque, 
                       Ncheckin, Nseguridad, Npasaporte, Nembarque,xS)
        TSseguridad <- k$TSseguridad
        TSpasaporte <- k$TSpasaporte
        TSembarque <- k$TSembarque
        SUMAcheckin <- k$SUMAcheckin
        SUMAseguridad <- k$SUMAseguridad
        SUMApasaporte <- k$SUMApasaporte
        SUMAembarque <- k$SUMAembarque
        Ncheckin <- k$Ncheckin
        Nseguridad <- k$Nseguridad
        Npasaporte <- k$Npasaporte
        Nembarque <- k$Nembarque
        TANT <- k$TANT
        xS <- k$xS
      }else if(Estado==4){
        k=Pasaporte(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                    Ncheckin,Nseguridad,Npasaporte,Nembarque)
        TSpasaporte <- k$TSpasaporte
        TSembarque <- k$TSembarque
        SUMAcheckin <- k$SUMAcheckin
        SUMAseguridad <- k$SUMAseguridad
        SUMApasaporte <- k$SUMApasaporte
        SUMAembarque <- k$SUMAembarque
        Ncheckin <- k$Ncheckin
        Nseguridad <- k$Nseguridad
        Npasaporte <- k$Npasaporte
        Nembarque <- k$Nembarque
        TANT <- k$TANT
      }else if(Estado==5){
        i=which.min(TSembarque)
        k=Embarque(i,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                   Ncheckin,Nseguridad,Npasaporte,Nembarque)
        TSembarque <- k$TSembarque
        SUMAcheckin <- k$SUMAcheckin
        SUMAseguridad <- k$SUMAseguridad
        SUMApasaporte <- k$SUMApasaporte
        SUMAembarque <- k$SUMAembarque
        Ncheckin <- k$Ncheckin
        Nseguridad <- k$Nseguridad
        Npasaporte <- k$Npasaporte
        Nembarque <- k$Nembarque
        TANT <- k$TANT
      }
    }
    v_SUMAcheckin[t]=SUMAcheckin
  }
  v_media_SUMAcheckin=c(v_media_SUMAcheckin,mean(v_SUMAcheckin))
}
plot(v_media_SUMAcheckin, type = "o", col = "blue", xlab = "Número de checkins", ylab = "SUMA_checkin")

#VAMOS A COMPARAR DISTINTOS NUMEROS DE CONTROLES DE SEGURIDAD, PARA VER CUANTOS SERIAN SUFICIENTES
v_media_SUMAseguridad=c()
for (num_segur in 1:10){
  num_checkin = 3 # asumimos 3 checkins
  MC=500
  v_SUMAseguridad=numeric(MC)
  for (t in 1:MC){
    Ncheckin=rep(0,num_checkin)
    Nseguridad=rep(0,num_segur)
    Npasaporte=0
    Nembarque=c(0,0) # dos vuelos
    TM=0
    TANT=0
    TScheckin=rep(Inf,num_checkin)
    TSseguridad=rep(Inf,num_segur)
    TSpasaporte=Inf
    TSembarque=c(Inf,Inf)
    Tmax=240
    xS=rep(1,num_segur)
    
    SUMAcheckin=0
    SUMAseguridad=0
    SUMApasaporte=0
    SUMAembarque=0
    
    #Genero la primera llegada, va a checkin
    DL=exp_inversa(1,189/240)
    TL=DL
    while(TM<Tmax){
      #Identificamos el evento que tiene lugar antes
      TM=min(TL,min(TScheckin),min(TSseguridad),TSpasaporte,min(TSembarque))
      Estado=1*(TM==TL)+2*(TM==min(TScheckin))+3*(TM==min(TSseguridad))+4*(TM==TSpasaporte)+5*(TM==min(TSembarque))
      if(Estado==1){
        k=Llegada(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                  Ncheckin,Nseguridad,Npasaporte,Nembarque,xS)
        TL = k$TL
        TScheckin <- k$TScheckin
        TSseguridad <- k$TSseguridad
        SUMAcheckin <- k$SUMAcheckin
        SUMAseguridad <- k$SUMAseguridad
        SUMApasaporte <- k$SUMApasaporte
        SUMAembarque <- k$SUMAembarque
        Ncheckin <- k$Ncheckin
        Nseguridad <- k$Nseguridad
        Npasaporte <- k$Npasaporte
        Nembarque <- k$Nembarque
        TANT <- k$TANT
      }else if(Estado==2){
        j=which.min(TScheckin)
        k=Checkin(j,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                  Ncheckin,Nseguridad,Npasaporte,Nembarque,xS)
        TScheckin <- k$TScheckin
        TSseguridad <- k$TSseguridad
        SUMAcheckin <- k$SUMAcheckin
        SUMAseguridad <- k$SUMAseguridad
        SUMApasaporte <- k$SUMApasaporte
        SUMAembarque <- k$SUMAembarque
        Ncheckin <- k$Ncheckin
        Nseguridad <- k$Nseguridad
        Npasaporte <- k$Npasaporte
        Nembarque <- k$Nembarque
        TANT <- k$TANT
      }else if(Estado==3){
        i=which.min(TSseguridad)
        k <- Seguridad(i,TM, SUMAcheckin, SUMAseguridad, SUMApasaporte, SUMAembarque, 
                       Ncheckin, Nseguridad, Npasaporte, Nembarque,xS)
        TSseguridad <- k$TSseguridad
        TSpasaporte <- k$TSpasaporte
        TSembarque <- k$TSembarque
        SUMAcheckin <- k$SUMAcheckin
        SUMAseguridad <- k$SUMAseguridad
        SUMApasaporte <- k$SUMApasaporte
        SUMAembarque <- k$SUMAembarque
        Ncheckin <- k$Ncheckin
        Nseguridad <- k$Nseguridad
        Npasaporte <- k$Npasaporte
        Nembarque <- k$Nembarque
        TANT <- k$TANT
        xS <- k$xS
      }else if(Estado==4){
        k=Pasaporte(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                    Ncheckin,Nseguridad,Npasaporte,Nembarque)
        TSpasaporte <- k$TSpasaporte
        TSembarque <- k$TSembarque
        SUMAcheckin <- k$SUMAcheckin
        SUMAseguridad <- k$SUMAseguridad
        SUMApasaporte <- k$SUMApasaporte
        SUMAembarque <- k$SUMAembarque
        Ncheckin <- k$Ncheckin
        Nseguridad <- k$Nseguridad
        Npasaporte <- k$Npasaporte
        Nembarque <- k$Nembarque
        TANT <- k$TANT
      }else if(Estado==5){
        i=which.min(TSembarque)
        k=Embarque(i,TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                   Ncheckin,Nseguridad,Npasaporte,Nembarque)
        TSembarque <- k$TSembarque
        SUMAcheckin <- k$SUMAcheckin
        SUMAseguridad <- k$SUMAseguridad
        SUMApasaporte <- k$SUMApasaporte
        SUMAembarque <- k$SUMAembarque
        Ncheckin <- k$Ncheckin
        Nseguridad <- k$Nseguridad
        Npasaporte <- k$Npasaporte
        Nembarque <- k$Nembarque
        TANT <- k$TANT
      }
    }
    v_SUMAseguridad[t]=SUMAseguridad
  }
  v_media_SUMAseguridad=c(v_media_SUMAseguridad,mean(v_SUMAseguridad))
}
plot(v_media_SUMAseguridad, type = "o", col = "blue", 
     xlab = "Número de controles de seguridad", ylab = "SUMAseguridad")
