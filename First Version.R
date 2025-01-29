
exp_inversa=function(n,lambda){ 
  #lambda=0.7875=189 pasajeros/240 minutos
  u=runif(n,0,1)
  #La inversa de la exponencial
  x= -log(1-u)/lambda
  return(x) }

norm_truncada=function(media,desviacion_tipica,tr_ab,tr_arr,n){ 
  #GENERAMOS NORMAL MEDIA 3 DESVIACION TIPICA 2 y MEDIA 10 Y DESVIACION TIPICA 5, COGEMOS COMO C=1.1 YA QUE CUMPLE LAS CONDICIONES DEL TEOREMA
  suma=c()
  cont=0
  c=1.1
  while(length(suma)<n){
    u=runif(1,0,1)
    y=runif(1,tr_ab,tr_arr)
    if(u<4*exp(-((y-media)/(desviacion_tipica*4))^2)/(0.6827*desviacion_tipica*c*sqrt(2*pi))){ 
      suma=c(suma,y)}
    cont=cont+1
  }
  return(suma)
}

Llegada=function(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                 Ncheckin,Nseguridad,Npasaporte,Nembarque){
  # distribucion de llegada
  DL=exp_inversa(1,189/240)
  TL=TM+DL
  # añade una persona a la cola de la siguiente etapa (checkin)
  Ncheckin=Ncheckin+1
  if(Ncheckin==1){ # genera siguiente fin de checkin si no habia nadie
    DScheckin=norm_truncada(3,2,1,5,1)
    TScheckin=TM+DScheckin
  }
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+(Ncheckin-1)*(TM-TANT)
  SUMAseguridad=SUMAseguridad+Nseguridad*(TM-TANT)
  SUMApasaporte=SUMApasaporte+Npasaporte*(TM-TANT)
  SUMAembarque=SUMAembarque+Nembarque*(TM-TANT)
  TANT=TM
  return(c(TL,TScheckin,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
           Ncheckin,Nseguridad,Npasaporte,Nembarque,TANT))
}

Checkin=function(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                 Ncheckin,Nseguridad,Npasaporte,Nembarque){
  # finalizacion de una persona en este servicio
  Ncheckin=Ncheckin-1
  DScheckin=DScheckin=norm_truncada(3,2,1,5,1)
  TScheckin=ifelse(Ncheckin==0,Inf,TM+DScheckin)
  # añade una persona a la cola de la siguiente etapa (seguridad)
  Nseguridad=Nseguridad+1
  if(Nseguridad==1){ # genera siguiente fin de seguridad si no habia nadie
    DSseguridad=norm_truncada(10,5,5,15,1)
    TSseguridad=TM+DSseguridad
  }
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+(Ncheckin+1)*(TM-TANT)
  SUMAseguridad=SUMAseguridad+(Nseguridad-1)*(TM-TANT)
  SUMApasaporte=SUMApasaporte+Npasaporte*(TM-TANT)
  SUMAembarque=SUMAembarque+Nembarque*(TM-TANT)
  TANT=TM
  return(c(TScheckin,TSseguridad,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
           Ncheckin,Nseguridad,Npasaporte,Nembarque,TANT))
}

Seguridad=function(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                   Ncheckin,Nseguridad,Npasaporte,Nembarque){
  # finalizacion de una persona en este servicio
  Nseguridad=Nseguridad-1
  DSseguridad=norm_truncada(10,5,5,15,1)
  TSseguridad=ifelse(Nseguridad==0,Inf,TM+DSseguridad)
  # añade una persona a la cola de la siguiente etapa (pasaporte)
  Npasaporte=Npasaporte+1
  if(Npasaporte==1){# genera siguiente fin de pasaporte si no habia nadie
    u=runif(1)
    DSpasaporte=norm_truncada(10,5,5,15,1)
    TSpasaporte=TM+DSpasaporte
  }
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+Ncheckin*(TM-TANT)
  SUMAseguridad=SUMAseguridad+(Nseguridad+1)*(TM-TANT)
  SUMApasaporte=SUMApasaporte+(Npasaporte-1)*(TM-TANT)
  SUMAembarque=SUMAembarque+Nembarque*(TM-TANT)
  TANT=TM
  return(c(TSseguridad,TSpasaporte,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
           Ncheckin,Nseguridad,Npasaporte,Nembarque,TANT))
}

Pasaporte=function(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                   Ncheckin,Nseguridad,Npasaporte,Nembarque){
  # finalizacion de una persona en este servicio
  Npasaporte=Npasaporte-1
  DSpasaporte=norm_truncada(10,5,5,15,1)
  TSpasaporte=ifelse(Npasaporte==0,Inf,TM+DSpasaporte)
  # añade una persona a la cola de la siguiente etapa (embarque)
  Nembarque=Nembarque+1
  if(Nembarque==1){
    u=runif(1)
    DSembarque=norm_truncada(10,5,5,15,1)
    TSembarque=TM+DSembarque
  }
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+Ncheckin*(TM-TANT)
  SUMAseguridad=SUMAseguridad+Nseguridad*(TM-TANT)
  SUMApasaporte=SUMApasaporte+(Npasaporte+1)*(TM-TANT)
  SUMAembarque=SUMAembarque+(Nembarque-1)*(TM-TANT)
  TANT=TM
  return(c(TSpasaporte,TSembarque,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
           Ncheckin,Nseguridad,Npasaporte,Nembarque,TANT))
}

Embarque=function(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                  Ncheckin,Nseguridad,Npasaporte,Nembarque){
  # finalizacion de una persona en este servicio
  Nembarque=Nembarque-1
  DSembarque=norm_truncada(10,5,5,15,1)
  TSembarque=ifelse(Nembarque==0,Inf,TM+DSembarque)
  # actualizacion de sumas
  SUMAcheckin=SUMAcheckin+Ncheckin*(TM-TANT)
  SUMAseguridad=SUMAseguridad+Nseguridad*(TM-TANT)
  SUMApasaporte=SUMApasaporte+(Npasaporte)*(TM-TANT)
  SUMAembarque=SUMAembarque+(Nembarque+1)*(TM-TANT)
  TANT=TM
  return(c(TSembarque,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
           Ncheckin,Nseguridad,Npasaporte,Nembarque,TANT))
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
#####################################################################

#PROGRAMA PRINCIPAL
#Inicializamos las variables
set.seed(1234)
Ncheckin=0
Nseguridad=0
Npasaporte=0
Nembarque=0
TM=0
TANT=0
TScheckin=Inf
TSseguridad=Inf
TSpasaporte=Inf
TSembarque=Inf
Tmax=240

SUMAcheckin=0
SUMAseguridad=0
SUMApasaporte=0
SUMAembarque=0

set.seed(1234)
#Genero la primera llegada
DL=exp_inversa(1,189/240)
TL=DL

while(TM<Tmax){
  #Identificamos el evento que tiene lugar antes
  TM=min(TL,TScheckin,TSseguridad,TSpasaporte,TSembarque)
  Estado=1*(TM==TL)+2*(TM==TScheckin)+3*(TM==TSseguridad)+4*(TM==TSpasaporte)+5*(TM==TSembarque)
  if(Estado==1){
    k=Llegada(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
              Ncheckin,Nseguridad,Npasaporte,Nembarque)
    TL=k[1]
    TScheckin=k[2]
    SUMAcheckin=k[3]
    SUMAseguridad=k[4]
    SUMApasaporte=k[5]
    SUMAembarque=k[6]
    Ncheckin=k[7]
    Nseguridad=k[8]
    Npasaporte=k[9]
    Nembarque=k[10]
    TANT=k[11]
  }else if(Estado==2){
    k=Checkin(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
              Ncheckin,Nseguridad,Npasaporte,Nembarque)
    TScheckin=k[1]
    TSseguridad=k[2]
    SUMAcheckin=k[3]
    SUMAseguridad=k[4]
    SUMApasaporte=k[5]
    SUMAembarque=k[6]
    Ncheckin=k[7]
    Nseguridad=k[8]
    Npasaporte=k[9]
    Nembarque=k[10]
    TANT=k[11]
  }else if(Estado==3){
    k=Seguridad(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                Ncheckin,Nseguridad,Npasaporte,Nembarque)
    TSseguridad=k[1]
    TSpasaporte=k[2]
    SUMAcheckin=k[3]
    SUMAseguridad=k[4]
    SUMApasaporte=k[5]
    SUMAembarque=k[6]
    Ncheckin=k[7]
    Nseguridad=k[8]
    Npasaporte=k[9]
    Nembarque=k[10]
    TANT=k[11]
  }else if(Estado==4){
    k=Pasaporte(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
                Ncheckin,Nseguridad,Npasaporte,Nembarque)
    TSpasaporte=k[1]
    TSembarque=k[2]
    SUMAcheckin=k[3]
    SUMAseguridad=k[4]
    SUMApasaporte=k[5]
    SUMAembarque=k[6]
    Ncheckin=k[7]
    Nseguridad=k[8]
    Npasaporte=k[9]
    Nembarque=k[10]
    TANT=k[11]
  }else if(Estado==5){
    k=Embarque(TM,SUMAcheckin,SUMAseguridad,SUMApasaporte,SUMAembarque,
               Ncheckin,Nseguridad,Npasaporte,Nembarque)
    TSembarque=k[1]
    SUMAcheckin=k[2]
    SUMAseguridad=k[3]
    SUMApasaporte=k[4]
    SUMAembarque=k[5]
    Ncheckin=k[6]
    Nseguridad=k[7]
    Npasaporte=k[8]
    Nembarque=k[9]
    TANT=k[10]
  }
}
print(c("En el instante de parada había en la primera etapa",Ncheckin,"personas"))
print(c("En el instante de parada había en la segunda etapa",Nseguridad,"personas"))
print(c("En el instante de parada había en la tercera etapa",Npasaporte,"personas"))
print(c("En el instante de parada había en la cuarta etapa",Nembarque,"personas"))
