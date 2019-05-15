###########  TSP
rm(list=ls())

setwd("C:/Escuela/Actuaría Oyente/Seminario 2")
dir()

coords<-read.csv("coordenadas_ciudades.csv", header = TRUE)




#COORDENADAS Y DISTANCIAS OBTENIDAS DE GOOGLE MAPS
# patzcuaro(19.513725,-101.609233)-gdl(20.686583,-103.350839): : 221,83 km 
# patzcuaro-cdmx(19.303921, -99.180256) :255,74 km
# patzcuaro-cuernavaca(18.927971, -99.214819) :259,60 km
# patzcuaro-acapulco(16.846578, -99.832163) :350,98 km
# patzcuaro-oaxaca(17.079247, -96.712171) :583,19 km
# patzcuaro-guanajuato(21.020876, -101.252029) : 170,98 km
# patzcuaro-celaya(20.542262, -100.811086):140,25 km
# patzcuaro-puertoescondido(15.863415, -97.075975): 628,64 km
# patzcuaro-puertovallarta(20.661581, -105.217828):397,47 km
# 
# cdmx-cuernavaca:41,75 km
# cdmx-guanajuato:288,52 k
# cdmx-celaya:218,29 km
# cdmx-acapulco:280,76 km
# cdmx-oaxaca:361,71 km 
# cdmx-puertoescondido:442,39 km
# cdmx-puertovallarta:645,98 km
# cdmx-gdl: 460,07 km
# 
# cuernavaca-gdl:473,79 km
# cuerna-ptovallarta:657,67 km
# cuerna-acapulco:238,89 km
# cuerna-gto: 315,64 km
# cuerna-celaya: 244,06 km
# cuerna-oaxaca: 333,95 km
# cuerna-ptoescondido:408,29 km
# 
# gdl-ptovallarta: 195,46 km
# gdl-gto: 221,13 km
# gdl-celaya:265,14 km
# gdl-acapulco:563,79 km
# gdl-oaxaca:802,08 km
# gdl-ptoescondido: 850,72 km
# 
# ptovallarta-gto:414,13 km
# ptovallarta-celaya:460,51 km
# ptovallarta-oaxaca:978,53 km
# ptovallarta-ptoescondido: 1.010,90 km
# ptovallarta-acapulco:708,34 km 
# 
# gto-celaya:71,67 km
# gto-acapulco:487,31 km
# gto-oaxaca:647,45 km
# gto-ptoescondido:721,57 km
# 
# celaya-acapulco:422,71 km
# celaya-oaxaca:576,86 km
# celaya-ptoescondido:651,52 km
# 
# acapulco-oaxaca:329,83 km
# acapulco-ptoescondido:312,09 km
# 
# oaxaca-ptoescondido:139,27 km
# 
# 1.acapulco
# 2.celaya
# 3.cdmx
# 4.cuernavaca
# 5.guadalajara
# 6.guanajuato
# 7.oaxaca
# 8.patzcuaro
# 9.puertoescondido
# 10.puertovallarta
# 



distancia<-function(long1,lat1,long2,lat2){
  r = 6371
  c = pi/180
  d = 2*r*asin(sqrt(sin(c*(lat2-lat1)/2)^2+cos(c*lat1)*cos(c*lat2)*sin(c*(long2-long1)/2)^2))
  return(d)           
  
}


distancia_de<-function(coords,long,lat){
  matriz<-c()
  for(i in 1:10){
    matriz<-c(matriz,distancia(long,lat,coords[i,2],coords[i,3]))
    
  }
  
  return(matriz)
}

acapulco<-distancia_de(coords,-99.832163,16.846578)
celaya<-distancia_de(coords,-100.811086,20.542262)
cdmx<-distancia_de(coords,-99.180256,19.303921)
cuerna<-distancia_de(coords,-99.214819,18.927971)
gdl<-distancia_de(coords,-103.350839,20.686583)
gto<-distancia_de(coords,-101.252029,21.020876)
oax<-distancia_de(coords,-96.712171,17.079247)
patz<-distancia_de(coords,-101.609233,19.513725)
ptoesc<-distancia_de(coords,-97.075975,15.863415)
ptovall<-distancia_de(coords,-105.217828,20.661581)


# Matriz de distancias
matriz<-trunc(rbind(acapulco,celaya,cdmx,cuerna,gdl,gto,oax,patz,ptoesc,ptovall)*1000)/1000
row.names(matriz)<-NULL
View(matriz)

library(combinat)

# rutas<-permn(10)
# View(rutas)
# rutas[[1]][1]

lr<-function(M){
 nombres <- c("Aca","Celaya","CDMX","Cuerna","Gdl","Gto","Oax","Patz","Pto. Esc.","Pto. Vallarta")
  if(ncol(M)!=nrow(M))
   break
 rutas<-permn(nrow(M))
 d<-NULL
 r<-NULL
 for(i in 1:length(rutas)){
   r<-rutas[[i]]
   c<-NULL
   for(j in 1:nrow(M)){
     if((j+1)<=nrow(M)){
       c<-c(c,M[r[j],r[j+1]]) 
     }
     else if((j+1)==(nrow(M)+1)){
       c<-c(c,M[r[j],r[1]])
     }
     
     d[i]<-sum(c)
   }
 }
 ropt<-which(d==min(d))
 rta<-NULL
 ro<-c(rutas[[ropt[1]]],rutas[[ropt[1]]][1])
 for(i in 1:length(ro)){
   rta[i]<-nombres[ro[i]]
 }
 print("La ruta más corta es: ")
 print(rta)
 print("o alguna equivalente")
 ret<-list(NULL)
 for(i in 1:length(ropt)){
   ret[[i]]<-c(rutas[[ropt[i]]],rutas[[ropt[i]]][1])
 }
 invisible(list(rutas_eq = ret,distancia_mínima = min(d))) 
}

# s<-matrix(1:25,5)
# s[lower.tri(s)] = t(s)[lower.tri(s)]
# diag(s)<-0
# s
# lr(s)
# a<-permn(nrow(s))
# d<-NULL
# r<-NULL
# for(i in 1:length(a)){
#   r<-a[[i]]
#   c<-NULL
#   for(j in 1:nrow(s)){
#     if((j+1)<=nrow(s)){
#       c<-c(c,s[r[j],r[j+1]])
#     }
#     d[i]<-sum(c)
#   }
# }
# d
# which(d==min(d))
# ropt<-which(d==min(d))
# print("La ruta más corta es: ")
# print(a[[ropt[1]]])
# print("o alguna equivalente")
# ret<-list(NULL)
# for(i in 1:length(ropt)){
#   ret[[i]]<-a[[ropt[i]]]
# }
# a[[ropt[3]]]
# ret
# lr(s)
# b<-lr(s)
# b

M<-lr(matriz)
M

# Obs obtenemos 20 rutas equivalentes pues como debemos regresar a la misma ciudad hay 10 permutaciones iguales
# y por cada permutación hay dos rutas simétricas así que el número de tours no es 10! es 9!