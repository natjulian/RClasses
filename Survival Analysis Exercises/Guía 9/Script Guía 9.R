library(readr)
mortgage <- read_csv(file.choose())

nrow(mortgage)

table(mortgage$id)   #Recordemos que tenemos info de seguimiento/longitudinal de clientes

mortgage[which(mortgage$id=="797"),c('time', 'default_time', 'gdp_time')]

#Variables tiempo-dependientes, ¿cuándo usarlas?
# - Tratamientos cambian en el tiempo
# - Variables de exposición que cambian en el tiempo (tener/no tener)
# - Mediciones longitudinales a nivel ID (paciente, cliente, etcétera)


#a) 

#Existen variables exógenas (el valor de la covariable no es afectada por el evento)
## Ejemplo: El evento es encontrar trabajo y la covariable es el % de desempleo nacional

#Variables endógenas (el valor de la covariable sí se podría ver afectada por el evento)
## Ejemplo: El evento es embarazarse y la covariable es el conteo de hormonas 

#Caracterizar qué tipo de variables tenemos es FUNDAMENTAL para especificar el modelo.
#Si incluimos variables endógenas así tal cual, podríamos estar concluyendo erróneamente,
#induciendo ruido.

#En este contexto, la variable gdp no se podría ver afectada por el incumplimiento o no 
#imcumplimiento del pago de la hipoteca! Es una variable a nivel "macro" no dependiente 
#del sujeto. Por lo tanto es exógena (o externa).

mortgage[which(mortgage$id=="3"),c('time', 'default_time', 'gdp_time')]

#b) 

#Los datos deben ser reestructurados de modo de tener start, stop, evento y covariable Z(t).

data<-mortgage[,c('id','time', 'default_time', 'gdp_time')]

library(dplyr)

#### Si se asume que se miró el lapso desde 0 hasta el primer tiempo y luego de 
#### a meses

#Por ejemplo, un cliente se observó desde el tiempo 0 hasta 25 y de ahí en adelante
#se registró durante lapsos de un mes

data<-data %>%
  arrange(id, time)%>% #Ordenar los tiempos
  group_by(id)%>%
  mutate(ind = row_number(), #Indica el número de fila
         mint=min(time), #Calculo el mínimo tiempo observado por cliente
         maxt=max(time), #Calculo el máximo tiempo observado por cliente 
         stop=ifelse(time==mint, mint, time), #stop es el tiempo hasta que se observó
         start=ifelse(time==mint, 0, stop[ind-1]-1))%>%
  select(id, start,stop,default_time, gdp_time, time)

View(data)

#c) 

library(survival) #Para la función coxph

badapproach<- coxph(Surv(time, default_time) ~ gdp_time, data = data)
#Modelo PH Cox con exp(beta*x_i)  (Modelo tiempo independiente)

model <- coxph(Surv(start, stop, default_time) ~ gdp_time, data = data)
#Modelo Cox con exp(alpha*Z_i(t)) (Modelo tiempo dependiente)

summary(badapproach)
summary(model)


#Ya no se le suele llamar riesgos proporcionales porque el HR no es constante
#respecto al tiempo

#Un modelo de Cox (covariable dependiente del tiempo) compararía el riesgo de
#un evento por valor de gdp en cada momento del evento (t)

#Un modelo de Cox PH (covariable independiente del tiempo) compara las 
#distribuciones de sobrevivencia para incrementos de valores de gdp