#Carga la data

library(readr)
puntajes <- read_delim(file.choose(), 
                       "\t", escape_double = FALSE, trim_ws = TRUE)

print(puntajes)

Puntajes<-puntajes$respuesta
Metodo<-factor(puntajes$metodo)
Bloque<-factor(puntajes$bloque)

addmargins(table(Metodo,Bloque),1)  #Caso balanceado una réplica

#Test de Tukey

(a<-length(levels(Metodo))) #Niveles del factor Metodo
(b<-length(levels(Bloque))) #Niveles de la variable bloque

library(dae)
options(contrasts = c("contr.sum", "contr.sum"))
Error.aov<-aov(Puntajes~Metodo+Bloque+Error(Metodo/Bloque))

(Tukey<-tukey.1df(aov.obj=Error.aov,data=puntajes,error.term='Metodo:Bloque'))

Tukey$Tukey.F>qf(0.95,1,a*b-a-b)  

#Utilizando un 95% de confianza la interacción entre el bloque y el método no es significativa


#### Eficiencia de la variable bloque

r<-length(levels(Metodo))  #Niveles de tratamientos
n<-length(levels(Bloque))  #Niveles asociados a la variable bloque

sigmar<-((n-1)*anova(aditivo)[2,3]+n*(r-1)*anova(aditivo)[3,3])/(r*n-1)

sigmab<- anova(aditivo)[3,3]  #MCE

(E<-sigmar/sigmab)  #Eficiencia


#Se requiere 3.084191 veces el tamaño muestral en una muestra completamente aleatorizada 
# para explicar la misma variabilidad o precision que un diseño de bloque


#Eficiencia corregida por grados de libertad

gl1<-r*(n-1)
gl2<-(r-1)*(n-1)

(Ecorregida<-(gl2+1)*(gl1+3)*E/((gl2+3)*(gl1+1)))



### Analisis de residuos

par(mfrow=c(1,4))

plot(residuals(aditivo), fitted(aditivo), main="Ajustados vs residuos",xlab="Residuos",ylab="Ajustados")

qqnorm(residuals(aditivo),main="Gráfico cuantil-cuantil de residuos",xlab="Cuantiles teóricos distribución normal",ylab="Cuantiles de los residuos")
qqline(residuals(aditivo))

boxplot(residuals(aditivo),main="Boxplot de los residuos")
boxplot(residuals(aditivo)~Bloque,main="Boxplot de los residuos por bloque")



####### Ejercicio 2


library(readxl)
Plantacion <- read_excel(file.choose())

print(Plantacion)


Bloque<-factor(Plantacion$Block)

levels(Bloque)

Fert<-factor(paste("Fertilizante",Plantacion$Fert))

levels(Fert)

Agua<-factor(Plantacion$Water)

levels(Agua)

Produccion<-Plantacion$Yield



table(table(Fert,Agua,Bloque))  #Una replica dentro de cada bloque

a<-length(levels(Fert))
b<-length(levels(Agua))

#En cada bloque hay a*b observaciones

a*b

#Analisis intra bloques

df<-data.frame(Agua,Fert,Bloque,Produccion)

df%>%group_by(Bloque)%>%summarise(desviacion=sd(Produccion),
                                       Media=mean(Produccion),
                                       n=length(Produccion),
                                       Maximo=max(Produccion),
                                       Minimo=min(Produccion))

boxplot(Produccion~Bloque)



#Modelo 


contrasts(Bloque)<-contr.sum
contrasts(Agua)<-contr.sum
contrasts(Fert)<-contr.sum


modelo<-aov(Produccion~Agua*Fert+Bloque)

anova(modelo)


(r<-a*b)   # existen r=a*b tratamientos

#Existen r-1 grados asociados a los tratamientos

a - 1 # Grados asociados al factor fert
b - 1 # Grados asociados al factor agua
(a-1)*(b-1)   #Grados asociados a la interacción de Agua y fertilizante

#Si se suman, es equivalente a r-1:


((a-1)+(b-1)+(a-1)*(b-1))==(r-1)


### Analisis de los efectos

coef(modelo)

# El intercepto corresponde a la media global de la variable Produccion de mazorca

levels(Agua)

# Agua 1 corresponde al efecto asociado a un agua filtrada en la producción dejando 
# como referencia en el factor Agua, al agua no filtrada (minerales)

#Note que Agua filtrada posee un efecto estimado de 0.427777 y por las restricciones
# de identificabilidad, Agua no filtrada (o mineral) tendrá como efecto asociado
# -0.427777, por lo tanto, se observa un efecto negativo en la producción de mazorcas
# utilizar un agua no filtrada (con muchos minerales)

#Considerar que la celda de referencia es el Agua no filtrada (con minerales)

#Respecto al Fertilizante es posible observar que el Fertilizante 1 tiene un efecto 
# negativo -0.9777 en la produccion de mazorcas, mientras que, el fertilizante 2
# tiene un efecto positivo de 0.4388889, por lo que, podría decirse que el fertilizante
# 2 tendría un mejor efecto que el fertilizante 1 (de hecho el fertilizante 1 empeora
# el rendimiento)
#Respecto al fertilizante 3 su efecto sería -(-0.9777778+0.4388889)=0.53888889 
# el cual es positivo y mayor que el efecto del fertilizante 2, por lo tanto 
# el fertilizante 3 tendría mayor efectividad en la produccion de mazorcas

levels(Bloque)

# Se tiene que el efecto asociado al bloque C es de -(0.02222+1.488889)=-1.511109 
# que corresponde al único efecto negativo en los bloques, por lo tanto, aquellos
# terrenos o campos que se clasifican dentro del Bloque C por sus características
# son aquellos predispuestos a un menor rendimiento en términos de producción

#Y aquellos terrenos o campos caracterizados en el bloque B son aquellos con mejor
#predisposición a la produccion de mazorcas


#También se pueden observar las interacciones que corresponden al efecto en una 
# combinacion de factores


#tenemos (alphabeta)_11 y (alphabeta)_12

#Las restricciones de identificabilidad: 

#sum_{j}{(alphabeta)_jk}=0   con k=1,2

#sum_{k}{(alphabeta)_jk}=0   con j=1,2,3

#Se resuelve el "sistema" y se pueden obtener los demás.