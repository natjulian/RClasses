#### Ejercicio 1)

library(readr)
operario <- read_csv(file.choose())


#a) Modelo de medias de celda por operario


#b) 


print(operario)

table(operario$operario)  #caso balanceado

operarios<-as.factor(operario$operario)

cigarros<-operario$respuesta

modelo<-aov(cigarros~operarios)


anova(modelo)[,-c(4,5)]


(Ftrat<-anova(modelo)[1,3]/anova(modelo)[2,3])

(r<-length(levels(operarios))) #Niveles de factor operarios
(n<-unique(table(operarios)))  #Cantidad de observaciones por nivel del factor

alpha<-0.05

Ftrat>qf(p=(1-alpha),df1=(r-1),df2=r*(n-1))

#Se rechaza la hipótesis de variabilidad nula de mu con un 95% de confianza



#######################


coef(modelo)  #¿Cómo se interpretan estos coeficientes sin el contraste suma?

mean(cigarros)  #El intercepto se aleja mucho de la media global Y..

library(tidyverse)

operario %>% group_by(operario) %>% summarise(Media=mean(respuesta)) #mu_i

####################


#d) 

(Ybar<-mean(cigarros))
(MCtrat<-anova(modelo)[1,3])

(IC<-c(Ybar+c(-1,1)*qt(1-alpha/2,r-1)*sqrt(MCtrat/(n*r))))


#e) 

(MCE<-anova(modelo)[2,3])

L=(1/n)*(MCtrat/(MCE*qf(p=(1-alpha/2),df1=(r-1),df2=r*(n-1)))-1)

U=(1/n)*(MCtrat/(MCE*qf(p=(alpha/2),df1=(r-1),df2=r*(n-1)))-1)

(InC<-c(L/(L+1),U/(U+1)))



#### Ejercicio 2)

library(readr)
suicide <- read_delim("C:/Users/HP/Desktop/Anova/Ayudantía 7/suicide.csv", 
                      ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                      trim_ws = TRUE)

print(suicide)




#a) 

#Corroborar que se tiene la misma cantidad de registros:

table(table(suicide$País))  #Cada país tiene 6 registros

table(table(suicide$Continente)) #En cada continente hay 4 registros

table(table(suicide$Etario)) #Por cada rango etario hay 6 registros

table(suicide$Continente,suicide$Etario)  #Balanceado n=4 observaciones por celda


library(dplyr)

suicide%>%group_by(Continente)%>%summarise(Media=mean(Suicidios), Minimo=min(Suicidios),Maximo=max(Suicidios), Mediana=median(Suicidios), n=n())

#c) 

Continente<-factor(suicide$Continente)
Edad<-factor(suicide$Etario)

Suicidios<-suicide$Suicidios

model<-aov(Suicidios~Continente*Edad)

anova(model)[,c(1,3)]



# d) 


anova(model)[,c(1,3)]

(Fcontinente=anova(model)[1,3]/anova(model)[3,3])
(a<-length(levels(Continente)))
(b<-length(levels(Edad)))

alpha=0.1

Fcontinente>qf(p=(1-alpha),df1=(a-1),df2=(a-1)*(b-1))

#Se rechaza la hipótesis de variabilidad nula de sigma_alpha con un 90% de confianza

#Para Edad

(Fedad=anova(model)[2,3]/anova(model)[3,3])

Fedad>qf(p=(1-alpha),df1=(b-1),df2=(a-1)*(b-1))


#Se rechaza la hipótesis de variabilidad nula de sigma_beta con un 90% de confianza

#Para la interacción

(n<-unique(table(Continente,Edad)))

(Finter=anova(model)[3,3]/anova(model)[4,3])

Finter>qf(p=(1-alpha),df1=(a-1)*(b-1),df2=a*b*(n-1))

#La interaccion no resulta significativa al 90% de confianza


#d) 

aditivo<-aov(Suicidios~Continente+Edad)

coef(aditivo)

levels(Continente)
levels(Edad)

#e) 


new<-rbind(c("Europa","35-54 years"))

colnames(new)<-c("Continente","Edad")

new

predict(aditivo,newdata = data.frame(new))
