#Carga la data

library(readr)
lapaeggs <- read_csv(file.choose())
View(lapaeggs)


# ¿Cuántas réplicas por ambiente hay? 

Estacion<-factor(lapaeggs$Estacion)
Densidad<-factor(lapaeggs$Densidad)

Huevosprom<-lapaeggs$Huevos

addmargins(table(Estacion,Densidad),1)  #Caso balanceado 1 réplica


#Análisis gráfico

df<-data.frame(Huevosprom,Estacion,Densidad)

library(ggplot2)

ggplot(aes(y = Huevosprom, x = Estacion,fill=Estacion), data = df) + 
  geom_boxplot()+theme_minimal()+
  theme(axis.text.x = element_blank())+xlab("")+ggtitle("Comparativa producción de huevos promedio por Estación")

ggplot(aes(y = Huevosprom, x = Densidad,fill=Densidad), data = df) + 
  geom_boxplot()+theme_minimal()+
  theme(axis.text.x = element_blank())+xlab("")+ggtitle("Comparativa producción de huevos promedio por Densidad")


library("ggpubr")
ggboxplot(df, x = "Densidad", y = "Huevosprom", color = "Estacion",
          palette = c("#00AFBB", "#E7B800"))



#Test de Tukey de Aditividad

options(contrasts = c("contr.sum", "contr.sum"))  #Restricciones
av = aov(Huevosprom ~ Estacion+Densidad, df)      #Modelo aditivo
summary(av)                                       #Importante revisar los df de cada factor


(isBalanced=!is.list(replications(av, data=df)))  #Otra forma de verificar que es balanceado


coef(av)    #El último nivel de cada variable lo expresa en función de los demás

levels(Estacion)

#El intercepto corresponde a la media global de la produccion de huevos
#Estacion 1 corresponde al efecto de Primavera en la media de la produccion de huevos
#Densidad 1 corresponde al efecto de la densidad=8 en la media de la produccion de huevos
#Notar que los efectos Densidad2 y Densidad3 son negativos, mayor densidad disminuye 
#promedio de produccion


#Paso 1) Extraer vectores de los coeficientes estimados para cada factor

avalues<-ifelse(df$Estacion=="Primavera",unname(coef(av)[2]),-unname(coef(av)[2]))
bvalues<-ifelse(df$Densidad=="8",unname(coef(av)[3]),ifelse(df$Densidad=="15",unname(coef(av)[4]),ifelse(df$Densidad=="30",unname(coef(av)[5]),-(unname(coef(av)[3])+unname(coef(av)[4])+unname(coef(av)[5])))))


#Paso 2) Estimar D

(Dhat<-sum(avalues*bvalues*Huevosprom)/(sum(unique(avalues)^{2})*sum(unique(bvalues)^{2})))


#Paso 3) Definir SCAB*

(SCABp<-Dhat^{2}*sum(avalues^{2}*bvalues^{2}))

#Paso 4) Extraer SCA, SCB y SCT del modelo aditivio

SCA<-anova(av)[1,2]
SCB<-anova(av)[2,2]
SCT<-sum(anova(av)[,2])


#Paso 5) Definir SCE*

(SCEp<-SCT-SCA-SCB-SCABp)

#Paso 6) Definir el estadístico Fp

(a<-length(levels(Estacion)))   #Niveles del factor Estacion
(b<-length(levels(Densidad)))   #Niveles del factor Densidad

(Fp=(SCABp/1)/(SCEp/(a*b-a-b)))

#Paso 7) Regla de decisión

Fp>qf(0.95,1,a*b-a-b)    #No se rechaza la hipótesis nula

#No se rechaza la hipotesis nula, el modelo aditivo es correcto con un 95%



#En una función

#install.packages("dae")
library(dae)

Error.aov<-aov(Huevosprom~Estacion+Densidad+Error(Estacion/Densidad))

tukey.1df(aov.obj=Error.aov,data=df,error.term='Estacion:Densidad')

#Entrega los mismos resultados recien calculados


### Caso desbalanceado

lapaeggs2 <- read_csv(file.choose())
View(lapaeggs2)


Estacion2<-factor(lapaeggs2$Estacion)
Densidad2<-factor(lapaeggs2$Densidad)
Huevosprom2<-lapaeggs2$Huevos

table(Estacion2,Densidad2)

addmargins(table(Estacion2,Densidad2),1)


#Test F modelos anidados

#Paso 1) Planteo un modelo reducido (sin Densidad) y otro completo 

options(contrasts = c("contr.sum", "contr.sum"))
modelo1<-aov(Huevosprom ~ Estacion, df)  #Modelo reducido

modelo2<-aov(Huevosprom ~ Estacion+Densidad, df) 

#Paso 2) Extraigo SCEm1, SCEm2, glEm1 y glEm2

SCEm1<-anova(modelo1)[2,2]
glEm1<-anova(modelo1)[2,1]

SCEm2<-anova(modelo2)[3,2]
glEm2<-anova(modelo2)[3,1]

#Paso 3) Plantear el Estadístico F

(F=((SCEm1-SCEm2)/(glEm1-glEm2))/(SCEm2/glEm2))

#Paso 4) Regla de decisión

F>qf(0.95,glEm1-glEm2,glEm2)

#No se rechaza la hipótesis nula

#Notar que al utilizar un 90% de confianza, se obtiene otro resultado

F>qf(0.9,glEm1-glEm2,glEm2)




#Diseño de bloques

#Carga la data

library(readr)
puntajes <- read_delim(file.choose(), 
                       "\t", escape_double = FALSE, trim_ws = TRUE)
View(puntajes)


Puntajes<-puntajes$respuesta
Metodo<-factor(puntajes$metodo)
Bloque<-factor(puntajes$bloque)

addmargins(table(Metodo,Bloque),1)  #Caso balanceado una réplica


#Interaction plot

library(dplyr)

puntajes %>% 
  ggplot() +
  aes(x = Metodo, y = Puntajes, color = Bloque) +
  geom_line(aes(group = Bloque)) +
  geom_point()+theme_minimal()+ggtitle("Gráfico de interacción entre Método y Bloque para Puntajes")


#Test de Tukey

(a<-length(levels(Metodo))) #Niveles del factor Metodo
(b<-length(levels(Bloque))) #Niveles de la variable bloque


options(contrasts = c("contr.sum", "contr.sum"))
Error.aov<-aov(Puntajes~Metodo+Bloque+Error(Metodo/Bloque))

(Tukey<-tukey.1df(aov.obj=Error.aov,data=puntajes,error.term='Metodo:Bloque'))

Tukey$Tukey.F>qf(0.95,1,a*b-a-b)  

#Utilizando un 95% de confianza la interacción entre el bloque y el método no es significativa


#Test F de significancia del efecto

#Paso 1) Definir el modelo aditivo

options(contrasts = c("contr.sum", "contr.sum"))
aditivo<-aov(Puntajes~Metodo+Bloque)

df<-data.frame(Metodo,Bloque,Puntajes)

(isBalanced=!is.list(replications(aditivo, data=df))) #Es balanceado

n<-table(Metodo,Bloque)[1]


#Paso 2) Extraer SCE y definir grados de libertad
anova(aditivo)

SCmetodo<-anova(aditivo)[1,2]
SCE<-anova(aditivo)[3,2]

a<-length(levels(Metodo)) #Niveles del factor Metodo
r<-(b-1)*(a-1)    #grados de libertad asociados a SCE  

#Equivalente a a*b(n-1)

#Paso 3) Calcular MCmetodo  y MCE

MCmetodo<-SCmetodo/(a-1)

MCE<-SCE/r

#Paso 4) Calcular estadistico F

(Fmetodo<-MCmetodo/MCE)

#Paso 5) Regla de decisión

Fmetodo>qf(0.95,a-1,(b-1)*(a-1))

#Con un 95% de confianza, la metodología presenta un efecto 
# significativo en los puntajes


#Para bloque es analogo, pero la lectura es distinta

SCbloque<-anova(aditivo)[2,2]
b<-length(levels(Bloque)) #Niveles de la variable bloque

MCbloque<-SCbloque/(b-1)

(Fbloque<-MCbloque/MCE)

Fbloque>qf(0.95,b-1,(b-1)*(a-1))

