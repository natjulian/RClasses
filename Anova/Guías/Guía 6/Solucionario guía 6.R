#Pregunta 1) 


#b) 

data<-mtcars[,c("am","mpg","hp")]

names(data)<-c("Motor","Distancia","Caballos")

print(data)

dim(data)

table(data$Motor)  #ni


#install.packages("extrafont")
library(extrafont)

#font_import()
#loadfonts(device="win")     #Importa tipos de letra de windows
fonts() 

data$Caballos<- data$Caballos-mean(data$Caballos) #Covariable centrada en 0

data$Motor<-factor(data$Motor)

attach(data)

par(family="Javanese Text") 
plot(Caballos,Distancia,pch="",main="Distancia recorrida promedio por energía por Motor y Caballos de fuerza",
     xlab="Caballos de Fuerza centrado",ylab="Distancia recorrida promedio por energía", axes=FALSE, cex.main=1.5)
points(Caballos[Motor==0],Distancia[Motor==0],col="yellowgreen",pch=15)
points(Caballos[Motor==1],Distancia[Motor==1],col="darkgoldenrod",pch=15)
axis(1, at=seq(-95,190, by=5), col.axis="darkgoldenrod4", las=1)
axis(2, at=seq(10,34, by=1), col.axis="darkgoldenrod4", las=1)
text(x=160, y= 30, labels="Motor automático", col="yellowgreen", cex=1)
text(x=160, y= 28, labels="Motor manual", col="darkgoldenrod", cex=1)


#c) 


modelo <- lm(Distancia~Motor+Caballos,
              contrasts=list(Motor=contr.sum))

coef(modelo)

levels(Motor)

#Intercepto de la recta para Motor automatico:

coef(modelo)[1]+coef(modelo)[2]

#Intercepto de la recta para Motor manual: 

coef(modelo)[1]-coef(modelo)[2]  #Usar contraste suma del factor motor


#La pendiente de ambas rectas es: 

coef(modelo)[3]


#Grafico

par(family="Javanese Text") 
plot(Caballos,Distancia,pch="",main="Distancia recorrida promedio por energía por Motor y Caballos de fuerza",
     xlab="Caballos de Fuerza centrado",ylab="Distancia recorrida promedio por energía", axes=FALSE, cex.main=1.5)
points(Caballos[Motor==0],Distancia[Motor==0],col="yellowgreen",pch=15)
points(Caballos[Motor==1],Distancia[Motor==1],col="darkgoldenrod",pch=15)
axis(1, at=seq(-95,190, by=5), col.axis="darkgoldenrod4", las=1)
axis(2, at=seq(10,34, by=1), col.axis="darkgoldenrod4", las=1)
text(x=160, y= 30, labels="Motor automático", col="yellowgreen", cex=1)
text(x=160, y= 28, labels="Motor manual", col="darkgoldenrod", cex=1)
abline(a=(coef(modelo)[1]+coef(modelo)[2]),b=modelo$coefficients[3],col="yellowgreen", lwd=2)
abline(a=(coef(modelo)[1]-coef(modelo)[2]),b=modelo$coefficients[3],col="darkgoldenrod", lwd=2)


#d) 


anova(modelo)             #Suma cuadrática secuencial 
                          #SS(A), SS(B|A) El orden importa!

summary(modelo)

#e) 


## En un caso balanceado, anova tipo I, II y III son coincidentes

library(car)
Anova(modelo, type="II")  #Suma cuadrática jerárquica 
                          #SS(A|B) y SS(B|A)


# Anova tipo III es para el caso con interacción significativa: 

Anova(modelo, type="III")  #Si no existe interacción equivale al tipo II
                          #Si existe interacción entrega:
                              #SS(A|B, AB) y SS(B|A,AB)


modelo1<-lm(Distancia~Motor,
            contrasts=list(Motor=contr.sum))

modelo2<-lm(Distancia~Caballos)

anova(modelo, modelo1)  #Modelo completo versus modelo con Motor

#Notar que el valor-p coincide con valor-p de la tabla tipo II

anova(modelo, modelo2)  #Modelo completo versus modelo con Caballos

#Notar que el valor-p coincide con valor-p de la tabla tipo II


#Podriamos probar con la interaccion:


modelointer <- lm(Distancia~Motor*Caballos,
             contrasts=list(Motor=contr.sum))

anova(modelointer, modelo)  #Interacción se asume no significativa



#f) 

summary(modelo)$adj.r.squared  #Considerando motor y caballos de fuerza

summary(modelo1)$adj.r.squared   #Considerando solo motor


#g) Supuestos


modeloauto<-lm(Distancia~Caballos, data=data[Motor==0,])

modelomanual<-lm(Distancia~Caballos, data=data[Motor==1,])


#install.packages("ggfortify")

library(ggfortify)

autoplot(modeloauto)

autoplot(modelomanual)

# Normalidad

shapiro.test(modeloauto$residuals);shapiro.test(modelomanual$residuals)


#Homocedasticidad:

library(lmtest)

bptest(modeloauto);bptest(modelomanual) 


#residuos intra grupo se comportan bien

anova(modeloauto)[2,3]; anova(modelomanual)[2,3]  #MCE

#No habria homocedasticidad entre grupos


###### Adicionalmente:

coef(modeloauto)
coef(modelomanual)

confint(modeloauto)
confint(modelomanual)

#No habria evidencia para no pensar que las rectas son
#paralelas


#Pregunta 2) 

data <- data.frame(trigl = c(142.3, 144.0, 148.6, 146.9, 142.9, 147.4, 133.8, 133.2, 134.9, 146.3, 145.2, 146.3, 125.9, 127.6, 108.9, 107.5, 148.6, 156.5, 148.6, 153.1, 135.5, 138.9, 132.1, 149.7, 152.0, 151.4, 149.7, 152.0, 142.9, 142.3, 141.7, 141.2), dia = factor(rep(1:4, each = 8)), maquina = factor(rep(rep(1:4, each = 2), 2)))


table(data$dia, data$maquina) #Balanceado

#b)

fonts() 
par(family="Castellar" ) 
with(data, interaction.plot(x.factor = dia, trace.factor = maquina, response = trigl, main="Interacción entre máquina y día en mediciones de triglicéridos"))

#c) 

model<-aov(trigl~dia*maquina, data=data)

anova(model)[,1:3]


#Factor dia

(Fdia=anova(model)[1,3]/anova(model)[3,3])


(a<-length(levels(data$dia)))

(b<-length(levels(data$maquina)))

alpha=0.05

Fdia>qf(p=(1-alpha),df1=(a-1),df2=(a-1)*(b-1))

#Factor maquina


(Fmaquina=anova(model)[2,3]/anova(model)[3,3])

Fmaquina>qf(p=(1-alpha),df1=(b-1),df2=(a-1)*(b-1))


#Efecto conjunto

(n<-unique(table(data$dia,data$maquina)))

(Finter=anova(model)[3,3]/anova(model)[4,3])


Finter>qf(p=(1-alpha),df1=(a-1)*(b-1),df2=a*b*(n-1))


#d) 

anova(model)[,c(1,3)]


#e) 

library(lme4)

randomeffects <- lmer(trigl ~ (1 | dia) + (1 | maquina) + (1 | maquina:dia), data = data)

summary(randomeffects)

