### Ejercicio 1) Máquinas de ensamblaje + Estación de ensamblaje


#b)

(error<-c(2.3, 3.4, 3.5, 3.5, 2.6, 3.6, 2.4, 2.7, 2.8, 3.7, 2.8, 3.7, 3.9, 3.9, 3.4, 3.5, 3.2, 3.5, 3.1, 3.2, 3.5, 3.3, 3.4, 3.5, 2.6, 2.6, 2.5))
(machine<-factor(rep(c(1,2,3), each=3, length=27)))
(estacion<-factor(rep(c(1,2,3),each=9)))

model<-aov(error~machine*estacion)

anova(model)[,1:3]


(Fmachine<-anova(model)[1,3]/anova(model)[3,3])

(a<-length(levels(machine)))
(b<-length(levels(estacion)))

(n<-unique(table(machine,estacion)))  #Caso balanceado

alpha=0.05

Fmachine>qf(p=(1-alpha),df1=(a-1),df2=(a-1)*(b-1))


(Festacion=anova(model)[2,3]/anova(model)[3,3])


Festacion>qf(p=(1-alpha),df1=(b-1),df2=(a-1)*(b-1))


(Finter=anova(model)[3,3]/anova(model)[4,3])

Finter>qf(p=(1-alpha),df1=(a-1)*(b-1),df2=a*b*(n-1))


#c) 

aditivo<-aov(error~machine+estacion)

anova(aditivo)[,1:3]


#d) 


t.test(error, mu=5, alternative=c("greater"))


### Ejercicio 2) Cigarros de calidad ~ Operario + Tabaco


#b) 
library(readr)
tabaco <- read_csv(file.choose())


library(tidyverse)

glimpse(tabaco)

cigarros<-tabaco$respuesta
maquina<-factor(tabaco$maquina)
operario<-factor(tabaco$operario)

contrasts(maquina)<-contr.sum

inter<-aov(cigarros~maquina*operario)

anova(inter)[,1:3]


#c) 


#Tests F
alpha=0.05

(a<-length(levels(maquina)))
(b<-length(levels(operario)))

(n<-unique(table(operario, maquina)))  #Caso balanceado

(FAB=anova(inter)[3,3]/anova(inter)[4,3])

FAB>qf((1-alpha), (a-1)*(b-1), a*b*(n-1))


#d) 


aditivo<-aov(cigarros~maquina+operario)

anova(aditivo)[,1:3]


(FA=anova(aditivo)[1,3]/anova(aditivo)[3,3])

FA>qf((1-alpha), (a-1), a*b*(n-1)+(a-1)*(b-1))


(FB=anova(aditivo)[2,3]/anova(aditivo)[3,3])

FB>qf((1-alpha), (b-1), a*b*(n-1)+(a-1)*(b-1))


#e) 

MCB<-anova(aditivo)[2,3]
MCE<-anova(aditivo)[3,3]

(sigmahat<-(MCB-MCE)/(n*a))

glE<-a*b*(n-1)+(a-1)*(b-1)


(gl<-((sigmahat)^{2}*(n*a)^{2})/(MCB^{2}/(b-1) + MCE^2/glE))
(gl<-round(gl)) #Debe aproximarse pues se utiliza como grado de libertad


#El intervalo es:
c(gl*sigma/qchisq(0.975,gl), gl*sigma/qchisq(0.025,gl))



#Pregunta 3) Propuesto

library(readr)
promociones <- read_delim(file.choose(), 
                          "\t", escape_double = FALSE, trim_ws = TRUE)


glimpse(promociones)

attach(promociones)


#install.packages("extrafont")
library(extrafont)

#font_import()
#loadfonts(device="win")     #Importa tipos de letra de windows
fonts() 


par(family ="Nirmala UI") 

plot(montopublicidad,numero,pch="",main="Publicidad vs Número de Productos",
     xlab="Gasto en Publicidad",ylab="Número de Productos Vendidos", cex.main=2, axes=FALSE)
points(montopublicidad[promocion==1],numero[promocion==1],col="darkorchid1",pch=15)
points(montopublicidad[promocion==2],numero[promocion==2],col="goldenrod2",pch=15)
points(montopublicidad[promocion==3],numero[promocion==3],col="deeppink2",pch=15)
text(x=33, y= 30, labels="Promoción 1", col="darkorchid1", cex=1)
text(x=33, y= 28, labels="Promoción 2", col="goldenrod2", cex=1)
text(x=33, y= 26, labels="Promoción 3", col="deeppink2", cex=1)

axis(1, at=seq(16,35, by=1), col.axis="darkmagenta", las=1)
axis(2, at=seq(21,45, by=2), col.axis="darkmagenta", las=1)



#b)


promocion<-as.factor(promocion)
publicidad.c<-montopublicidad-mean(montopublicidad)

contrasts(promocion)<-contr.sum

modelo<-lm(numero~publicidad.c+promocion)

modelo$contrasts
modelo$coefficients

(tauprom3<--(6.0174070+0.9420168))

#Intercepto para la promocion 1
(intercepto.1<-coef(modelo)[1]+coef(modelo)[3])

#Intercepto para la promoción 2
(intercepto.2<-coef(modelo)[1]+coef(modelo)[4])

#Intercepto para la promoción 3
(intercepto.3<-coef(modelo)[1]+tauprom3)

#Gráfico

par(family = "Modern No. 20") 
plot(publicidad.c,numero,pch="",main="Publicidad vs. Número de Productos",
     xlab="Gasto en Publicidad Centrado",ylab="Número de Productos Vendidos", axes=FALSE, cex.main=2)
abline(intercepto.1,modelo$coefficients[2],col="darkorchid1", lwd=2)
abline(intercepto.2,modelo$coefficients[2],col="goldenrod2", lwd=2)
abline(intercepto.3,modelo$coefficients[2],col="deeppink2", lwd=2)
text(x=7, y= 27,labels="Promoción 1", col="darkorchid1", cex=1)
text(x=7, y= 25,labels="Promoción 2", col="goldenrod2", cex=1)
text(x=7, y= 23,labels="Promoción 3", col="deeppink2", cex=1)
axis(1, at=seq(-9,9, by=1), col.axis="darkmagenta", las=1)
axis(2, at=seq(21,45, by=2), col.axis="darkmagenta", las=1)




par(family = "Ink Free") 
plot(publicidad.c,numero,pch="",main="Publicidad vs. Número de Productos",
     xlab="Gasto en Publicidad Centrado",ylab="Número de Productos Vendidos", axes=FALSE, cex.main=2)
points(publicidad.c[promocion==1],numero[promocion==1],col="darkorchid1",pch=15)
points(publicidad.c[promocion==2],numero[promocion==2],col="goldenrod2",pch=15)
points(publicidad.c[promocion==3],numero[promocion==3],col="deeppink2",pch=15)
abline(intercepto.1,modelo$coefficients[2],col="darkorchid1", lwd=2)
abline(intercepto.2,modelo$coefficients[2],col="goldenrod2", lwd=2)
abline(intercepto.3,modelo$coefficients[2],col="deeppink2", lwd=2)
text(x=7, y= 27,labels="Promoción 1", col="darkorchid1", cex=2)
text(x=7, y= 25,labels="Promoción 2", col="goldenrod2", cex=2)
text(x=7, y= 23,labels="Promoción 3", col="deeppink2", cex=2)
axis(1, at=seq(-9,9, by=1), col.axis="darkmagenta", las=1)
axis(2, at=seq(21,45, by=2), col.axis="darkmagenta", las=1)

