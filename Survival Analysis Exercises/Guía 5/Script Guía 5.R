#install.packages("rio")

library(rio)

#Carga los datos

cirrosis<-import(file.choose()) #Esta función es muy útil, carga archivos de distinto tipo
                                #siempre y cuando las columnas estén bien separadas

#Se definen como factores:

cirrosis$Sexo<-as.factor(cirrosis$Sexo)
cirrosis$Ascitis<-as.factor(cirrosis$Ascitis)
cirrosis$Hepatomegalia<-as.factor(cirrosis$Hepatomegalia)
cirrosis$Aranasvasculares<-as.factor(cirrosis$Aranasvasculares)
cirrosis$Edemas<-as.factor(cirrosis$Edemas)
cirrosis$Etapadelacirrosis<-as.factor(cirrosis$Etapadelacirrosis)
cirrosis$Tratamiento<-as.factor(ifelse(cirrosis$Tratamiento==1, "D-Penicilamina", "Placebo"))

#Notar que se están utilizando los contrastes tratamiento:

contrasts(cirrosis$Sexo)

#install.packages("flexsurv")

library(flexsurv)

#log(Y)~Valores extremos equivalente a distribución Weibull en Y
extreme<-flexsurvreg(Surv(t, Censura)~ Tratamiento+Cobreenorina+Sexo+Etapadelacirrosis,
                     data=cirrosis, dist = "weibull")


#Equivalente a: survreg(Surv(t, Censura)~ Tratamiento+Cobreenorina+Sexo+Etapadelacirrosis, data=cirrosis, dist = "weibull")

#log(Y)~Normal equivalente a distribución log-normal en Y:
gaussian<-flexsurvreg(Surv(t, Censura)~ Tratamiento+Cobreenorina+Sexo+Etapadelacirrosis,
                     data=cirrosis, dist = "lognormal")

#Equivalente a: survreg(Surv(t, Censura)~ Tratamiento+Cobreenorina+Sexo+Etapadelacirrosis, data=cirrosis, dist = "lognormal")

#log(Y)~Logistica equivalente a log-logistica en Y:

logistic<-flexsurvreg(Surv(t, Censura)~ Tratamiento+Cobreenorina+Sexo+Etapadelacirrosis,
                      data=cirrosis, dist = "llogis")

#Equivalente a survreg(Surv(t, Censura)~ Tratamiento+Cobreenorina+Sexo+Etapadelacirrosis, data=cirrosis, dist = "loglogistic")

names(gaussian)


### Gráfico

#install.packages("extrafont")
library(extrafont)

#font_import() #Importa los tipos de letra (tarda un poco)
#loadfonts()      
fonts() #Muestra las que hay disponibles

par(family = "Century Schoolbook") #Se define el tipo de letra

cols <- c("lightblue3", "greenyellow", "sandybrown")
plot(extreme, col = cols[1], lwd=3, lwd.obs = 1, lty.obs=5, xlab = "Times", ylab = "Survival Probability", main="AFT model comparison",axes=FALSE)
lines(gaussian, col = cols[2], lty = 2, lwd=3)
lines(logistic, col = cols[3], lty=3, lwd=3)
legend("bottomleft", col = c("black", cols[1], cols[2], cols[3]), 
       lty = c(1, 1, 2, 4), bty = "n", lwd = c(1, 3, 3, 3),
       c("Kaplan-Meier", "Weibull (AFT)", "Gaussian (AFT)",
         "Logistic (AFT)"), cex=0.8)
axis(2, at=seq(0,1, by=0.1), las=1)
axis(1, at=seq(0,max(cirrosis$t), by=400), las=1)

coef(extreme)
coef(gaussian)
coef(logistic)

#Estimaciones y varianzas asociadas de los parámetros:

coef.extreme<-paste(round(coef(extreme)[3:8], 4), "(", round(diag(vcov(extreme)[3:8,3:8]), 6), ")", sep="")
coef.gaussian<-paste(round(coef(gaussian)[3:8], 4), "(", round(diag(vcov(gaussian)[3:8,3:8]), 6), ")", sep="")
coef.logistic<-paste(round(coef(logistic)[3:8], 4), "(", round(diag(vcov(logistic)[3:8,3:8]), 6), ")", sep="")

tabla<-data.frame(coef.extreme, coef.gaussian, coef.logistic)

rownames(tabla)<-names(coef(logistic)[3:8])

tabla

#Los coeficientes estimados difieren, sobretodo para el modelo de valores 
#extremos.

AIC(extreme)
AIC(gaussian)
AIC(logistic)

extreme$loglik
gaussian$loglik
logistic$loglik

#Primero debemos plantear el modelo "grande" o no reducido incluyendo el término de interés:
full<-survreg(Surv(t, Censura)~ Tratamiento+Cobreenorina+Sexo+Etapadelacirrosis+Tratamiento*Sexo, data=cirrosis, dist = "weibull")

#Luego planteamos el modelo reducido o "pequeño" o "anidado"

small<-survreg(Surv(t, Censura)~ Tratamiento+Cobreenorina+Sexo+Etapadelacirrosis, data=cirrosis, dist = "weibull")

#Planteamos el test utilizando anova() (para modelos anidados)

#Da lo mismo el orden:

anova(full, small)
anova(small, full)

#Hipotesis nula: beta(interaccion)=0
#Hipotesis alternativa: beta(interaccion)!=0

#Valor-p=0.16, no se incluye interacción.


## La hipotesis nula es que el modelo reducido es verdadero, beta asociado a la interacción es 0

## No se rechaza la hipótesis nula. No se incluye interacción

# Calcula residuos AFT en base al modelo
resids <- (log(small$y[, 1]) - small$linear.predictors) / small$scale

#Los residuos tambien son censurados:

resKM <- survfit(Surv(resids, Censura) ~ 1, data = cirrosis)

#Grafica el estimador de Kaplan-Meier
plot(resKM, mark.time = FALSE, xlab="AFT residuals", ylab="Survival Probability")

#Grafica la funcion de sobrevivencia de la distribucion de valores
#extremos
xx <- seq(min(resids), max(resids), length.out = 35)
yy <- exp(- exp(xx))
lines(xx, yy, col = "red", lwd = 2)


legend("bottomleft", col = c("black", "black", "red"), 
       lty = c(1, 2, 1), bty = "n", lwd = 1,
       c("Kaplan-Meier", "95% CI KM Estimate", "Survival for Extreme Value distribution"), cex=0.8)



#Link complementos para el paquete flexsurv:

# https://www.jstatsoft.org/article/view/v070i08 

