## Ejercicio aplicado 

library("KMsurv")
data("larynx")

View(larynx)

?larynx

str(larynx)



#a) 

larynx$stage<-factor(larynx$stage)

library(survival) #Para no considerar la censura:

fit <- survreg(Surv(time)~ stage + age+diagyr , larynx, dist='lognormal')
coef(fit)

fit.lm <- lm(log(time) ~ stage + age+diagyr , larynx)
coef(fit.lm)

#En ausencia de censura, es posible utilizar el estimador de OLS usual.

#Se obtienen las mismas estimaciones para beta dado que el modelo que planteamos
#con survreg y con lm son, en esencia, los mismos:

# y_i=x_{i}^{t}\beta+\epsilon; con y_i=log(t_i) y asumiendo epsilon ~ N(0,\sigma^{2})

#Ambas funciones realizan lo mismo (dado que no estamos considerando la censura),
# por esto obtenemos beta_fit=beta_lm.

#Pero, si hubieramos utilizado modelos diferentes, y los coeficientes son los mismos
#esto no quiere decir que son equivalentes, acá partimos de la premisa que en el fondo
#ambos modelos son equivalentes por su estructura y por eso las estimaciones de beta
#son iguales!

#b) 

fitln <- survreg(Surv(time, delta)~ stage + age+diagyr , larynx, dist='lognormal')
coef(fitln)


# log(T)= Intercepto+betastage2+....

#El coeficiente de etapa 2 es -0.2511 es decir:

#Para pacientes con igual edad y año de diagnostico: 

#El logaritmo del tiempo de sobrevivencia es 0.25 menor en promedio para los
# pacientes con etapa de cancer de laringe 2 respecto a pacientes en etapa 1.

#El logaritmo del tiempo de sobrevivencia es 0.93 menor en promedio para los
# pacientes con etapa de cancer de laringe 3 respecto a pacientes en etapa 1.

#El logaritmo del tiempo de sobrevivencia es 1.9 menor en promedio para los
# pacientes con etapa de cancer de laringe 2 respecto a pacientes en etapa 1.

# Si se aplica exp (fijando age y diagyr)

#El tiempo medio de sobrevivencia para los pacientes en etapa 2 de cancer de 
#laringe es

exp(coef(fitln)[2])

#veces el tiempo medio de pacientes con etapa 1


#El tiempo medio de sobrevivencia para los pacientes en etapa 3 de cancer de 
#laringe es

exp(coef(fitln)[3])

#veces el tiempo medio de pacientes con etapa 1

#El tiempo medio de sobrevivencia para los pacientes en etapa 4de cancer de 
#laringe es

exp(coef(fitln)[4])

#veces el tiempo medio de pacientes con etapa 1


#c) 

#Una opcion es Weibull: 

fitextw <- survreg(Surv(time, delta)~ stage+age+diagyr , larynx, dist='weibull')

#Otra opcion es exp:

fitextex <- survreg(Surv(time, delta)~ stage+age+diagyr , larynx, dist='exponential')

#Coeficientes

cbind(coef(fitln), coef(fitextw), coef(fitextex))

AIC(fitln);AIC(fitextw); AIC(fitextex)

summary(fitln)
summary(fitextw)
summary(fitextex)
