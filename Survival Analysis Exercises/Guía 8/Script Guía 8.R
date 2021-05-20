library(readr)
mortgage <- read_csv("C:/Users/HP/Desktop/Trabajo/2020-2/Análisis de Sobrevivencia/Ayudantía 8/mortgage.csv")
View(mortgage)

nrow(mortgage)

table(mortgage$id)

unique(mortgage$id) #No son unicos los id

mortgage[which(mortgage$id=="797"),c('time', 'default_time')]

mortgage[which(mortgage$id=="510"),c('time', 'default_time')]

#Se tiene el seguimiento en varios cortes de tiempo en meses

#Si existe algún tiempo tal que default_time (censura) es 1
#se debe calcular el menor tiempo tal que es uno

#Si no existe ningun tiempo tal que la censura es 1
#se debe calcular el maximo del tiempo observado

library(dplyr)

df<-mortgage%>%
  group_by(id)%>%
  mutate(censored=ifelse(max(default_time)!=0, 1,0), 
         t=ifelse(censored==1, time[which(default_time==1)[1]],max(time)))%>%
  slice(length(censored))  #Corta en rodaja a la última fila por cliente, para que las variables estén actualizadas

#En la parte del slice también se pudo haber hecho diferente, por ejemplo, si no era censurado, tomar el último registro del cliente
#Si hubo fallo, considerar exactamente la fila donde se registró primero el fallo! 

View(df)

#b)

prop.table(table(df$censored))


filter<-df%>%
  filter(censored==1)  #De 1 a 60 meses

library(ggplot2)

ggplot(filter, aes(x=t)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")+
  theme_minimal()+
  ggtitle("Distribución de tiempos en casos de incumplimiento")+
  theme(plot.title = element_text(hjust=0.5))

summary(filter$t)

library(survival)

vsurvfit <- survfit(Surv(t, censored) ~ 1, data = df)

vsurvfit


library(survminer)

ggsurvplot(fit = vsurvfit, 
           data = df,
           title = "Estimación de S(t)",
           subtitle = "Incumplimiento de hipoteca",
           risk.table = TRUE)


#d) 

##d.1) 

df$REtype_SF_orig_time<-factor(df$REtype_SF_orig_time)


contrasts(df$REtype_SF_orig_time) #0 queda como referencia

#Gráficamente se puede testear si cumple o no el supuesto de riesgos proporcionales

fit <- survfit(Surv(t, censored) ~ REtype_SF_orig_time, data = df)
plot(fit, mark.time = FALSE, xlab="Years", ylab="Survival", col = 1:2)
plot(fit, mark.time = FALSE, fun = function (s) - log(-log(s)),xlab = "Years", ylab="-log(-log(Survival))", col = 1:2)

#Se cruzan minimamente al final

#Que dos curvas se crucen significa que a partir de cierto t existe un cambio en la sobrevivencia, es decir, 
#al principio (por ejemplo) se veía una sobrevivencia mayor para el tratamiento 1 pero para cierto t_i
#la sobrevivencia es mayor para el tratamiento 2


##d.2)

coxfit<-coxph(Surv(t,censored)~hpi_time+interest_rate_time+uer_time+gdp_time+REtype_SF_orig_time, data=df)

summary(coxfit)

#Si aumenta el interest_rate_time en una unidad, el riesgo de presentar incumplimiento
#es 1.25 veces mayor

#Si el tipo de vivienda es unifamiliar el riesgo de presentar incumplimiento es
#1.01 veces mayor (pero no es significativa)

cox.zph(coxfit)

#No se cumple en ninguno el supuesto de riesgos proporcionales

