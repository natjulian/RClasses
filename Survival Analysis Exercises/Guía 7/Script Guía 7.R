#Fijo mi directorio (carpeta donde se encuentran los datos)

setwd("C:/Users/HP/Desktop/Trabajo/2020-2/An�lisis de Sobrevivencia/Ayudant�a 7")

library(readr)  #Cargo el paquete para abrir el archivo 

projects <- read_csv("proj.csv")

View(projects)

nrow(projects);ncol(projects) #96354 proyectos, �son �nicos?

table(table(projects$ID)) #Son �nicos!

str(projects)  #Las fechas se encuentran en formatos Date y POSIXct


#a) 

####################### Definiremos la censura:


#Queremos modelar el riesgo de NO ser financiado, por lo tanto
#el evento ser�: No alcanzar la meta de fondos, el resto se considera censurado


table(projects$state) 

library(dplyr)

projectsfil<-projects %>%
                mutate(state=ifelse(state=="failed", 1, 0), 
                       time=difftime(deadline,launched, units="days"),
                       Estado=ifelse(state==1, "No recauda total", "Recauda total"))

View(projectsfil)

min(projectsfil$time)
max(projectsfil$time)

table(projectsfil$state) #58148 proyectos no recaudaron totalmente el monto

table(projectsfil$state) #38206 proyectos recaudaron totalmente el monto


table(projectsfil$category)

library(ggplot2)

ggplot(projectsfil, aes(x=category, fill=Estado)) + 
  theme_bw() +
  geom_bar() +
  labs(y="N�mero de proyectos", 
       x="", 
       title="Recaudaci�n de fondos para projectos por categor�a")+
  scale_fill_manual(values=c("lightcoral","lightgreen"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
       legend.position = "top", title = element_text(hjust=0.5))


ggplot(projectsfil, aes(x=backers)) +
  geom_boxplot()+
  labs(y="", title="Cantidad de patrocinadores y estado de proyecto")+ 
  facet_wrap(~Estado) 

#Naturalmente, a mayor cantidad de donadores se esperar�a que
#alcancen el total


#b) 

str(projectsfil)

projectsfil$category<-factor(projectsfil$category)

table(projectsfil$category)

length(levels(projectsfil$category))

library(survival)

vsurvfit <- survfit(Surv(time, state) ~ 1, data = projectsfil)

plot(vsurvfit,las=1, main="Sobrevivencia de proyectos a ser financiados", xlab="D�as", ylab="Probabilidad de sobrevivencia")

coxfit<-coxph(Surv(time,state)~category+backers,data=projectsfil)

summary(coxfit)

#Notar que exp(backers)<1 ser�a "factor protector" de no recaudar el dinero

contrasts(projectsfil$category) #Art queda como celda de referencia

#Categor�as que tienen mayor riesgo de no recaudar los fondos
#en comparaci�n a la categor�a Arte son:

#- Crafts
# - Design
# - Fashion
# - Games
# - Technology

#y otros

cox.zph(coxfit)

#Se rechaza el supuesto de riesgos proporcionales en todos los
#casos. No ser�a adecuado utilizar un modelo en base a este supuesto.


