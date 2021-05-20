#### ANOVA UNIFACTORIAL EFECTOS FIJOS

##Se carga la base de datos

#install.packages("readr")

library(readr)
drugs <- read_delim(file.choose(), 
                    ";", escape_double = FALSE, trim_ws = TRUE)


print(drugs)


table(drugs$Drug)

#Alprazolam:

drugs$`Memory score`[which(drugs$Drug=="A")]


#Triazolam:

drugs$`Memory score`[which(drugs$Drug=="T")]


#Ninguno

drugs$`Memory score`[which(drugs$Drug=="N")]


###### Intuiciones

## Medias por tratamiento:

#install.packages("dplyr")
library(dplyr)

drugs%>%group_by(Drug)%>% summarise(Media=mean(`Memory score`))


mean(drugs$`Memory score`)


drugs$Drug<-factor(drugs$Drug, levels=c("A","T","N"))
df<-data.frame(drugs)

#install.packages("ggplot2")
library(ggplot2)

p<-ggplot(df, aes(y=drugs$`Memory score`,x=Drug, fill=Drug)) + 
  geom_violin() + 
  theme_minimal()+
  geom_hline(yintercept=mean(drugs$`Memory score`),col="black")+
  ggtitle("Distribución de tiempos de demora en test de memoria")+
  ylab("Tiempos de demora en test de memoria")+
  xlab("")+
  scale_fill_manual(name = "Tratamiento", labels = c("Alprazolam", "Triazolam", "Ninguno"), values=c("darkslateblue","gray67", "lightblue3"))+
  scale_y_continuous(breaks = round(seq(min(drugs$`Memory score`), max(drugs$`Memory score`), by = 7),0))+
  theme(axis.text.x = element_blank(), plot.title=element_text(hjust=0.5, size=18),
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent")
  )

ggsave(p, filename = "myplot.png",  bg = "transparent")  #Guarda en formato png el gráfico


p2<-ggplot(data = df, aes(y = drugs$`Memory score`, x = seq_along(drugs$`Memory score`), color=drugs$Drug)) +
  geom_point() + 
  theme_minimal()+
  ggtitle("Distribución de tiempos de demora en test de memoria")+
  ylab("Tiempos de demora en test de memoria")+
  xlab("")+
  scale_y_continuous(breaks = round(seq(min(drugs$`Memory score`), max(drugs$`Memory score`), by = 7),0))+
  scale_color_manual(name = "Tratamiento", labels = c("Alprazolam", "Ninguno", "Triazolam"), values=c("darkslateblue","gray67", "lightblue3"))+
  theme(axis.text.x = element_blank(), plot.title=element_text(hjust=0.5, size=18),
    panel.background = element_rect(fill = "transparent"), 
    plot.background = element_rect(fill = "transparent", color = NA),
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(), 
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"))

ggsave(p2, filename = "myplot2.png",  bg = "transparent")  #Guarda en formato png el gráfico




colors<-ifelse(drugs$Drug=="A","pink2", ifelse(drugs$Drug=="T","seagreen2", "skyblue3"))

##Modelo

Time<-drugs$`Memory score`
Drug<-factor(drugs$Drug)   #Se debe definir como variable de tipo factor

contrasts(Drug)<-contr.sum #contraste suma

model<-aov(Time~Drug)

model.matrix(model)

coef(model)

levels(Drug)

#alpha1 se asocia al primer nivel del factor Droga (Drug1), es decir Alprazolam 
#alpha2 se asocia al segundo (Drug2), es decir, Triazolam

#Bajo el contraste suma, alpha3=-(alpha1+alpha2)

#alpha3 (Drug3: No droga)

-sum(coef(model)[2:3])


6.815822-4.263191-2.552631   #suma da cero

mean(drugs$`Memory score`)

anova(model)


#Test F modelos anidados

contrasts(Drug)<-contr.sum #contraste suma
modelo1<-aov(Time~1)
modelo2<-aov(Time~Drug)

#Manualmente
SCEm1<-anova(modelo1)[1,2]  #Suma cuadrática error modelo 1
glEm1<-anova(modelo1)[1,1]  #Grados de libertad asociados al error

SCEm2<-anova(modelo2)[2,2]  #Suma cuadrática error modelo 2
glEm2<-anova(modelo2)[2,1]  #Grados de libertad asociados al error

#Plantear el Estadistico F:

(F=((SCEm1-SCEm2)/(glEm1-glEm2))/(SCEm2/glEm2))

#Regla de decision

F>qf(0.95,glEm1-glEm2,glEm2)  #El estadístico es mayor al cuantil

#Por ende, se rechaza la hipótesis nula. Existe evidencia para
# rechazar que el modelo restringido sólo con intercepto fuera
# el correcto en términos de bondad de ajuste.

#Opción rápida: 

anova(modelo1,modelo2)  #Test F de modelos anidados

TukeyHSD(modelo2)

confint.lm(model)

### ANALISIS MULTIFACTORIAL EFECTOS FIJOS


library(readr)
school <- read_delim(file.choose(), 
                     ";", escape_double = FALSE, trim_ws = TRUE)


print(school)

mean(school$raisedhands)

library(dplyr)

school %>% group_by(gender) %>% summarise(promedio=mean(raisedhands), n=n())

school %>% group_by(parent) %>% summarise(promedio=mean(raisedhands), n=n())

school %>% group_by(satisfaction) %>% summarise(promedio=mean(raisedhands), n=n())


library(ggplot2)

df<-data.frame(school)


#Sexo

p3<-ggplot(df, aes(y=school$raisedhands,x=school$gender, fill=school$gender)) + 
  geom_violin() + 
  theme_minimal()+
  geom_hline(yintercept=mean(school$raisedhands),col="black")+
  ggtitle("Distribución de dudas en aula por sexo")+
  ylab("Dudas en aula")+
  xlab("")+
  scale_fill_manual(name = "Sexo estudiante", labels = c("Mujer", "Hombre"), values=c("darkslateblue","gray67"))+
  scale_y_continuous(breaks = round(seq(min(school$raisedhands), max(school$raisedhands), by = 7),0))+
  theme(axis.text.x = element_blank(), plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent")
  )

ggsave(p3, filename = "myplot3.png",  bg = "transparent")  


#Apoderado

p4<-ggplot(df, aes(y=school$raisedhands,x=school$parent, fill=school$parent)) + 
  geom_violin() + 
  theme_minimal()+
  geom_hline(yintercept=mean(school$raisedhands),col="black")+
  ggtitle("Distribución de dudas en aula por apoderado")+
  ylab("Dudas en aula")+
  xlab("")+
  scale_fill_manual(name = "Apoderado", labels = c("Padre", "Madre"), values=c("darkslateblue","gray67"))+
  scale_y_continuous(breaks = round(seq(min(school$raisedhands), max(school$raisedhands), by = 7),0))+
  theme(axis.text.x = element_blank(), plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent")
  )

ggsave(p4, filename = "myplot4.png",  bg = "transparent")  

#Satisfaccion de apoderado

p5<-ggplot(df, aes(y=school$raisedhands,x=school$satisfaction, fill=school$satisfaction)) + 
  geom_violin() + 
  theme_minimal()+
  geom_hline(yintercept=mean(school$raisedhands),col="black")+
  ggtitle("Distribución de dudas en aula por satisfacción de apoderado")+
  ylab("Dudas en aula")+
  xlab("")+
  scale_fill_manual(name = "Satisfacción con la escuela", labels = c("Mala", "Buena"), values=c("darkslateblue","gray67"))+
  scale_y_continuous(breaks = round(seq(min(school$raisedhands), max(school$raisedhands), by = 7),0))+
  theme(axis.text.x = element_blank(), plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent")
  )

ggsave(p5, filename = "myplot5.png",  bg = "transparent")  

#Interacción Sexo y apoderado

p6<-df %>% 
  ggplot() +
  aes(x = gender, color = parent, group = parent, y = raisedhands) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")+
  theme_minimal()+
  xlab("")+
  ggtitle("Interacción Apoderado y Sexo del estudiante")+
  ylab("Promedio cantidad de preguntas en clase")+
  scale_color_manual(name = "Apoderado", labels = c("Padre","Madre"), values=c("darkslateblue","lightblue3"))+
  scale_y_continuous(breaks = round(seq(min(school$raisedhands), max(school$raisedhands), by = 7),0))+
  scale_x_discrete(labels=c("F" = "Mujer", "M" = "Hombre"))+
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))


ggsave(p6, filename = "myplot6.png",  bg = "transparent")  

#Sexo y Satisfacción


p7<-df %>% 
  ggplot() +
  aes(x = gender, color = satisfaction, group = satisfaction, y = raisedhands) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")+
  theme_minimal()+
  xlab("")+
  ggtitle("Interacción Satisfacción del apoderado y Sexo del estudiante")+
  ylab("Promedio cantidad de preguntas en clase")+
  scale_color_manual(name = "Satisfacción", labels = c("Mala", "Buena"), values=c("darkslateblue","lightblue3"))+
  scale_y_continuous(breaks = round(seq(min(school$raisedhands), max(school$raisedhands), by = 7),0))+
  scale_x_discrete(labels=c("F" = "Mujer", "M" = "Hombre"))+
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))


ggsave(p7, filename = "myplot7.png",  bg = "transparent") 

#Apoderado y Satisfacción


p8<-df %>% 
  ggplot() +
  aes(x = parent, color = satisfaction, group = satisfaction, y = raisedhands) +
  stat_summary(fun = mean, geom = "point") +
  stat_summary(fun = mean, geom = "line")+
  theme_minimal()+
  xlab("")+
  ggtitle("Interacción Satisfacción del apoderado y Apoderado")+
  ylab("Promedio cantidad de preguntas en clase")+
  scale_color_manual(name = "Satisfacción", labels = c("Mala", "Buena"), values=c("darkslateblue","lightblue3"))+
  scale_y_continuous(breaks = round(seq(min(school$raisedhands), max(school$raisedhands), by = 7),0))+
  scale_x_discrete(labels=c("Mum" = "Madre", "Father" = "Padre"))+
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))


ggsave(p8, filename = "myplot8.png",  bg = "transparent") 



#Modelo

#Las variables de tipo factor se definen como factores:

sexo<-factor(school$gender)
apoderado<-factor(school$parent)
satisfaccion<-factor(school$satisfaction)

participacion<-school$raisedhands

contrasts(sexo)<-contr.sum     #contraste suma para cada variable
contrasts(apoderado)<-contr.sum
contrasts(satisfaccion)<-contr.sum


#Modelo con interacciones dobles (pudieran incluirse triples)
model<-aov(participacion~(sexo+apoderado+satisfaccion)^2)

anova(model)


#aditivo

aditivo<-aov(participacion~sexo+apoderado+satisfaccion)

anova(aditivo)   #Suma cuadrática secuencial 
#SS(A), SS(B|A), SS(C|A,B) 


anova(aov(participacion~apoderado+satisfaccion+sexo)) #El orden de ingreso importa

#install.packages("car")
library(car)

Anova(aditivo, type="II") #Suma cuadrática jerárquica 
#SS(A|B,C), SS(B|A, C) y SS(C|A,B)


# Cuando hay interaccion se usa tabla type=III

Anova(aditivo, type="III")  #Si no existe interacción equivale al tipo II

#Si existe interacción entrega:
#SS(A|B, AB) y SS(B|A,AB)


#Coeficientes

coef(aditivo)

levels(sexo)

levels(apoderado)

levels(satisfaccion)


#Pruebas posthoc

TukeyHSD(aditivo)


anova(aditivo)[,1:3]

## ANOVA ALEATORIO

library(readr)
suicide <- read_delim(file.choose(), 
                      ";", escape_double = FALSE, locale = locale(encoding = "ISO-8859-1"), 
                      trim_ws = TRUE)
View(suicide)


print(suicide)

Continente<-factor(suicide$Continente)
Etario<-factor(suicide$Etario)

Suicidios<-suicide$Suicidios

levels(Continente)
levels(Etario)

table(Continente, Etario)  #4 observaciones por combinación de factores

modelo<-aov(Suicidios~Continente*Etario)

anova(modelo)  #Test F erroneo

a<-length(levels(Continente))

b<-length(levels(Etario))

n<-unique(table(Continente, Etario))

MCA<-anova(modelo)[1,3]
MCB<-anova(modelo)[2,3]
MCAB<-anova(modelo)[3,3]
MCE<-anova(modelo)[4,3]

FA<-MCA/MCAB
FB<-MCB/MCAB
FAB<-MCAB/MCE

alpha=0.05

FA>qf(p=(1-alpha) , df1=(a-1) , df2=(a-1)-(b-1))

FB>qf (p=(1-alpha) , df1=(b-1) , df2=(a-1)-(b-1))

FAB>qf (p=(1-alpha) , df1=(a-1)-(b-1) , df2=a-b-(n-1))


#Interacción no da significativa

aditivo<-aov(Suicidios~Continente+Etario)

anova(aditivo)

library(car)

Anova(aditivo, type="II") #Si se posee un caso balanceado tabla anova coinciden

TukeyHSD(aditivo)


### Planteando modelos aleatorios
library(lme4)
m1<-lmer(Suicidios~(1|Continente)+(1|Etario))

summary(m1)



### ANOVA MIXTO

#install.packages("datarium")
library(datarium)
data("performance", package = "datarium")  #Carga la base de datos performance

performance #información agrupada por id

#install.packages("tidyverse")
library(tidyverse)
#install.packages("ggpubr")
library(ggpubr)
#install.packages("rstatix")
library(rstatix)

performance <- performance %>%   
  gather(key = "time", value = "score", t1, t2) %>%
  convert_as_factor(id)

performance  #información a lo largo, extendida

performance %>%
  group_by(gender, stress, time) %>%
  get_summary_stats(score, type = "mean_sd")


p9<-ggboxplot(performance, x = "gender", y = "score",
  color = "stress", palette=c("green4","mediumpurple2","firebrick3"), 
  add = "jitter",
  facet.by ="time")+ggtitle("Scores by gender and stress in two time measures")+
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))


ggsave(p9, filename = "myplot9.png",  bg = "transparent") 


performance%>% 
  group_by(gender,stress,time)%>%
  summarise(n=n())  #Caso balanceado


#Modelo

performance$gender<-factor(performance$gender)
performance$stress<-factor(performance$stress)
performance$time<-factor(performance$time)

levels(performance$gender)
levels(performance$stress)
levels(performance$time)

contrasts(performance$gender)<-contr.sum
contrasts(performance$stress)<-contr.sum


Type1<- anova_test(data = performance, #base de datos
                   dv = score,  #variable dependiente
                   between = c(gender, stress), #variables efecto fijo
                   wid = id, #id del individuo
                   within = time, type=1) #variable efecto aleatorio

get_anova_table(Type1) 


#por realización:

performance %>%
  group_by(time) %>%
  anova_test(dv = score, wid = id, between = c(gender, stress), type=1)


#Test de comparaciones múltiples

performance %>%
  group_by(time, gender) %>%
  pairwise_t_test(score ~ stress, p.adjust.method = "bonferroni") %>%
  select(-p, -p.signif) 

#Supuestos

#Outliers

performance %>%
  group_by(gender, stress, time) %>%
  identify_outliers(score)


#Normalidad

performance %>%
  group_by(gender, stress, time ) %>%
  shapiro_test(score)

p10<-ggqqplot(performance, "score", ggtheme = theme_classic()) +
  facet_grid(time ~ stress, labeller = "label_both") +
  ggtitle("Shapiro test of normality in mix anova")+
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))


ggsave(p10, filename = "myplot10.png",  bg = "transparent") 


#Homogeneidad de varianzas 

performance %>%
  group_by(time) %>%
  levene_test(score~gender*stress)  #dentro de cada combinación de factores fijos


###Variabilidades y coeficientes

#install.packages("lme4")
library(lme4)

#Modelo completo

completo<-lmer(score~gender*stress*(1|time), data=performance)

fixef(completo)   #Estimaciones de componentes fijas

fitted(completo)  #Valores ajustados

residuals(completo)  #residuos

summary(residuals(completo))  #estadísticas de los errores

#Modelo aditivo mixto

lmer(score~gender+stress+(1|time), data=performance)

#Modelo efectos fijos

fijos<-aov(score~gender*stress, data=performance)
anova(fijos)

coef(fijos)

summary(residuals(fijos))


##Modelo ANCOVA


##Modelo ANCOVA


library(readr)
breast <- read_csv("C:/Users/HP/Downloads/breast.csv")
View(breast)

dim(breast)  #569 registros y 33 columnas

names(breast) #primera columna es id

table(table(breast$id))  #no existen registros con mismo id

table(breast$diagnosis)  #357 tumores benignos, 212 tumores malignos


cor(breast[,3:32])[order(abs(cor(breast[,3:32])[,2]), decreasing=TRUE),2]  



plot(breast$texture_mean, breast$radius_mean, col=ifelse(breast$diagnosis=="M", "red","blue"))

p11<-ggplot(data = breast, aes(y = texture_mean, x = radius_mean, color=diagnosis)) +
  geom_point() + 
  theme_minimal()+
  ggtitle("Textura respecto al radio medio en células mamarias")+
  ylab("Textura media en escala de grises")+
  xlab("Radio promedio de células")+
  scale_y_continuous(breaks = round(seq(min(breast$texture_mean), max(breast$texture_mean), by = 3),0))+
  scale_x_continuous(breaks = round(seq(min(breast$radius_mean), max(breast$radius_mean), by = 2),0))+
  scale_color_manual(name = "Tumor", labels = c("Benigno", "Maligno"), values=c("darkslateblue", "lightblue3"))+
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))

ggsave(p11, filename = "myplot11.png",  bg = "transparent")  #Guarda en formato png el gráfico



p12<-ggplot(data = breast, aes(y = texture_mean, x = radius_mean, color=diagnosis, shape=0.054)) +
  geom_point() + geom_smooth(method = "lm", fill = NA)+
  theme_minimal()+
  ggtitle("Textura respecto al radio medio en células mamarias")+
  ylab("Textura media en escala de grises")+
  xlab("Radio promedio de células")+
  scale_y_continuous(breaks = round(seq(min(breast$texture_mean), max(breast$texture_mean), by = 3),0))+
  scale_x_continuous(breaks = round(seq(min(breast$radius_mean), max(breast$radius_mean), by = 2),0))+
  scale_color_manual(name = "Tumor", labels = c("Benigno", "Maligno"), values=c("darkslateblue", "lightblue3"))+
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"), legend.position = "none")

ggsave(p12, filename = "myplot12.png",  bg = "transparent")  #Guarda en formato png el gráfico


breast$diagnosis<-factor(breast$diagnosis)

contrasts(breast$diagnosis)<-contr.sum

radius_meancent<-breast$radius_mean-mean(breast$radius_mean)

model<-lm(texture_mean~diagnosis*radius_meancent, data=breast)

summary(model)  #interaccion no significativa

contrasts(breast$diagnosis)

aditivo<-lm(texture_mean~diagnosis+radius_meancent, data=breast)

summary(aditivo)

coef(aditivo)


#Interpretacion

coef(aditivo)[1]

mean(breast$texture_mean)


coef(aditivo)[2]  #Efecto asociado a tumor benigno

levels(breast$diagnosis)

#Note que:

coef(aditivo)[1]+coef(aditivo)[2]  

#es el intercepto de la recta para el grupo tumor benigno

coef(aditivo)[1]-coef(aditivo)[2]  

#es el intercepto de la recta para el grupo tumor maligno


df = cbind(breast, pred = predict(aditivo))


p13<-ggplot(data = df, mapping=aes(y = texture_mean, x = radius_mean, color=diagnosis)) +
  geom_point() + geom_line(mapping=aes(y=pred))+
  theme_minimal()+
  ggtitle("Textura respecto al radio medio en células mamarias")+
  ylab("Textura media en escala de grises")+
  xlab("Radio promedio de células")+
  scale_y_continuous(breaks = round(seq(min(breast$texture_mean), max(breast$texture_mean), by = 3),0))+
  scale_x_continuous(breaks = round(seq(min(breast$radius_mean), max(breast$radius_mean), by = 2),0))+
  scale_color_manual(name = "Tumor", labels = c("Benigno", "Maligno"), values=c("darkslateblue", "black"))+
  theme(plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))

ggsave(p13, filename = "myplot13.png",  bg = "transparent")  #Guarda en formato png el gráfico


#### ¿Pendientes iguales?
confint(lm(texture_mean~radius_mean,data=subset(breast, diagnosis=="M")))

confint(lm(texture_mean~radius_mean, data=subset(breast, diagnosis=="B")))

summary(aditivo)$adj.r.squared


modeldiag<-lm(texture_mean~diagnosis, data=breast)

anova(modeldiag,aditivo)

summary(aditivo)$coefficients



#Para pensar


textcent<-breast$texture_worst-mean(breast$texture_worst)

model2<-lm(texture_mean~diagnosis*textcent, data=breast)

summary(model2)


