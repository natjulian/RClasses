#Fuente base de datos: https://www.cbioportal.org/study/summary?id=brca_metabric

setwd("C:/Users/HP/Desktop/Trabajo/2020-2/Análisis de Sobrevivencia/Ayudantía 3")

library(readr)
brcametabric <- read_delim("brcametabric.tsv", 
                           "\t", escape_double = FALSE, trim_ws = TRUE)
View(brcametabric)

head(brcametabric, 4)

#Overall survival (months) corresponde a y_i
#Patients Vital Status indica estado paciente

#a)

unique(brcametabric$`Patient's Vital Status`)

summary(brcametabric$`Overall Survival (Months)`)

unique(brcametabric$`3-Gene classifier subtype`)

brcametabric<-brcametabric[-which(is.na(brcametabric$`Patient's Vital Status`)|is.na(brcametabric$`Overall Survival (Months)`)|is.na(brcametabric$`3-Gene classifier subtype`)|is.na(brcametabric$`Tumor Size`)), ]

unique(brcametabric$`Patient's Vital Status`)

summary(brcametabric$`Overall Survival (Months)`)

table(brcametabric$`Patient's Vital Status`)

431+753 #Casos censurados

558 #Fallos

prop.table(table(brcametabric$`Patient's Vital Status`))

#Fallecieron el 32% de los casos (sin considerar NA's)

library(dplyr)

brcametabric<-brcametabric%>%
  mutate(censored=ifelse(`Patient's Vital Status`=="Died of Disease",0,1))

View(brcametabric)


#b)
  
table(brcametabric$censored, brcametabric$`3-Gene classifier subtype`)

#Por subtipo como se distribuye la censura:
prop.table(table(brcametabric$censored, brcametabric$`3-Gene classifier subtype`), margin = 2)

#Por censura como se distribuye el subtipo:
prop.table(table(brcametabric$censored, brcametabric$`3-Gene classifier subtype`), margin = 1)


#c) 

library(survival)

df<-data.frame(brcametabric$`Overall Survival (Months)`, brcametabric$censored, brcametabric$`3-Gene classifier subtype`)
names(df)<-c("t", "delta", "subtype")

df$subtype<-factor(df$subtype)

weibull<-survreg(Surv(t, delta)~subtype, data=df, dist="weibull")

summary(df$t)

#Recordar que un supuesto es que S(0)=1. Quitar observaciones donde ocurrencia es al tiempo 0

brcametabric<-brcametabric[brcametabric$`Overall Survival (Months)`>0,]
df<-df[df$t>0,]


#install.packages("SurvRegCensCov")
library(SurvRegCensCov)

modelW<-WeibullReg(Surv(t, delta)~subtype, data=df)

modelW

?WeibullReg

modelW$coef

WeibullDiag(Surv(t, delta)~subtype, data=df)

#c)

#install.packages("eha")
library(eha)

df2<-data.frame(df, brcametabric$`Age at Diagnosis`, brcametabric$`Tumor Size`)

colnames(df2)[4:5]<-c("Age", "Size")

weibull2<-weibreg(Surv(t, delta)~Age+Size, data=df2)


par(mfrow=c(2,2))
plot(weibull2, fn="sur", new.data=c(20, 15))
plot(weibull2, fn="sur", new.data=c(40, 15))
plot(weibull2, fn="sur", new.data=c(60, 15))
plot(weibull2, fn="sur", new.data=c(80, 15))


par(mfrow=c(2,2))
plot(weibull2, fn="sur", new.data=c(40, 5))
plot(weibull2, fn="sur", new.data=c(40, 7))
plot(weibull2, fn="sur", new.data=c(40, 10))
plot(weibull2, fn="sur", new.data=c(40, 20))

summary(weibull2)
