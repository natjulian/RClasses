#Carga la base de datos cirrosis

cirrosis<-read.table(file.choose(),
                     h=T,dec=",")

View(cirrosis)

library(tidyverse)

glimpse(cirrosis)


table(cirrosis$Censura)

round(100*table(cirrosis$Censura)/nrow(cirrosis),2)

library(dplyr)

Censored<-cirrosis %>%
          filter(Censura==0)

NotCensored<-cirrosis%>%
             filter(Censura==1)

nrow(Censored); nrow(NotCensored)

df<-data.frame(apply(Censored, MARGIN=2, FUN=mean),apply(NotCensored, MARGIN=2, FUN=mean))

names(df)<-c("Censored", "Not Censored")

round(df,2)

cirrosis$Sexo<-as.factor(cirrosis$Sexo)
cirrosis$Ascitis<-as.factor(cirrosis$Ascitis)
cirrosis$Hepatomegalia<-as.factor(cirrosis$Hepatomegalia)
cirrosis$Aranasvasculares<-as.factor(cirrosis$Aranasvasculares)
cirrosis$Edemas<-as.factor(cirrosis$Edemas)
cirrosis$Etapadelacirrosis<-as.factor(cirrosis$Etapadelacirrosis)
cirrosis$Tratamiento<-as.factor(ifelse(cirrosis$Tratamiento==1, "D-Penicilamina", "Placebo"))

cirrosis$Censura2<-ifelse(cirrosis$Censura==0,"Censurado", "No censurado")

prop.table(table(cirrosis$Sexo, cirrosis$Censura2), 1)

prop.table(table(cirrosis$Ascitis, cirrosis$Censura2), 1)

prop.table(table(cirrosis$Hepatomegalia, cirrosis$Censura2), 1)

prop.table(table(cirrosis$Aranasvasculares, cirrosis$Censura2), 1)

prop.table(table(cirrosis$Edemas, cirrosis$Censura2), 1)

prop.table(table(cirrosis$Etapadelacirrosis, cirrosis$Censura2), 1)


library(survival)

fit <- survfit(Surv(t, Censura) ~ Tratamiento,
               data = cirrosis)

library(survminer)

ggsurvplot(
  fit,
  data = cirrosis,
  size = 1,                 
  palette =
    c("lightsalmon2", "lightslateblue"),
  conf.int = TRUE,          
  legend.labs =
    c("D-Penicilamina", "Placebo"),    
  risk.table.height = 0.28, 
  ggtheme = theme_bw()      
)


ggsurvplot(
  fit, 
  conf.int = TRUE, 
  palette = c("lightsalmon2", "lightslateblue"),
  risk.table = FALSE, risk.table.col = "strata",
  fun = "cumhaz", 
  legend.labs = c("D-Penicilamina", "Placebo")
  )



survfun<-function(t,n,b){
  exp(-(t/n)^{b})
}


#Basta con cambiar los valores:
curve(survfun(x,1,4),main="S(t)")

