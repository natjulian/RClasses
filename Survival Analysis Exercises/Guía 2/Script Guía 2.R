library(readr)
Meteoritos <- read_csv("C:/Users/HP/Desktop/Trabajo/2020-2/Análisis de Sobrevivencia/Ayudantía 2/Meteoritos.csv")
View(Meteoritos)

head(Meteoritos,6)

head(Meteoritos$year, 6)

nrow(Meteoritos)

table(table(Meteoritos$id))

#Extrae el año:

year<-as.numeric(substr(Meteoritos$year, start=7, stop=10))

Meteoritos$year2<-year

min(year)

max(year)

Meteoritos<-Meteoritos[-which(year==2101),]

## Fechas aleatorias: 

set.seed(2020)

days<-sample(1:28, size=nrow(Meteoritos), replace=TRUE)
month<-sample(1:12, size=nrow(Meteoritos), replace=TRUE)

start_date <- as.Date(paste(Meteoritos$year2, month, days, sep="-"))  

head(start_date, 7)

## Hora aleatoria

Meteoritos$Registro<-as.POSIXct(start_date, tz="GMT") + runif(n=nrow(Meteoritos), min=0, max=100000)
Meteoritos$Final<-as.POSIXct(Meteoritos$Registro+runif(n=nrow(Meteoritos), min=1000, max=7200))

?as.POSIXct

head(Meteoritos[, c('name', 'fall', 'Registro', 'Final')])

#Calculo de los tiempos t (en minutos)

t<-Meteoritos$Final-Meteoritos$Registro 

head(t)

#Otra manera:

library(dplyr)

df<-Meteoritos %>% 
  mutate(
    time = 
      as.numeric(round(
        difftime(Final, 
                 Registro, 
                 units = "mins"),1)), status=ifelse(fall=="Found",1, 0)
    ) %>% select(c('name', 'mass (g)', 'recclass', 'fall', 'GeoLocation', 'time', 'status'))


?difftime

head(df)
View(df)

library(survival)

head(with(df, Surv(time, status)), 50)
tail(with(df, Surv(time, status)), 50)

my.fit <- survfit(Surv(time, status) ~ 1, data = df) #Objeto surv

my.fit

summary(my.fit)$surv # Entrega estimaciones de Kaplan meier al tiempo tk
summary(my.fit)$time # Entrega los cortes de tiempo tk (minutos)
summary(my.fit)$n.risk # Entrega numero de meteoritos en riesgo al tiempo tk
summary(my.fit)$n.event # Fallos {d_k}
summary(my.fit)$std.err # Error estandar de la estimacion de K-M en t_k
summary(my.fit)$lower # Cota inferior de la estimacion
summary(my.fit)$upper # Cota superior de la estimacion
str(my.fit) # Informacion extraible del objeto my.fit


summary(survfit(Surv(time, status) ~ 1, data = df)) #Toda la información

summary(my.fit, times = 68.7) #Al minuto 68.7 el 50% de los meteroitos impactó la tierra


library(survminer)

ggsurvplot(
  my.fit,
  data = df,
  size = 1,  
  risk.table.height = 0.28, 
  ggtheme = theme_bw()      
)


filter<-df%>%
  filter(recclass=="Diogenite"|recclass=="L5")

my.fit2 <- survfit(Surv(time, status) ~ recclass, data = filter)


ggsurvplot(
  my.fit2,
  data = filter,
  size = 1,                 
  palette =
    c("darkkhaki", "indianred4"),
  conf.int = TRUE,          
  legend.labs =
    c("Diogenite", "L5"), 
  ggtheme = theme_bw()      
)

survdiff(Surv(time, status)~recclass, data=filter)

