#############
###LIBRERÍAS

library(tidyverse)
library(dplyr)
library(rvest)
library(forecast)
library(ggplot2)
library(tseries)
library(lmtest)

##### - CASOS DIARIOS COVID EN REGIÓN DE MAGALLANES DESDE MEDIADOS DE FEBRERO

enlace <- read_html("https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto3/CasosTotalesCumulativo_T.csv")
data1   <- html_table(enlace, header = T) %>% 
  data.frame() %>% 
  select(-"Var.1") %>% 
  rename(Fecha = Region)

data1[-1,-1] <-  apply(data1[,-1], MARGIN = 2, FUN = diff)
data1$Fecha <- as.Date(data1$Fecha)

data1<-subset(data1, Fecha>"2021-02-15" & Fecha<"2021-05-30")

data1<-data1%>%select(Fecha, Magallanes)
colnames(data1)<-c("Fecha", "Casos_Magallanes")

head(data1)


##### - CASOS DIARIOS DE VACUNACIÓN

enlace2 <- read_html("https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto76/vacunacion.csv")
data2   <- html_table(enlace2, header = T)  %>% 
  data.frame() %>% 
  select(-"Var.1")%>%
  filter(Region=="Magallanes")%>%
  gather("Fecha", "Vacunados", -Region, -Dosis)%>%
  mutate(Fecha=gsub("X", "", Fecha))

substr(data2$Fecha, 5, 5)  <- "-"

substr(data2$Fecha, 8, 8)  <- "-"

data2$Fecha <- as.Date(data2$Fecha)

data2<-reshape(data2, dir="w", idvar=c("Fecha", "Region"), timevar="Dosis")

data2[-1,-c(1,2)] <-  apply(data2[,-c(1:2)], MARGIN = 2, FUN = diff)

data2<-subset(data2, Fecha>"2021-02-15" & Fecha<"2021-05-30")

colnames(data2)<-c("Region", "Fecha", "Primera_Dosis", "Segunda_Dosis", "Unica_Dosis")

head(data2)


#### UNIFICACION DE LA INFORMACIÓN

data<-data1%>%
  left_join(., data2, by="Fecha")

head(data)


#### FORMATO DE SERIE DE TIEMPO

casos_ts <- ts(data$Casos_Magallanes, 
               start = min(data$Fecha), 
               frequency = 365)

#Opción 1 (No muestra detalle de fechas)
autoplot(casos_ts) +
  ggplot2::theme_light()

#Opción 2: (Muestra más detalle)
ggplot() +
  aes(x = data$Fecha, y = data$Casos_Magallanes) +
  geom_line(lwd = 0.8) +
  xlab("Fecha")+
  ylab("Casos Diarios")+
  ggtitle("Casos diarios en la Región de Magallanes")+
  theme_light()+
  theme(title = element_text(size=16, hjust=0.5))


#### REVISIÓN DE LA SERIE DE TIEMPO 

autoplot(diff(casos_ts)) #Gráfico de la serie diferenciada

(lambda <- BoxCox.lambda(casos_ts)) #Transformación de BoxCox no necesaria

adf.test(diff(casos_ts)) #Test de Dickey-Fuller

acf(diff(casos_ts), lag.max=60) #Acf
pacf(diff(casos_ts), lag.max=60) #Pacf


####MODELO OBTENIDO CON AUTO.ARIMA

modelo <- auto.arima(casos_ts)

coeftest(modelo)

autoplot(casos_ts) +
  geom_line(aes(y = modelo$fitted), color = "darkcyan") +
  theme_light() 


#### SUPUESTOS DEL MODELO

checkresiduals(modelo) #test de independencia - Ljung-Box 

shapiro.test(modelo$residuals) #test de normalidad - Shapiro



