#### DATOS CASOS DIARIOS

library(tidyverse)
library(rvest)

enlace <- read_html("https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto3/CasosTotalesCumulativo_T.csv")
data   <- html_table(enlace, header = T) %>% 
  data.frame() %>% 
  select(-"Var.1") %>% 
  rename(Fecha = Region)

data[-1,-1] <-  apply(data[,-1], MARGIN = 2, FUN = diff)
data$Fecha <- as.Date(data$Fecha)


data<-subset(data, Fecha>"2021-02-15" & Fecha<"2021-05-30")

data<-data%>%select(Fecha, Magallanes)
colnames(data)<-c("Fecha", "Casos_Magallanes")


## DATOS VACUNACION


enlace2 <- read_html("https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto76/vacunacion.csv")
data   <- html_table(enlace2, header = T)  %>% 
  data.frame() %>% 
  select(-"Var.1")%>%
  filter(Region=="Magallanes")%>%
  gather("Fecha", "Vacunados", -Region, -Dosis)%>%
  mutate(Fecha=gsub("X", "", Fecha))

substr(data$Fecha, 5, 5)  <- "-"

substr(data$Fecha, 8, 8)  <- "-"

data$Fecha <- as.Date(data$Fecha)


data<-reshape(data, dir="w", idvar=c("Fecha", "Region"), timevar="Dosis")

data[-1,-c(1,2)] <-  apply(data[,-c(1:2)], MARGIN = 2, FUN = diff)


data<-subset(data, Fecha>"2021-02-15" & Fecha<"2021-05-30")

colnames(data)<-c("Region", "Fecha", "Primera_Dosis", "Segunda_Dosis", "Unica_Dosis")

head(data)

write.csv(data, "Vacunacion_Magallanes.csv", row.names = FALSE)

#DATOS PREDICT

enlace2 <- read_html("https://github.com/MinCiencia/Datos-COVID19/blob/master/output/producto76/vacunacion.csv")
data   <- html_table(enlace2, header = T)  %>% 
  data.frame() %>% 
  select(-"Var.1")%>%
  filter(Region=="Magallanes")%>%
  gather("Fecha", "Vacunados", -Region, -Dosis)%>%
  mutate(Fecha=gsub("X", "", Fecha))

substr(data$Fecha, 5, 5)  <- "-"

substr(data$Fecha, 8, 8)  <- "-"

data$Fecha <- as.Date(data$Fecha)


data<-reshape(data, dir="w", idvar=c("Fecha", "Region"), timevar="Dosis")

data[-1,-c(1,2)] <-  apply(data[,-c(1:2)], MARGIN = 2, FUN = diff)


data<-subset(data, Fecha>="2021-05-30")

colnames(data)<-c("Region", "Fecha", "Primera_Dosis", "Segunda_Dosis", "Unica_Dosis")

head(data)

write.csv(data, "Vacunacion_Magallanes_predict.csv", row.names = FALSE)

