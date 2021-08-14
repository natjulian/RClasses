### Pauta control 2

#Carga la data

library(readxl)
potencial <- read_excel("potencial.xlsx")

print(potencial)

str(potencial)  #Revisar formato de las variables 


#Las variables Education y Personal Loan deben recodificarse:

potencial$`Personal Loan`<-ifelse(potencial$`Personal Loan`=="0","No adquiere","Adquiere")

potencial$Education<-ifelse(potencial$Education=="1","Undergraduated",ifelse(potencial$Education=="2","Graduated","Advanced"))


#Verificar que no hay NA:
summary(potencial)


#Verificar que no hay observaciones repetidas:

table(table(potencial$ID))   #Efectivamente no se repiten ID


#Análisis de la variable target

(table(potencial$`Personal Loan`)/nrow(potencial))*100

df<-data.frame((table(potencial$`Personal Loan`)/nrow(potencial))*100)
df[,3]<-df[,2]*10

colnames(df)<-c("class","prop","n")

#install.packages("dplyr")

library(dplyr)

df <- df %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

#Gráfico de torta

library(ggplot2)

ggplot(df, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 2, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")),size=5.5, color = "black")+
  scale_fill_manual(values = c("pink2","seagreen2")) +
  theme_void()+labs(fill="Adquiere o no el producto",title = "Distribución éxito de campaña")+
  theme(plot.title = element_text(hjust=0.5,size=22),legend.title = element_text(size=18),legend.text = element_text(size=16))

#el 9.6% adquiere el producto. Hay que determinar características de estos clientes
# el 90.4% no adquiere el producto


#Income

summary(potencial$Income)

#Los valores de Income no son muy altos


#columnas que no se utilizarán:

# - ID 
# - ZIP CODE

#Set de entrenamiento

#install.packages("caret")
library(caret)

set.seed(2021) #Semilla de aleatoriedad para el split


#split de 75% entrenamiento

index <- createDataPartition(potencial$`Personal Loan`, p = 0.75, list = FALSE)
Train <- potencial[index,]
Test <- potencial[-index,]


table(Train$`Personal Loan`)

table(Test$`Personal Loan`)


# Creando el arbol

#install.packages("rpart")
library(rpart)

model <- rpart(`Personal Loan` ~., data=Train[,-c(1,4)], method="class",model=TRUE)

model$variable.importance

#La variable que mas aporta es Education



#install.packages("rpart.plot")
library("rpart.plot")


#El arbol completo:
prp(model,type=1, box.palette = "-RdYlGn",legend.x=NA,cex=0.7)

rpart.plot(model,extra=4,box.palette = "-RdYlGn",cex=0.5)

#Costo de complejidad

model$cptable


#Se obtienen los mismos árboles

#El menor xerror se tiene en el arbol mas grande
# por lo tanto, podar utilizando el cp con minimo xerror no resulta
# efectivo.

# Podría podarse en términos de parsimonia, y fijarse
# otro cp

# Aquí dependerá del criterio de cada uno, lo importante es
# especificar

model_dt.pruned2 <- prune(model, cp = 0.02777778) #arbol podado

prp(model_dt.pruned2,type=1,box.palette = "-RdYlGn",legend.x=NA)


#Desempeño arbol podado

predpod <- unname(predict(model_dt.pruned2, Test[,-c(1,4)], type = "class"))

table(predpod==Test$`Personal Loan`)

addmargins(table(predpod,Test$`Personal Loan`),margin=1)

#Tasa de clasificación correcta:

sum(diag(addmargins(table(predpod,Test$`Personal Loan`),margin=1)))/nrow(Test)

