### Árboles de decisión

library(readr)
credit <- read_csv("credit.csv")


str(credit)  #Revisar formato de las variables 

#variables en formato character:
# - credit_status
# - purpose
# - employment_length
# - personal_status
# - housing

#el resto son variables en formato numerical

summary(credit)

#No existen observaciones faltantes

print(credit)

names(credit)

ncol(credit)   #10 variables

nrow(credit)   #1000 registros


#Análisis de la variable target

(table(credit$credit_status)/nrow(credit))*100

df<-data.frame((table(credit$credit_status)/nrow(credit))*100)
df[,3]<-df[,2]*10

colnames(df)<-c("class","prop","n")

#install.packages("dplyr")

library(dplyr)

df$class<-factor(df$class,levels=c("delayed","critical","repaid","fully repaid"))


df <- df %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

df

#Gráfico de torta

library(ggplot2)

ggplot(df, aes(x = "", y = prop, fill = class)) +
  geom_bar(width = 2, stat = "identity", color = "black") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = paste(prop,"%")),size=5.5, color = "black")+
  scale_fill_manual(values = c("paleturquoise3","lightsalmon","pink2","seagreen2")) +
  theme_void()+labs(fill="Estado de pago",title = "Distribución estado de pago de cuota vigente")+
  theme(plot.title = element_text(hjust=0.5,size=22),legend.title = element_text(size=18),legend.text = element_text(size=16))


# El estado de pago crítico corresponde a un 29.3%, es alto
# El estado de pago atrasado corresponde a un 8.8%
# El estado de pago fully repaid corresponde a un 8.9%
# El estado de pago repaid corresponde a un 53%, la categoría más alta


#Variable personal status

table(credit$personal_status)

#Extrañamente, no aparece married female o single female,
# la mayoría de las categorías son referentes a male

#Creación de variable sexo:

sex<-credit$personal_status

sex<-ifelse(sex=="divorced male"|sex=="married male"|sex=="single male","male","female")

table(sex) 

credit$sex<-sex


#Variables cuantitativas

#months_loan duration

table(credit$months_loan_duration)

df<-data.frame(credit)

ggplot(aes(y = months_loan_duration, x = credit_status,fill=credit_status), data = df) + 
  geom_boxplot()+theme_minimal()+
  scale_fill_manual(values = c("paleturquoise3","lightsalmon","pink2","seagreen2"))+
  theme(axis.text.x = element_blank())+xlab("")+ylab("Cuotas")+
  ggtitle("Boxplots del número de cuotas por Estado de pago")+labs(fill="Estado de pago")+
  theme(plot.title = element_text(hjust=0.5,size=18),legend.title = element_text(size=18),legend.text = element_text(size=16))


#Amount

plt1 <- credit %>% select(amount) %>%
  ggplot(aes(x="", y = amount)) +
  geom_boxplot(fill = "lightblue", color = "black") + 
  coord_flip() +
  theme_classic() + xlab("")+
  ylab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plt2 <- credit %>% select(amount) %>%
  ggplot() +
  geom_histogram(aes(x = amount, y = (..count..)/sum(..count..)),
                 position = "identity", binwidth = 100, 
                 fill = "lightblue", color = "black") +
  ylab("") + xlab("")+ ggtitle("Monto del crédito")+
  theme_classic()+ theme(plot.title = element_text(hjust=0.5,size=28),legend.title = element_text(size=18),legend.text = element_text(size=16))


#install.packages("patchwork")

library(patchwork)
plt2 + plt1 + plot_layout(nrow = 2, heights = c(2, 1))


#logaritmo de amount

credit$logamount<-log(credit$amount)

plt3 <- credit %>% select(logamount) %>%
  ggplot(aes(x="", y = logamount)) +
  geom_boxplot(fill = "seagreen2", color = "black") + 
  coord_flip() +
  theme_classic() + xlab("")+
  ylab("") +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

plt4 <- credit %>% select(logamount) %>%
  ggplot() +
  geom_histogram(aes(x = logamount, y = (..count..)/sum(..count..)),
                 position = "identity", binwidth = 0.02, 
                 fill = "seagreen2", color = "black") +
  ylab("") + xlab("")+ ggtitle("log Monto del crédito")+
  theme_classic()+ theme(plot.title = element_text(hjust=0.5,size=28),legend.title = element_text(size=18),legend.text = element_text(size=16))

plt2 + plt4+plt1 +plt3+plot_layout(nrow = 2, heights = c(2, 2))

# Age

ggplot(aes(y = age, x = credit_status,fill=credit_status), data = df) + 
  geom_violin()+theme_minimal()+
  scale_fill_manual(values = c("paleturquoise3","lightsalmon","pink2","seagreen2"))+
  theme(axis.text.x = element_blank())+xlab("")+ylab("Edad")+
  ggtitle("Gráfico de Violin de Edad por Estado de Pago")+labs(fill="Estado de pago")+
  theme(plot.title = element_text(hjust=0.5,size=18),legend.title = element_text(size=18),legend.text = element_text(size=16))


#Set de entrenamiento

#install.packages("caret")
library(caret)

set.seed(1) #Semilla de aleatoriedad para el split


#split de 60% entrenamiento

index <- createDataPartition(credit$credit_status, p = 0.6, list = FALSE)
Train <- credit[index,]
Test <- credit[-index,]


table(Train$credit_status)

table(Test$credit_status)



# Creando el arbol

#install.packages("rpart")
library(rpart)

model <- rpart(credit_status~., data=Train[,-4], method="class",model=TRUE)

model$variable.importance



#install.packages("rpart.plot")
library("rpart.plot")

prp(model,type=1, box.palette = "RdYlGn",legend.x=NA)

prp(model,type=2, box.palette = "RdYlGn",legend.x=NA)

prp(model,type=3, box.palette = "RdYlGn",legend.x=NA)


# Costo de complejidad

printcp(model)

(bestcp <- model$cptable[which.min(model$cptable[,"xerror"]),"CP"])

model_dt.pruned <- prune(model, cp = bestcp) #arbol podado

prp(model_dt.pruned,type=1,box.palette = "RdYlGn",legend.x=NA)



#Predicción de los modelos

pred <- unname(predict(model, Test[,-4], type = "class"))

table(pred==Test$credit_status)

addmargins(table(pred,Test$credit_status),margin=1)

#Tasa de clasificación correcta:

sum(diag(addmargins(table(pred,Test$credit_status),margin=1)))/nrow(Test)


#Desempeño arbol podado

pred2 <- unname(predict(model_dt.pruned, Test[,-4], type = "class"))

table(pred2==Test$credit_status)

addmargins(table(pred2,Test$credit_status),margin=1)

#Tasa de clasificación correcta:

sum(diag(addmargins(table(pred2,Test$credit_status),margin=1)))/nrow(Test)


#5 nodos

printcp(model)

model5nodos<-prune(model, cp = 0.010601)

model5nodos$variable.importance


prp(model5nodos,type=1,box.palette = "RdYlGn",legend.x=NA)
prp(model5nodos,type=3,box.palette = "RdYlGn",legend.x=NA)


#Desempeño arbol podado 5 nodos

pred3 <- unname(predict(model5nodos, Test[,-4], type = "class"))

table(pred3==Test$credit_status)

addmargins(table(pred3,Test$credit_status),margin=1)

#Tasa de clasificación correcta:

sum(diag(addmargins(table(pred3,Test$credit_status),margin=1)))/nrow(Test)

