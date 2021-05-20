#### Ayudantía: Basket Analysis

#Defino el workspace o directorio

setwd("C:/Users/HP/Desktop/Minería de Datos/Basket Analysis")


#Obtiene un vector con el nombre de los archivos csv en el directorio

nombres<- list.files(path = getwd() , recursive = TRUE,
                            pattern = "\\.csv$", 
                            full.names = FALSE)

nombres

library(readr)

Datas<-lapply(nombres,"read_csv")  #Carga todos los archivos csv 

names(Datas)<-substr(nombres,1,nchar(nombres)-4) #Añade nombre a cada data

Datas


#Cruce de las tablas de interés


# Nos interesa saber por boleta(order_id) los productos que
# se distribuyeron (products)


names(Datas)

library(dplyr)

cruce<-left_join(Datas[[3]],Datas[[5]],by="product_id")


print(cruce)

?left_join


#Basket Analysis

length(unique(cruce$order_id)) #131209 pedidos


set.seed(5)

boletas<-sample(cruce$order_id,5)

(compra1<-cruce$product_name[which(cruce$order_id==boletas[1])])
(compra2<-cruce$product_name[which(cruce$order_id==boletas[2])])
(compra3<-cruce$product_name[which(cruce$order_id==boletas[3])])
(compra4<-cruce$product_name[which(cruce$order_id==boletas[4])])
(compra5<-cruce$product_name[which(cruce$order_id==boletas[5])])


#Productos observados:

unique(c(compra1,compra2, compra3,compra4,compra5))


#Si queremos ver cuales son los productos que se repiten:

intersect(compra1, compra2)
intersect(compra1, compra3)
intersect(compra1, compra4)
intersect(compra1, compra5)

intersect(compra2, compra3)
intersect(compra2, compra4)
intersect(compra2, compra5)

intersect(compra3, compra4)
intersect(compra3, compra5)

compras<-c(compra1,compra2,compra3,compra4,compra5)

length(which(compras=="Banana"))/5   #Soporte de Banan

length(which(compras=="Organic Blackberries"))/5


#### Trabajando con la base


library(tidyverse) # Funcion glimpse para ver estructura de la data

glimpse(cruce)

barplot(table(cruce$product_name))


frecuencias<-data.frame(table(cruce$product_name))

fivenum(frecuencias$Freq) #Minimo, 1quantil, Mediana, 3quantile, Maximo


frecuencias<-frecuencias[frecuencias$Freq>5000,]

library(ggplot2)
frecuencias  %>% ggplot(aes(x=Var1, y=Freq)) +
  geom_bar(stat="identity", fill="palegreen2", 
           show.legend=FALSE, colour="black") +
  geom_label(aes(label=Freq)) +
  labs(title="Frecuencia en órdenes de delivery de productos") +
  theme_bw()+xlab("")+ylab("Frecuencia")+
  theme(axis.text.x.bottom = element_text(size=7), plot.title = element_text(hjust=0.5,size=18))


### Probando umbral de confianza y soporte


supportLevels <- c(0.05, 0.1, 0.20, 0.25)/10
confidenceLevels <- c(0.6, 0.5, 0.4, 0.3, 0.2, 0.1)


rules_sup1 <- rep(NA, length(confidenceLevels))
rules_sup2 <- rep(NA, length(confidenceLevels))
rules_sup3 <- rep(NA, length(confidenceLevels))
rules_sup4 <- rep(NA, length(confidenceLevels))

# Reglas

library(arules)


trans<-data.frame(factor(cruce$order_id),cruce$product_name)

colnames(trans)<-c("Transaction","Item")


basket_data = group_by(cruce, order_id)
basket_data = summarise(basket_data,itens=as.vector(list(product_name)))

trans=as(basket_data$itens, 'transactions')


for (i in 1:length(confidenceLevels)) {
  
  rules_sup1[i] <- length(apriori(trans, parameter=list(sup=supportLevels[1], conf=confidenceLevels[i], target="rules")))
  
}


for (i in 1:length(confidenceLevels)){
  
  rules_sup2[i] <- length(apriori(trans, parameter=list(sup=supportLevels[2], conf=confidenceLevels[i], target="rules")))
  
}


for (i in 1:length(confidenceLevels)){
  
  rules_sup3[i] <- length(apriori(trans, parameter=list(sup=supportLevels[3], conf=confidenceLevels[i], target="rules")))
  
}

for (i in 1:length(confidenceLevels)){
  
  rules_sup4[i] <- length(apriori(trans, parameter=list(sup=supportLevels[4], conf=confidenceLevels[i], target="rules")))
  
}

num_rules <- data.frame(rules_sup1, rules_sup2, rules_sup3, rules_sup4, confidenceLevels)


ggplot(data=num_rules, aes(x=confidenceLevels)) +
  
  geom_line(aes(y=rules_sup1, colour=paste("Soporte de", supportLevels[1]*100,"%"))) + 
  geom_point(aes(y=rules_sup1, colour=paste("Soporte de", supportLevels[1]*100,"%"))) +
  
  geom_line(aes(y=rules_sup2, colour=paste("Soporte de", supportLevels[2]*100,"%"))) +
  geom_point(aes(y=rules_sup2, colour=paste("Soporte de", supportLevels[2]*100,"%"))) +
  
  geom_line(aes(y=rules_sup3, colour=paste("Soporte de", supportLevels[3]*100,"%"))) + 
  geom_point(aes(y=rules_sup3, colour=paste("Soporte de", supportLevels[3]*100,"%"))) +
  
  geom_line(aes(y=rules_sup4, colour=paste("Soporte de", supportLevels[4]*100,"%"))) +
  geom_point(aes(y=rules_sup4, colour=paste("Soporte de", supportLevels[4]*100,"%"))) +
  
  
  labs(x="Confianza", y="Número de reglas encontradas", 
       title="Algoritmo apriori con distintos Soportes") +
  theme_minimal() +
  theme(legend.title=element_blank(), plot.title = element_text(hjust=0.5))



rules <- apriori(trans, parameter=list(sup=0.01, conf=0.2, target="rules"))


inspect(sort(rules,by="lift")) #Ordena por ganancia o lift las reglas


#install.packages("arulesViz")

library(arulesViz) # Visualización de reglas de asociación

plot(rules, method="graph",cex=0.6, main="Lift de Reglas de asociación")
