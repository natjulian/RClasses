########### PAUTA CONTROL CLUSTERING



#Carga la data 


library(readr)
Cost_of_living <- read_delim(file.choose(), 
                             ";", escape_double = FALSE, trim_ws = TRUE)


#Vista de la data

names(Cost_of_living)

print(Cost_of_living)

#Nos aseguramos de que las variables se lean en el formato adecuado:

str(Cost_of_living) 

#Sólo la variable City se lee como caracter, las demás variables
# son de tipo numeric


dim(Cost_of_living)  #518 ciudades, ¿son distintas?

table(table(Cost_of_living$City))  #Son todas las ciudades distintas


#Análisis de las variables

library(ggplot2)
library(dplyr) 
library(tidyverse)  


##Histogramas

Cost_of_living %>%
  gather(Attributes, value, 2:7) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE,bins=30) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Valores", y="Frecuencias",
       title="Histogramas de los distintos costos de vida en una ciudad") +
  theme_classic()


#Si bien, todos los índices están en una misma escala de medición
#respecto a Nueva york, es importante notar que hay variables que alcanzan
# valores más altos que otros, por ejemplo, Local Purchasing Power Index, el poder
# adquisitivo dado un sueldo promedio alcanza valores mayores  a 150

#Alcanzan distintos valores las variables, quizás sea necesario estandarizar

#Variabilidad de las variables:

diag(var(Cost_of_living[,-1]))

#Notar que las variabilidades cambian bastante a pesar de que se encuentran
# en una misma escala. La variable Local Purchasing Power Index presenta una
# variabilidad de 1371, versos 270 (variabilidad de Rent Index) por ejemplo

#Es necesario estandarizar

stand<-scale(Cost_of_living[,-1])

stand<-data.frame(stand)

stand %>%
  gather(Attributes, values) %>%
  ggplot(aes(x=reorder(Attributes, values, FUN=median), y=values, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Boxplots de los distintos costos de vida estandarizados") +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())  +
  coord_flip()


#Solo dos variables presentan outliers

#Restaurant Price Index

#Rent Index


#Identificamos aquellos outliers:

outrest<-which(stand$Restaurant.Price.Index < boxplot(stand$Restaurant.Price.Index,plot=FALSE)$stats[1] | stand$Restaurant.Price.Index > boxplot(stand$Restaurant.Price.Index,plot=FALSE)$stats[5])

outrent<-which(stand$Rent.Index < boxplot(stand$Rent.Index,plot=FALSE)$stats[1] | stand$Rent.Index > boxplot(stand$Rent.Index,plot=FALSE)$stats[5])


outliers<-unique(c(outrest,outrent))

(length(outliers)/nrow(Cost_of_living))*100

#Tasa porcentual de observaciones outliers es bajísima


#Existen dos opciones:


#a) La primera es continuar con el clustering incluyendo
#outliers, esto especificando que la tasa es bastante baja
#o que los outliers no se alejan demasiado de la masa de los datos
# no son outliers abismantes.

#b) Quitar los outliers, bajo algún criterio que especifique, o
#proponer metodología.


#K-means

###Eleccion del k optimo


n<-13        #Cantidad de valores k a probar

#Pueden usar distintas grillas, pero a partir de 12 aproximadamente
# no se observan mayores diferencias

bss <- rep(NA,n)
wss <- rep(NA,n)

set.seed(2019)

for(i in 1:n){
  bss[i] <- kmeans(stand, centers=i)$betweenss
  wss[i] <- kmeans(stand, centers=i)$tot.withinss
  
}


#Graficas

betweenplot <- qplot(1:n, bss, geom=c("point", "line"), 
                     xlab="K", ylab="Suma cuadrática entre clusters") +
  scale_x_continuous(breaks=seq(0, n, 1)) +
  theme_classic()+theme(axis.text.y = element_text(size=14),axis.title.y = element_text(size=16),
                        axis.text.x = element_text(size=14),axis.title.x = element_text(size=16))


whithinplot <- qplot(1:n, wss, geom=c("point", "line"),
                     xlab="K", ylab="Suma cuadrática Total intra clusters") +
  scale_x_continuous(breaks=seq(0, n, 1)) +
  theme_classic()+theme(axis.text.y = element_text(size=14),axis.title.y = element_text(size=16),
                        axis.text.x = element_text(size=14),axis.title.x = element_text(size=16))


library(ggpubr)
plot<-ggarrange(betweenplot, whithinplot, ncol=2)

annotate_figure(plot,top=text_grob("Elección del K óptimo",size=22))


#El valor más sugerente pudiera ser k=2, pero en k=4 también
# se observa una diferencia que pudiera ser importante

#La elección del k dependerá de la justificación que realicen 


# K = 4

set.seed(2019)  

cluster<-kmeans(stand, centers=4)

cluster$cluster

cluster$centers  #Centroides

cluster$size  #No hay ningun cluster demasiado pequeño en 
#comparacion a otro



library(GGally)

ggpairs(cbind(Cost_of_living[-1], Cluster=as.factor(cluster$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_classic()

#Es interesante observar cómo se diferencias las variables 
#por clustering


#Se agrega la clusterizacion encontrada:

Cost_of_living$cluster<-cluster$cluster


### Santiago, Chile

which(Cost_of_living$City=="Santiago, Chile")

#Observación 310 corresponde a Chile

Cost_of_living$cluster[310]  #Pertenece al cluster 2



Cost_of_living$City[which(Cost_of_living$cluster=="2")]

#Otras ciudades que pertenecen al mismo cluster son:

#Rio de Janeiro, Brazil
#Moscow, Russia
#Shanghai, China