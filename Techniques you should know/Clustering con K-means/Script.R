##### Clustering con K-means

#Cargar la data

library(readxl)
segmentacion <- read_excel(file.choose())


#Vista de la data

names(segmentacion)

print(segmentacion)

#Nos aseguramos de que las variables se lean en el formato adecuado:

str(segmentacion)  

#Sólo la variable Código se lee como caracter, las demás variables
# son de tipo numeric


dim(segmentacion)    #Carta de 177 vinos, 1 ID y 13 variables


table(table(segmentacion$Codigo))  #No hay repetición de ID


#Análisis de las variables

library(ggplot2)
library(dplyr) #Para utilizar %>%
library(tidyverse)  #Para utilizar gather()


##Histogramas

segmentacion %>%
  gather(Attributes, value, 2:14) %>%
  ggplot(aes(x=value, fill=Attributes)) +
  geom_histogram(colour="black", show.legend=FALSE,bins=30) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Valores", y="Frecuencias",
       title="Histogramas de las características de una carta de vinos") +
  theme_classic()

# Es posible observar que las variables se encuentran en escalas
# bastante diferentes, además se distribuyen de distinta forma
# Magnesiumy prolina presentan escalas muy altas respecto a las demás.

#Variabilidad de las variables:

diag(var(segmentacion[,-1]))

#Existen diferencias importantes en las variabilidades. Además, las variables estan
#en distintas escalas. Es necesario estandarizar

stand<-scale(segmentacion[,-1])

diag(var(stand))   #Variabilidades unitarias

#Boxplots

stand<-data.frame(stand)

stand %>%
  gather(Attributes, values) %>%
  ggplot(aes(x=reorder(Attributes, values, FUN=median), y=values, fill=Attributes)) +
  geom_boxplot(show.legend=FALSE) +
  labs(title="Boxplots de las características de una carta de vinos reescaladas") +
  theme_classic() +
  theme(axis.title.y=element_blank(),
        axis.title.x=element_blank())  +
  coord_flip()


#Las variables que tienen outliers son 

#Matiz del vino
#Alcalinidad de ceniza
#Ceniza
#Proantocianidinas
#Magnesium
#Intensidad del color
#Acido Malico


#Detectando outliers:

out1<-which(stand$Matiz.del.vino < boxplot(stand$Matiz.del.vino,plot=FALSE)$stats[1] | stand$Matiz.del.vino > boxplot(stand$Matiz.del.vino,plot=FALSE)$stats[5])
out2<-which(stand$Alcalinidad.de.ceniza < boxplot(stand$Alcalinidad.de.ceniza,plot=FALSE)$stats[1] | stand$Alcalinidad.de.ceniza > boxplot(stand$Alcalinidad.de.ceniza,plot=FALSE)$stats[5])
out3<-which(stand$Ceniza < boxplot(stand$Ceniza,plot=FALSE)$stats[1] | stand$Ceniza > boxplot(stand$Ceniza,plot=FALSE)$stats[5])
out4<-which(stand$Proantocianidinas < boxplot(stand$Proantocianidinas,plot=FALSE)$stats[1] | stand$Proantocianidinas > boxplot(stand$Proantocianidinas,plot=FALSE)$stats[5])
out5<-which(stand$Magnesium < boxplot(stand$Magnesium,plot=FALSE)$stats[1] | stand$Magnesium > boxplot(stand$Magnesium,plot=FALSE)$stats[5])
out6<-which(stand$Intensidad.del.color < boxplot(stand$Intensidad.del.color,plot=FALSE)$stats[1] | stand$Intensidad.del.color > boxplot(stand$Intensidad.del.color,plot=FALSE)$stats[5])
out7<-which(stand$Acido.Malico < boxplot(stand$Acido.Malico,plot=FALSE)$stats[1] | stand$Acido.Malico > boxplot(stand$Acido.Malico,plot=FALSE)$stats[5])

(outliers<-unique(c(out1,out2,out3,out4,out5,out6,out7)))  #Lista de outliers

length(outliers)/nrow(segmentacion)    #Tasa de observaciones outliers


#K-means

#Ejemplo, utilizando k=2

set.seed(2020)  #Semilla de aleatoriedad para los centroides iniciales

clusterk2<-kmeans(stand, centers=2)


#Podemos extraer:

names(clusterk2)

#Clusters

clusterk2$cluster

#Centroides finales 

clusterk2$centers


#Distribucion de observaciones en clusters

clusterk2$size    #En el cluster 1 se agruparon 64 vinos
                  #En el cluster 2 se agruparon 113 vinos



# withinss, dentro de cada cluster la suma cuadratica
#importante considerar que si el numero de observaciones
#en un cluster es mayor, withinss aumenta


clusterk2$withinss    #cohesion por cluster


#tot.whithinss  (suma de withinss)

clusterk2$tot.withinss   #La suma cuadratica intra cluster total


#Lo ideal es que sea menor



# betweenss    #separacion

clusterk2$betweenss      #La suma cuadratica entre cluster 


#Lo ideal es que sea mayor



###Eleccion del k optimo


n<-15        #Cantidad de valores k a probar

bss <- rep(NA,n)
wss <- rep(NA,n)

set.seed(2020)

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

   

#3 clusters

set.seed(2020)  

clusterk3<-kmeans(stand, centers=3)

clusterk3$cluster

segmentacion$cluster<-clusterk3$cluster

clusterk3$centers  #Centroides

clusterk3$size  #El tamaño de los grupos es bastante similar



library(GGally)

ggpairs(cbind(segmentacion[-1], Cluster=as.factor(clusterk3$cluster)),
        columns=1:6, aes(colour=Cluster, alpha=0.5),
        lower=list(continuous="points"),
        upper=list(continuous="blank"),
        axisLabels="none", switch="both") +
  theme_classic()
