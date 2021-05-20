### Cargando la data

library(readr)
breast_cancer <- read_delim("breast-cancer.csv", 
                            ";", escape_double = FALSE, trim_ws = TRUE)
#Análisis inicial de una data en R


View(breast_cancer)    #Muestra la data en una ventana aparte

head(breast_cancer,4)  #Muestra los primeros 4 registros de una data

tail(breast_cancer,4)  #Muestra los últimos 4 registros de una data

dim(breast_cancer)     #Entrega un vector de: número de filas (registros) 
                       #número de columnas (variables) de la data

nrow(breast_cancer)    #Entrega sólo el número de filas (registros) de la data

ncol(breast_cancer)    #Entrega sólo el número de columnas (variables) de la data

names(breast_cancer)   #Entrega el nombre de las variables de la data

colnames(breast_cancer) #Entrega el nombre de las variables de la data

str(breast_cancer)      #Indica formato de cada una de las variables de la data

summary(breast_cancer)  #Entrega un resumen estadístico de cada variable


#Conclusiones inciales de la data: 

#Variable id corresponde a un identificador

table(breast_cancer$id)   #Cada id aparece una y sólo una vez, no precisa análisis

table(table(breast_cancer$id)) #Duda clase pasada

#Variable diagnosis indica el diagnóstico del tumor, es la única variable cualitativa
#de la base de datos

table(breast_cancer$diagnosis)

#Crearemos una matriz de las variables cuantitativas:

numvar<-breast_cancer[ , -c(1,2)]   #Quitamos las dos primeras columnas
View(numvar)


#¿Cómo verificar que efectivamente se quitaron las dos columnas? 
# Comparando las dimensiones

dim(breast_cancer)

dim(numvar)


#Notar que todas las variables numéricas de la data son de tipo cuantitativas continuas


#Cuantificando la relación entre variables cuantitativas

#### Covarianza

cov(numvar)    #Matriz de varianzas-covarianzas

head(cov(numvar))

#### Correlación de Pearson: Medida de relación entre variables cuantitativas continuas

cor(numvar)    #Matriz de correlación de Pearson

head(cor(numvar))


##### Primera sesión llega hasta aquí 





#aplicación entre dos variables

#radius_mean: mean of distances from center to points on the perimeter
#perimeter_mean: mean size of the core tumor

#Tiene sentido que estén relacionadas positivamente, verifiquemoslo:

cov(numvar$radius_mean,numvar$perimeter_mean)








#La covarianza entre las variables es positiva, por lo cual, existe una asociación 
# positiva, si una aumenta, la otra también

#No se puede comentar sobre el grado de asociación mirando la varianza, pero sí mirando 
# la correlación de Pearson:

cor(numvar$radius_mean,numvar$perimeter_mean)

#La correlación es prácticamente 1, por lo cual, se estaría viendo una relación lineal
# prácticamente perfecta entre ambas variables y de tipo positiva


#Gráfico de dispersión básico entre radius_mean y perimeter_mean

plot(numvar$radius_mean,numvar$perimeter_mean,main="Relación entre radius mean y perimeter mean",xlab="Radius mean",ylab="Perimeter mean",las=1)

#Gráfico de dispersión mejorado entre radius_mean y perimeter_mean

#Necesitaremos instalar el paquete ggplot2
install.packages("ggplot2")

#Carga el paquete
library(ggplot2)

graph<-ggplot(numvar, aes(x = radius_mean, y = perimeter_mean))+geom_point()+
  ggtitle("Relación entre radius mean y perimeter mean")+xlab("Radius mean")+
  ylab("Perimeter mean")+labs(subtitle="Diagnosis for breast cancer")+theme_minimal()+theme(
    plot.title = element_text(color = "red", size = 13, face = "bold"))

graph  #Muestra el grafico


#Le agregamos una recta que represente la relación lineal entre ambas variables

graph+geom_smooth(method='lm', formula= y~x,col="red")


#La recta se ajusta casi perfectamente, los puntos que no están sobre la 
# recta son los que provocan que la correlación de pearson no sea igual a 1



#Correlación de Spearman

cor(numvar$radius_mean,numvar$perimeter_mean)

cor(numvar$radius_mean,numvar$perimeter_mean,method="spearman")

#Cuando existe una fuerte asociación lineal, Pearson y Spearman 
# entregan resultados muy similares. 
#Pues una relacion lineal es monotonica.
#Pero no toda relacion monotonica es lineal.


graph<-ggplot(numvar, aes(x = radius_mean, y = perimeter_mean))+geom_point()+
  ggtitle("Relación entre radius mean y perimeter mean")+xlab("Radius mean")+
  ylab("Perimeter mean")+labs(subtitle="Diagnosis for breast cancer")+theme_minimal()+theme(
    plot.title = element_text(color = "red", size = 13, face = "bold"))

graph  #Muestra el grafico



##### Sesión 2 llega hasta aquí



###Ejemplo

# compactness_meanmean of perimeter^2 / area - 1.0

# concavity_meanmean of severity of concave portions of the contour

metodo_A<-breast_cancer$compactness_mean
metodo_B<-breast_cancer$concavity_mean

summary(metodo_A)

summary(metodo_B)

cov(metodo_A,metodo_B)

cor(metodo_A,metodo_B)

cor(metodo_A,metodo_B,method="spearman")


#Grafico de dispersión


library(ggplot2)

ggplot(data = datos, mapping = aes(x = metodo_A, y = metodo_B)) +
  geom_point(color = "black", size = 1) +
  labs(title = "Diagrama de dispersión", x = "método A", y = "método B") +
  geom_smooth(method = "lm", se = TRUE, color = "blue", lwd = 0.5) +
  geom_abline(intercept = 0, slope = 1, lwd = 0.7, col = "red") +
  theme(axis.line = element_line(colour = "black"), 
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(), 
        panel.background = element_blank()) +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5))


#Gráfico de Bland Altman

###Concordancia

diferencia <- metodo_A - metodo_B
media <- (metodo_A + metodo_B) / 2
porcentaje <- ((diferencia / media) * 100)
datos <- data.frame(metodo_A, metodo_B, diferencia, media, porcentaje)

#Grafico de Bland y Altman

install.packages("BlandAltmanLeh")

library(BlandAltmanLeh)
bland.altman.plot(metodo_A, metodo_B, main = "Bland-Altman Plot", 
                  xlab = "media método A y método B", 
                  ylab = "método A - método B", conf.int = .95)


##### Sesión 3 llega hasta aquí

