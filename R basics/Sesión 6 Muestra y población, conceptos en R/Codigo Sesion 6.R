library(rio)

setwd("C:/Users/HP/Desktop/Trabajo/2020-2/Laboratorio I. Estadística/Sesiones-Semanas/S7 Muestra y población") #Puedes fijar la ruta del archivo :)
born<-import("Born.csv")

dim(born)

set.seed(2020) #Semilla de aleatoriedad

(muestra<-sample(1:nrow(born), 10, replace = FALSE))

born$children[muestra] #Valores que toma la variable en la muestra

table(born$children[muestra]) #Cómo se distribuyen los valores

prop.table(table(born$children[muestra])) #Tabla de frecuencias relativas

summary(born$children[muestra]) #Estadísticas en la muestra

options(digits=2) #Usa menos decimales

prop.table(table(born$children[muestra])) #Muestra

prop.table(table(born$children)) #Poblacion

summary(born$children) #Poblacion


# Muestras mas grandes

set.seed(2020)
(muestra10<-sample(1:nrow(born), 10, replace = FALSE)) #Tamaño 10
set.seed(2020)
(muestra50<-sample(1:nrow(born), 50, replace = FALSE)) #Tamaño 50
set.seed(2020)
(muestra100<-sample(1:nrow(born), 100, replace = FALSE)) #Tamaño 100
set.seed(2020)
(muestra500<-sample(1:nrow(born), 500, replace = FALSE)) #Tamaño 500
set.seed(2020)
(muestra4000<-sample(1:nrow(born), 4000, replace = FALSE)) #Tamaño 4000

mean(born$children[muestra10])
mean(born$children[muestra50])
mean(born$children[muestra100])
mean(born$children[muestra500])
mean(born$children[muestra4000])

x<-c(10, 50, 100, 500, 4000)
y<-c(mean(born$children[muestra10]),mean(born$children[muestra50]),mean(born$children[muestra100]),mean(born$children[muestra500]),mean(born$children[muestra4000]))

plot(x,y) #Realiza gráfico

plot(x,y, xlab="Tamaño muestral", ylab="Media estimada", main="Estimación de la media por tamaño muestral")

plot(x,y, type="o",xlab="Tamaño muestral", ylab="Media estimada", main="Estimación de la media por tamaño muestral")


plot(x,y, type="l", xlab="Tamaño muestra",ylab="Media", cex.lab=1.5, main="Estimación de la media") #Gráfico
abline(h=mean(born$children), col="blue", lwd=2) #Añade línea horizontal


m<-nrow(born)  #Cantidad de filas
medias<-rep(0, m) #Vector que se llenará

for(i in 1:m){
  set.seed(2020)
  muestra<-sample(1:nrow(born), i, replace = FALSE)
  medias[i]<-mean(born$children[muestra])
}

plot(medias, type="l", ylab="Media", xlab="Tamaño muestra", cex.lab=1.5, main="Convergencia de la media estimada")
abline(h=mean(born$children), col="red", lwd=2) 

#Otra forma de hacer lo mismo:

muestra<-function(i){
  set.seed(2020)
  muestra<-sample(1:nrow(born), i, replace=FALSE)
  return(mean(born$children[muestra]))
}

medias<-sapply(1:nrow(born), FUN=muestra)

plot(medias, type="l", ylab="Media", xlab="Tamaño muestra", cex.lab=1.5, main="Convergencia de la media estimada")
abline(h=mean(born$children), col="red", lwd=2)
