1-pnorm(7,mean=6.5,sd=1) #P(X>7)


mu<-6.5

sd<-1 

#Gráfico

#install.packages("extrafont") #No es necesario correr si ya está instalado
#library(extrafont)

#font_import()
#loadfonts()     #Importa tipos de letra de windows
#fonts()    #Muestra todos los tipos de letra

#par(family="Myanmar Text") #Se define el tipo de letra

x<-seq(3,10,length=10000)
y<-1/sqrt(2*pi*sd^{2})*exp(-(x-mu)^2/(2*sd^{2}))
plot(x,y,type="l",lwd=2,col="mediumpurple1",las=1,xlab="Valores de X",ylab="Densidad",main="X ~ N(6.5,1)")

#Área sombreada

a<-7
b<-Inf

bounds_filter<-(x>=a)&(x<=b)
x_within_bounds<-x[bounds_filter]
y_within_bounds<-y[bounds_filter]

x_polygon<-c(a,x_within_bounds,b)
y_polygon<-c(0,y_within_bounds,0)

polygon(x_polygon, y_polygon, col = "lightslateblue", border=NA)


probability_within_bounds <- pnorm(b, mu, sd) - pnorm(a, mu, sd)

text <- paste("p(", a, "< X <", b, ") =", signif(probability_within_bounds, digits = 3))

mtext(text)


# Calculo de la integral

densidad<-function(x){
  1/sqrt(2*pi*sd^{2})*exp(-(x-mu)^2/(2*sd^{2}))
}

integrate(densidad,lower=a,upper=Inf) #Coincide con 1-pnorm(7,mean=6.5,sd=1) #P(X>7)

integrate(densidad,lower=-Inf,upper=Inf)  #Area debe ser 1

#El valor esperado en una variable continua se define como
#la integral de x*función de densidad

VEsperado<-function(x){
  x/sqrt(2*pi*sd^{2})*exp(-(x-mu)^2/(2*sd^{2}))
}

integrate(VEsperado, lower=-Inf,upper=Inf)  

#Da efectivamente la media :)

#Datos anime

library(rio)

anime<-import(file.choose())

summary(anime)

#El comando summary entrega información para las variables
#cuantitativas

#Algunas variables tienen NA's, esto significa que hay 
#registros que no se especificaron o están faltantes

View(anime)

is.na(anime$rating) #Indica con TRUE si el registro es faltante

which(is.na(anime$rating)) #Indica cuáles registros son faltantes
 
anime<-anime[-which(is.na(anime$rating)),] #Quito los registros donde hay ratings faltantes

summary(anime) #Ya no aparecen NA's

summary(anime$rating)

#Histograma
hist(anime$rating)

hist(anime$rating, plot=FALSE)

hist(anime$rating, plot=FALSE)$breaks #Puntos de corte
hist(anime$rating, plot=FALSE)$counts #Cuántas observaciones cayeron en cada intervalo

hist(anime$rating, plot=FALSE)$equidist #Indica si los intervalos se calcularon de manera equidistante

#Cambiamos título y nombre de los ejes: main=, xlab=, ylab=

hist(anime$rating, main="Histograma de los ratings de anime",
     xlab="Ratings de anime",
     ylab="Frecuencia absoluta",
     las=1)

#Cambiamos cantidad de barritas con breaks:

hist(anime$rating, main="Histograma de los ratings de anime",
     xlab="Ratings de anime",
     ylab="Frecuencia absoluta",
     las=1,
     breaks=50)


#Si quisieramos observar la información de este histograma, usamos plot=FALSE

hist(anime$rating,plot=FALSE,breaks=50)

hist(anime$rating,plot=FALSE,breaks=50)$breaks  #Puntos de corte (intervalos)

hist(anime$rating,plot=FALSE,breaks=50)$count   #Frecuencias absolutas por intervalo

which.max(hist(anime$rating,plot=FALSE,breaks=50)$count) #¿en dónde se observa la mayor frecuencia?

#Intervalo:
c(hist(anime$rating,plot=FALSE,breaks=50)$breaks[which.max(hist(anime$rating,plot=FALSE,breaks=50)$count)], hist(anime$rating,plot=FALSE,breaks=50)$breaks[which.max(hist(anime$rating,plot=FALSE,breaks=50)$count)+1])

hist(anime$rating,plot=FALSE,breaks=50)$count[which.max(hist(anime$rating,plot=FALSE,breaks=50)$count)]  #Frecuencia en ese intervalo


#Si queremos observar frecuencias relativas freq=FALSE

hist(anime$rating, 
     main="Histograma de los ratings de anime",
     xlab="Ratings de anime",
     ylab="Frecuencia relativa",
     las=1,
     breaks=50,
     freq=FALSE,
     col="thistle2")


#Añadir la curva de densidad

lines(density(anime$rating), col="peachpuff4") 

#Aumentar el grosor de la curva

lines(density(anime$rating),col="peachpuff4",lwd=2)


#Añadir curva de distribución normal

curve(dnorm(x, mean=mean(anime$rating), sd=sd(anime$rating)), 
      col="purple1", lwd=2,add=TRUE)

legend("topleft",legend="Curva Distribución Normal",col="purple1",cex=0.8,pch=20)


#Boxplot

#El boxplot también puede utilizarse para variables discretas!

boxplot(anime$rating,
        main="Boxplot de ratings de anime",
        las=1, 
        col="thistle4")


#Detectando outliers

Q1<-quantile(anime$rating,0.25)
Q3<-quantile(anime$rating,0.75)

RIC<-Q3-Q1

which(anime$rating<Q1-1.5*RIC|anime$rating>Q3+1.5*RIC)


#par(family="Myanmar Text") #Se define el tipo de letra

time<-1:10
nbact<-c(2,4,8,16,32,64,128,256, 512,1024)
plot(time, nbact, main="Crecimiento de bacterias en placa de petri", ylab= "Bacterias", pch=16, col="mediumseagreen",cex.main=1.3, cex=1.2,cex.lab=1.2, cex.axis=1.2,bty="l", xlab="Segundos")


mean(nbact)

lambda<-1/mean(nbact)

simulacion<-rexp(10, rate=lambda)

simulacion[order(simulacion)]

cumsum(simulacion[order(simulacion)])


par(mfrow=c(1,2))
plot(time, nbact, main="Crecimiento real", ylab= "Bacterias", pch=16, col="mediumseagreen",cex.main=1.3, cex=1.2,cex.lab=1.2, cex.axis=1.2,bty="l", xlab="Segundos", las=1)
plot(time, cumsum(simulacion[order(simulacion)]), main="Crecimiento simulado", ylab= "Bacterias", pch=16, col="maroon3",cex.main=1.3, cex=1.2,cex.lab=1.2, cex.axis=1.2,bty="l", xlab="Segundos", las=1)

