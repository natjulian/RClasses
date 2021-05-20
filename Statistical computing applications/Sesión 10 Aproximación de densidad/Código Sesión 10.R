##### ESTIMACIÓN NO PARAMÉTRICA DE LA DENSIDAD

library(readr)
presion <- read_csv(file.choose())
print(presion)

y<-presion$y 

## HISTOGRAMA

hist(y)
summary(y)

#Sturges

n<-length(y)
ceiling(log(n,2))+1

hist(y, breaks=9) #por default

#Freedman-Diaconis

r<-IQR(y)
2*r/n^(1/3)

hist(y, breaks=26)

hist(y, breaks=26, plot=FALSE)$density

#Problema de este approach: No es suave, es muy tosco, un valor puede
#estar más cercano a un valor de otro intervalo que a un valor del intervalo
#donde cayó


#Kernel

## KERNEL GAUSSIANO

#install.packages("kdensity")
library("kdensity")

kernelg <- kdensity(y, kernel = "gaussian") #Calcula un óptimo de bandwith

summary(kernelg)

plot(kernelg, main = "Fluctuación de presión atmosférica")

## Otro ancho de banda

kernelg30 <- kdensity(y, kernel = "gaussian", bw=30)

plot(kernelg, main = "Fluctuación de presión atmosférica")
lines(kernelg30, col="red")

hist(y, freq=FALSE, main="Histograma de Fluctuación de presión", las=1)
lines(kernelg, col="blue")
lines(kernelg30, col="red")
legend("topleft", c("Kernel gaussiano",
                    "Kernel gaussiano bw=30"), lty = "solid", lwd=1,
       col=c("blue", "red"), bty="n", cex=0.6)

##OTROS KERNEL

kernelg10 <- kdensity(y, kernel = "gaussian", bw=10)
kernelb10 <- kdensity(y, kernel = "biweight", bw=10)
kernele10 <- kdensity(y, kernel = "epanechnikov", bw=10)

hist(y, freq=FALSE, main="Histograma de fluctuación de presión", las=1)
lines(kernelg10, col="blue")
lines(kernelb10, col="red")
lines(kernele10, col="goldenrod3")
legend("topleft", c("Kernel gaussiano bw=10",
                    "Kernel biweight bw=10", "Kernel epanechnikov bw=10"), lty = "solid", lwd=1,
       col=c("blue", "red", "goldenrod3"), bty="n", cex=0.6)

#Otra librería

#install.packages("demoKde")

library(demoKde)

hist(y, freq=FALSE, border="grey", las=1, main="Fluctuación de presión")
lines(stats::density(y), col="skyblue", lwd=3)
lines(kde(y))
lines(kde(y, kernel = kernelUniform), col="red")
rug(jitter(y), col="black")
legend("topleft", c("Densidad histograma",
                    "KDE gaussiano (denstiy)", "KDE gaussiano (kde)",
                    "KDE rectangular (kde)"), lty = "solid", lwd=c(1,3,1,1),
       col=c("grey", "skyblue", "black", "red"), bty="n", cex=0.6)


#Kernel rectangular, como su nombre indica toma una vecindad rectangular
#no tiene el mismo efecto suavizado del kernel gaussiano.


#Estimación bivariada con kernel gaussiano 

library(MASS)

x<-presion$x 

f2hat <- kde2d(x, y)  #bivariate normal kernel
contour(f2hat) 

filled.contour(f2hat,color.palette=terrain.colors) 

#Probar esto tiene más sentido cuando 2 variables podrían asumirse aprox
#normal


######################################## COMPLEMENTO
###### MISMA IDEA DE DENSIDAD PERO PARA AJUSTE

##Esta misma idea de suavizamiento para la densidad, se puede utilizar
#para suavizar una curva, incluyendo información de x e y:

library(ggplot2) #Carga el paquete ggplot2
df<-data.frame(x,y) #Crea dataframe df con x e y

(plot<-ggplot(data=df, aes(x, y))+ 
  geom_point()+ 
  xlab("Semana")+ 
  ylab("Fluctuación")+
  ggtitle("Fluctuación presión atmósferica por semana")+ 
  theme_minimal() +
    theme(
      panel.background = element_rect(fill = "transparent"), # bg of the panel
      plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
      panel.grid.major = element_blank(), # get rid of major grid
      panel.grid.minor = element_blank(), # get rid of minor grid
      legend.background = element_rect(fill = "transparent"), # get rid of legend bg
      legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
    )+theme(plot.title=element_text(hjust=0.5))
)


ggsave(plot, filename = "fluct.png",  bg = "transparent")

#Evidentemente, ajustar una recta no sería lo ideal. Tampoco mirar
#el coeficiente de regresión lineal

plot2<-plot+
  geom_smooth(method='lm', formula=y~x, se=FALSE)+theme(
    panel.background = element_rect(fill = "transparent"), # bg of the panel
    plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    legend.background = element_rect(fill = "transparent"), # get rid of legend bg
    legend.box.background = element_rect(fill = "transparent") # get rid of legend panel bg
  )+theme(plot.title=element_text(hjust=0.5))

ggsave(plot2, filename = "Rplot03.png",  bg = "transparent")

cor(x,y)

#Sobretodo si lo que nos interesa capturar son bajas y altas!!

#Suavizamiento gaussiano

suvnor<-stats::ksmooth(x,y, bandwidth = 17, kernel="normal") #Suavizamiento con kernel gaussiano y ancho de banda 17
suvnor2<-stats::ksmooth(x,y, bandwidth = 3, kernel="normal") #Suavizamiento con kernel gaussiano y ancho de banda 3

names(suvnor)

nor1<-data.frame(
  x=suvnor$x,
  y=suvnor$y,
  metodo="Kernel gaussiano h=17"
)

nor2<-data.frame(
  x=suvnor2$x,
  y=suvnor2$y,
  metodo="Kernel gaussiano h=3"
)

ggplot(dat=rbind(nor1, nor2), aes(x,y))+
  geom_point(dat=df, aes(x,y), col="thistle4", alpha=0.4)+
  geom_line(col="violetred4")+
  facet_wrap(~metodo)+
  theme_minimal()+ylab("Fluctuación")+xlab("Semana")

#Suavizamiento rectangular

suavbox1<-stats::ksmooth(x, y, bandwidth=17 , kernel="box") #Suavizamiento con kernel rectangular y ancho de banda 17

suavbox2<-stats::ksmooth(x, y, bandwidth=3 , kernel="box") #Suavizamiento con kernel rectangular y ancho de banda 3

box1<-data.frame(
  x=suavbox1$x,
  y=suavbox1$y,
  metodo="Kernal rectangular h=17"
)

box2<-data.frame(
  x=suavbox2$x,
  y=suavbox2$y,
  metodo="Kernal rectangular h=3"
)

ggplot(dat=rbind(box1, box2), aes(x, y))+
  geom_point(data=df, aes(x,y), col="blue", alpha=0.4)+ #Customizo el grafico
  geom_line(col="red")+
  facet_wrap(~metodo)+
  theme_minimal()+ylab("Fluctuación")+xlab("Semana")


#Comparamos gaussiano con rectangular

ggplot(dat=rbind(nor2, box2), aes(x, y))+
  geom_point(data=df, aes(x,y), col="blue", alpha=0.4)+ #Customizo el grafico
  geom_line(col="red")+
  facet_wrap(~metodo)+
  theme_minimal()+ylab("Fluctuación")+xlab("Semana")

