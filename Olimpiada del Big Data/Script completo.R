#################### CLASE 1

### Bienvenid@ al curso de R :)

2**(-1/4)

log(50)

# Aprendiendo RStudio 

a<-1000000000000
b<-20

(1000000000000*20)/(20**exp(1))*log(20)/1000000000000

(a*b)/(b**exp(1))*log(b)/a


#Ejercicios

impar<-sqrt(625)
par<-4*sin(3*pi/2)

productoabs<-abs(par/impar)


#Proximamente

vector1<-c(2, -1, 7/3, pi/2)

#################### CLASE 2

cantidades<-c(-1,1.6,0,0,0,0,7)

class(cantidades)

nombres<-c("Miguel","José","Ricardo","María")

class(nombres)

fracciones<-c(4/5,7,0,-1/3,8)

class(fracciones)

valorlogico<-c(TRUE,FALSE,TRUE,TRUE,TRUE,FALSE,FALSE,FALSE)

class(valorlogico)

class(c(1,2,"José","Ariel","B"))


0:4
seq(0,4)
seq(0,4,by=0.5)
seq(0,4,len=4)


rep(1,5)
rep(1:5,each=2)
rep(c(1,5),c(3,7))
rep(c(1,5),len=9)

c(rep(c(2,3),3),0,1,-2,seq(15,17,by=0.5))


c(seq(0,5,by=1),rep(c(6,7,8),len=8),1)


vector1<-c(2,4,1,seq(9,20,by=0.3))

length(vector1)

sum(vector1)
prod(vector1)

min(vector1)
which.min(vector1)

max(vector1)
which.max(vector1)

vector1[1]
vector1[3]

vector1[1:3]

vector1[length(vector1)]

mean(vector1)
median(vector1)

quantile(vector1, 0.5)

sort(vector1)

order(vector1)

vector1[order(vector1)]

pais<-c("Hungria", "Reino Unido", "Australia", "Canada", 
        "Irlanda", "Luxemburgo", "Estados Unidos")

porcentaje<-c(19.5, 25, 27, 25, 23, 23, 35)

class(pais)
class(porcentaje)


length(pais)

median(porcentaje)
mean(porcentaje)

min(porcentaje)
max(porcentaje)

which.max(porcentaje)

pais[which.max(porcentaje)]
pais[which.min(porcentaje)]


x<-5 #Bastian
z<-10 #Lorena

x<z

x>z

x==z

x!=z

(x<10)&(z<25)


(x>5)|(z<3)


porcentaje

porcentaje<19

which((porcentaje>20)&(porcentaje<25))

table(porcentaje==24)

table(porcentaje==27)

which(porcentaje==27)

table(porcentaje)

table(porcentaje>30)

which(porcentaje>30)


#################### CLASE 3

pais<-c("Hungria", "Reino Unido", "Australia", "Canada", 
        "Irlanda", "Luxemburgo", "Estados Unidos")

porcentaje<-c(19.5, 25, 27, 25, 23, 23, 35)


porcentaje<19

table(porcentaje<19)

table((porcentaje>20)&(porcentaje<25))

pais[which((porcentaje>20)&(porcentaje<25))]

porcentaje==24

table(porcentaje==27)

table(porcentaje)


table(porcentaje>30)

pais[which(porcentaje>30)]


vector1<-seq(0, 20, len=8)
vector2<-seq(30, 70, len=8)


cbind(vector1, vector2)

rbind(vector1, vector2)


vector3<-c("h", "a","m", "p", "o","f", "g","p")

class(vector3)


cbind(vector1, vector2, vector3)


data.frame(vector1, vector2, vector3)


class(pais)
class(porcentaje)


cbind(pais, porcentaje)

tabla1<-data.frame(pais, porcentaje)



peso<-c(55,90,80,45,59) #Se define la información por columna
altura<-c(163,174,163,150,147)

cbind(peso, altura)



paciente1<-c(55,163) #Se define la información por fila
paciente2<-c(90,174)
paciente3<-c(80,163)
paciente4<-c(45,150)
paciente5<-c(59,147)

rbind(paciente1, paciente2, paciente3, paciente4, paciente5)


matrix(c(peso,altura),nrow=5,ncol=2)


matrix(c(55,90,80,45,59,163,174,163,150,147),ncol=2,nrow=5)


matrix(c(55,163,90,174,80,163,45,150,59,147),byrow=TRUE,ncol=2,nrow=5)


(DF<-data.frame("peso"=c(55,90,80,45,59),
                "altura"=c(163,173,163,150,147)))


colnames(DF)<-c("peso (kg)", "altura (cms)")

rownames(DF)<-c("Paciente 1", "Paciente 2", "Paciente 3", "Paciente 4", "Paciente 5")


dim(DF)

#################### CLASE 4

(DF<-data.frame("peso"=c(55,90,80,45,59),
                "altura"=c(163,173,163,150,147)))


colnames(DF)<-c("peso (kg)", "altura (cms)")

rownames(DF)<-c("Paciente 1", "Paciente 2", "Paciente 3", "Paciente 4", "Paciente 5")


DF[,1]

DF[,2]

DF[,1:2]

DF[4,]

DF[2,]

DF[4:5,]

#Opcion 1

DF$`peso (kg)`/(DF$`altura (cms)`/100)**2

#Opcion 2

DF[,1]/(DF[,2]/100)**2


DF[,3]<-DF[,1]/(DF[,2]/100)**2


colnames(DF)[3]<-"IMC"

which(DF$`peso (kg)`>60)

DF[which(DF$`peso (kg)`>60),1]/(DF[which(DF$`peso (kg)`>60),2]/100)**2

DF$`peso (kg)`[which(DF$`peso (kg)`>60)]/(DF$`altura (cms)`[which(DF$`peso (kg)`>60)]/100)**2


#################### CLASE 5

library(readr)
hitspotify <- read_csv("C:/Users/HP/Desktop/Datos/hitspotify.csv")
View(hitspotify)

install.packages("tidyverse")

library(tidyverse)

glimpse(hitspotify)

hitspotify<-hitspotify[ , -1]

View(hitspotify)

summary(hitspotify)


which.max(hitspotify$Popularity)

hitspotify$Track.Name[which.max(hitspotify$Popularity)]

hitspotify[which.max(hitspotify$Popularity), ]

order(hitspotify$Popularity, decreasing = TRUE)

hitspotify$Track.Name[order(hitspotify$Popularity, decreasing = TRUE)]


which(hitspotify$Popularity>quantile(hitspotify$Popularity, 0.75))

populars<-hitspotify[which(hitspotify$Popularity>quantile(hitspotify$Popularity, 0.75)), ]

nrow(populars)

table(populars$Genre)

#Base de datos breast cancer


library(readr)
breast_cancer <- read_table2("C:/Users/HP/Desktop/Datos/breast-cancer.txt")
View(breast_cancer)


table(breast_cancer$diagnosis)


Benigno<-subset(breast_cancer, diagnosis=="B")

Maligno<-subset(breast_cancer, diagnosis=="M")


summary(Benigno$texture_mean)

summary(Maligno$texture_mean)


#Base de datos pokedex

library(readxl)
pokedex <- read_excel("C:/Users/HP/Desktop/Datos/pokedex.xlsx")
View(pokedex)

table(pokedex$`Type 1`)

table(pokedex$Generation)

plot(pokedex$Attack, main="Ataque de pokemones", ylab="Ataque")

