#Carga un archivo de tipo RData:

load(file.choose())

#Vista previa:

nganadas
njugadas
personaje

#Formato:

class(nganadas)
class(njugadas)
class(personaje)

#Extensión:

length(nganadas);length(njugadas);length(personaje)


#¿Cómo poder ver solo algunos elementos y no todos?

head(personaje, 20) #Muestra los primeros 20 personajes
tail(personaje, 5)  #Muestra los ultimos 5 personajes


#Tabla de frecuencias de las partidas ganadas

table(nganadas)

#Hubo 17 personajes con los que no ganó ninguna de las partidas
#Hubo 1 personaje con el que ganó 37 partidas 

personaje[order(nganadas, decreasing=TRUE)] 

#Ordena los personajes de manera decreciente respecto al n° de partidas 
#ganadas 

head(personaje[order(nganadas, decreasing=TRUE)], 5)

tail(personaje[order(nganadas, decreasing=TRUE)], 5)


personaje[which.max(nganadas)]

#Efectivamente son 37 partidas ganadas con Kai'sa

(njugadas==1)&(nganadas==1)  #Vector de pruebas logicas aplicado a cada personaje 

table((njugadas1)&(nganadas==1)) #Indica cantidad de TRUE y FALSE

which((njugadas==1)&(nganadas==1)) #Indica cuales son

personaje[which((njugadas==1)&(nganadas==1))] #Indica nombre de estos personajes

#Verificamos:

nganadas[which((njugadas==1)&(nganadas==1))]
njugadas[which((njugadas==1)&(nganadas==1))]


(probabilidad<-nganadas/njugadas)


njugadas


#Hay casos en los que sólo se jugó una partida y si la perdió la probabilidad
# de ganar con dicho personaje arroja 0

#el n importa!

(probfiltro<-probabilidad[which(njugadas>=10)])
(personajefiltro<-personaje[which(njugadas>=10)])


probfiltro[which(personajefiltro=="Yuumi"|personajefiltro=="Nautilus"|personajefiltro=="Galio")]
personajefiltro[which(personajefiltro=="Yuumi"|personajefiltro=="Nautilus"|personajefiltro=="Galio")]

#Se recomienda menos Galio, tiene menor probabilidad de victoria


