load(file.choose())

probabilidad<-nganadas/njugadas

#Con cbind los vectores se pegan verticalmente, uno al lado del otro

datosvertical<-cbind(njugadas, probabilidad)

head(datosvertical, 7)

dim(datosvertical) #numero de filas y numero de columnas

#Con rbind los vectores se pegan horizontalmente, uno debajo del otro

datoshorizontal<-rbind(njugadas, probabilidad)

View(datoshorizontal)

dim(datoshorizontal)

class(datosvertical)  #Ambas son matrices
class(datoshorizontal)


#matrix()

#Equivalentes a cbind:

matrix(c(njugadas, probabilidad), ncol=2)
dim(matrix(c(njugadas, probabilidad), ncol=2))

matrix(c(njugadas, probabilidad), nrow=98)
dim(matrix(c(njugadas, probabilidad), nrow=98))

?matrix

class(matrix(c(njugadas, probabilidad), nrow=98))


#¿Y si queremos unificar toda la información?

leaguecbind<-cbind(personaje, njugadas, nganadas, probabilidad)

head(leaguecbind, 15)

#data.frame()

(df<-data.frame(personaje, njugadas, nganadas, probabilidad))

head(df, 10)
tail(df, 5)

####### Extraer elementos

### Extraer columnas/variables

df$personaje

df[ , 4]

names(df)

df$probabilidad

df[,4]

df[,3:4]

### Extraer filas

df[6,]

df[7:8,]

df[3,4]

df[1:2,3:4]

### Extraer varias filas y columnas

df[seq(1, 5, by=2), c(1,4)]


###Filtrar datos

df[which(df$probabilidad>0&df$probabilidad<0.5), ]

df[which(df$personaje=="Nautilus"), ]

df[which(df$personaje=="Sion"), ]

df[which(df$personaje=="Sion"|df$personaje=="Nautilus"), ]

df[which(df$nganadas<10), 1]

### Funciones aplicables a tablas de datos

summary(df) #Resumen de las variables

str(df) #Indica cómo se lee cada variable

dim(df) #Dimensiones 

nrow(df) #Número de filas
ncol(df) #Número de columnas

names(df) #Nombre de las columnas

class(df) #Indica el tipo de objeto, notar que ya no es matrix

View(df) #Vista previa
