#Vectores de caracteres

c("Natalie", "Sergio", "Vanesa")
c("Papas fritas", "Mayonesa", "Doritos", "Cheetos")
c("Cuaderno", "Lapiz", "Regla", "Post it", "Pegamento")

#eliminar el objeto mesada:

mesada<-c(500, 1000, 350, 600, 450, 760)

rm(list=c("mesada"))

rm(list=ls())


palabras<-c("hola", "me", "llamo", "Natalie")

length(palabras)

nchar(palabras)

substr(palabras, start=1, stop=2)

substr(palabras, start=2, stop=nchar(palabras))


parrafo<-c("hola me llamo Natalie")
#Ejercicios 

Info<-c("12466824Anciano","19566573Joven","18622134Adulto","17823471Adulto","20172423Infante","19784132Joven","17234124Adulto")

IMC<-c(19.5,25,27,25,23,23,35)

class(Info)

class(IMC)

# ¿Cuántos pacientes se estudiaron?

length(Info) #7 pacientes


# Extraiga el rango etario de los pacientes en un vector etario


etario<-substr(Info, start=9, stop=nchar(Info))

substr(Info, start=1, stop=nchar(Info)-1)  #Extrae hasta el penultimo digito

substr(Info, start=nchar(Info)-1, stop=nchar(Info)) #Extrae ultima silaba

# Y si queremos el rut? 

(RUT<-substr(Info, start=1, stop=8))


RUT<-as.numeric(RUT)   #modifica el formato a numerico


#  Obtenga mínimo, máximo, media y mediana de los índices de obesidad. Interprete.

min(IMC) #Minimo

max(IMC) #Maximo

mean(IMC) #Media

median(IMC) #Mediana

summary(IMC)


# ¿Cuál es el paciente que posee el mayor índice de obesidad?

which.max(IMC) ; etario[which.max(IMC)]

which.min(IMC); etario[which.min(IMC)]


#Conjuntos en R


A<-c("Palta", "Leche", "Pan", "Queso", "Arroz", "Harina", "Chocolates", "Shampoo", "Fideos", "Salsa de tomate", "Servilletas")
B<-c("Toalla", "Shampoo", "Pan", "Cepillo dental", "Queso", "Jugo", "Leche", "Servilletas")


union(A, B)

intersect(A, B)

setdiff(A, B)  #A-B

setdiff(B, A)  #B-A


#Forma 1
setdiff(union(A, B), intersect(A, B))

#Forma 2
union(setdiff(A, B), setdiff(B, A))

palabras<-c("Hola", "Bienvenid@s")

palabras2<-c("Bienvenid@s", "Hola")

setequal(palabras, palabras2)


unique(c("hola", "hola", "me", "llamo", "llamo", "llamo"))

