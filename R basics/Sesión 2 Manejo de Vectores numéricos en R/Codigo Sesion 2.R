# Vectores numéricos

c(1/2, exp(3), 1, -1, 0, 1/9, 2, -6*2)
c(1,0,1,1,1,1,0,1,0,1)
c(-1/2, 1, -3/2, 2)
c(-exp(1), -exp(2)*4, -exp(3), -exp(4))

c(3,4,20, -1, 0, 1/10, 2**3, log(5))

round(c(3,4,20, -1, 0, 1/10, 2**3, log(5)), 2)


# Vectores de secuencias

0:4
seq(0,4)
seq(0,4,by=0.04)
seq(0,4,len=100)


?seq

# Vectores de repeticiones

rep(1:9,4)
rep(1:5,each=2)
rep(c(1,5),c(3,7))
rep(c(1,5),len=9)

# Vectores compuestos

c(rep(c(2,3),3),0,1,-2,seq(15,17,by=0.5))

c(seq(0,5),rep(c(6,7,8),len=8),1, 10, log(4), 2^{5})


#Creando objetos con vectores

#Creamos un objeto con las mesadas de Leo:
mesada<-c(500, 1000, 350, 600, 450, 760) 

#Le restamos los 150 pesos que gasta en stickers:

mesada-rep(150, 6)

mesada-150 #son equivalentes

mesada-c(200, 300, 100, 50, 100, 200)

mesada*2

mesada/2

log(mesada)

#Extraer elementos de un vector

mesada[1] #Extrae el primer elemento del vector

mesada[-1] #Extrae todos los elementos excepto el primero

mesada[length(mesada)] #Extrae el último elemento del vector

mesada[1:3] #Extrae los primeros tres elementos

mesada[order(mesada)] #Extrae los elementos ordenados de menor a mayor

mesada[order(mesada, decreasing=TRUE)] #de mayor a menor


mesada[1]<-700

mesada[3:6]<-c(100, 200, 600, 800)

mesada2<-mesada[2:4]

mesada2[order(mesada2)]

mesada3<-c(mesada, 400, 500)