#Caso Poisson: 

#¿Cuándo usar Poisson? Cuando en un lapso de tiempo fijo se cuenta la llegada de 
#personas, o sucesos de interés.

#Vector aleatorio Poisson(13)

rpois(n=40,lambda=13)     #40 observaciones muestreadas
                          #aleatoriamente a través de esta distribución Pois(13)

set.seed(2020) #Fijo semilla de aleatoriedad

muestraX<-rpois(n=2000,lambda=13) #Guardo en muestraX, muestra tamaño 2000

mean(muestraX);var(muestraX) #Media y varianza de la muestra

#Son similares, ¿por qué?

#Una distribución Poisson cumple que Media=lambda, Varianza=lambda

set.seed(2020)
muestraY<-rpois(n=2000,lambda=6) #Defino otra muestra pero con lambda=6

pie(table(muestraX)) #Gráfico de torta, pero no se ve muy bien :(
                     #Sugiero este gráfico para categorías! o cuando hay pocos
                     #valores

barplot(table(muestraX)) #Gráfico de barras de la frecuencia de cada valor en la muestra

barplot(table(muestraX),xlab="Número de pasajeros en paradero", 
                        ylab="Frecuencia absoluta", 
                        main="Distribución de pasajeros en paradero recorrido 210v", 
                        las=1, 
                        col="cyan3")

#xlab="Número de pasajeros en paradero" <- Cambia nombre del eje x
#ylab="Frecuencia absoluta" <- Cambia nombre del eje y
#main <- Cambia el título
#las=1 <- Cambia la orientación de los números en el eje y
#col="cyan3" <- Cambia el color de las barras

barplot(table(muestraY))

barplot(table(muestraY), xlab="Número de pasajeros en paradero", 
                         ylab="Frecuencia absoluta", 
                         main="Distribución de pasajeros en paradero recorrido 229", 
                         col="chartreuse3",
                         las=1)

par(mfrow=c(1,2)) #Crea una ventana de 1 fila y dos columnas de gráficos


#Ahora se vuelven a graficar los dos gráficos anteriores:

barplot(table(muestraX), xlab="Número de pasajeros en paradero", ylab="Frecuencia absoluta", main="Distribución de pasajeros en paradero recorrido 210v", las=1, col="cyan3", cex.main=0.8)

barplot(table(muestraY), xlab="Número de pasajeros en paradero", ylab="Frecuencia absoluta", main="Distribución de pasajeros en paradero recorrido 229", las=1, col="chartreuse3", cex.main=0.8)

c(mean(muestraX), var(muestraX), mean(muestraY), var(muestraY))

Z<-muestraX+muestraY

dev.off() #<-Cierra la ventana de gráficos (si no quedarán las dos ventanas abiertas)

barplot(table(Z), las=1, xlab="Pasajeros", ylab="Frecuencia absoluta", main="Pasajeros de recurridos 210v y 229", cex.main=0.8, col="paleturquoise3")

mean(Z);var(Z)

#Como X ~ Poisson(13)
# Y ~ Poisson(6)

#Son independientes (porque son muestras aleatorias)

# ---> X+Y=Z~Poisson(13+6)

#Caso Binomial

npreguntas<-15 #Número de preguntas: Cantidad máxima de aciertos en el cuestionario

(aciertos<-0:npreguntas) #Secuencia de posibles aciertos

dbinom(aciertos,size=npreguntas,prob=0.62) #Entrega la probabilidad del vector
                                           #de posibles aciertos con tal parámetro

plot(aciertos, dbinom(aciertos,size=npreguntas,prob=0.62)) #Grafica n°aciertos y probabilidad

plot(aciertos,dbinom(aciertos,size=npreguntas,prob=0.62),
              xlab="N° de aciertos",  #Nombre eje x
              ylab="Probabilidad",    #Nombre eje y
              pch=19,
              type="o") #Tipo de unión entre los puntos, otras opciones: "o", "l", "p" 

plot(aciertos,dbinom(aciertos,size=npreguntas,prob=0.62), 
              xlab="N° de aciertos",
              ylab="Probabilidad", 
              type="b",
              pch=19, #Cambia el estilo del punto
              col="lightsteelblue4", #Cambia el color
              las=1)#Cambia la orientación de los valores en el eje y


#install.packages("extrafont") #Instalar este paquete solo si es windows, sino omitir todo lo de los tipos de letra
library(extrafont)

#font_import() #Importa tipos de letra

loadfonts()     #Carga tipos de letra disponibles
fonts()    #Muestra todos los tipos de letra

dev.off()
par(mfrow=c(1,2),family="Comic Sans MS") #Abre una fila con dos ventanas para gráfico
                                        #Tipo de letra es Myanmar Text

plot(aciertos,dbinom(aciertos,size=npreguntas,prob=0.62),
     type="b", 
     xlab="N° de aciertos", 
     ylab="Probabilidad", 
     pch=19, 
     col="lightsteelblue4",
     axes=FALSE,  #No grafica los ejes
     main="Probabilidad del n° de aciertos", #Título
     cex.main=0.9) #Tamaño de letra del título

axis(1, #1 indica que es el eje x
     at=aciertos, #Números o valores que quieren poner en el eje x
     col.axis="lightslategrey", #Color del eje
     las=1, #Orientación
     col.tick="darkblue") #Color de las líneas hacia el número

axis(2, #2 indica que es el eje y
     at=seq(0,0.2, by=0.02), #Secuencia de valores que recorre el eje y
     col.axis="lightslategrey", #Color del eje
     las=1, #Orientación
     col.tick="darkblue") #Color de las líneas hacia los números


#Gráfica de la Función de distribución:

plot(aciertos,pbinom(aciertos,size=npreguntas,prob=0.62),type="b", xlab="N° de aciertos", ylab="Probabilidad acumulada", pch=19, col="lightsteelblue4", las=1, axes=FALSE, main="Probabilidad acumulada hasta n° de aciertos", cex.main=0.9)

axis(1, at=aciertos, col.axis="lightslategrey", las=1, col.tick="darkblue")
axis(2, at=seq(0,1, by=0.1), col.axis="lightslategrey", las=1, col.tick="darkblue")

dev.off()

N<-73  #Número de participantes

(puntajes<-rep(NA, N)) #Vector vacío


#Se genera un for, se repite para cada participante la generación de 15 bernoullis
#1 o 0, donde 1 indicaría que acertó en determinada pregunta. La probabilidad de
#acierto se asume 0.62. 

#El puntaje para cada participante sería la suma del vector bernoulli aleatorio
#en cada iteración

for(i in 1:N){
  puntajes[i]<-sum(rbinom(n=15, size=1,prob=0.62))
}

puntajes  #Vector de puntaje de los 73 participantes

mean(puntajes); var(puntajes)

#n*p, n*p*(1-p)

15*0.62; 15*0.62*(1-0.62)

plot(ecdf(puntajes)) #Función de distribución

summary(ecdf(puntajes))

plot.stepfun(puntajes) #Función de distribución

#Caso hipergeométrica

#A mano

n<-1000 # tamaño población
r<-200  # tamaño de la muestra tomada
k<-60  # total de defectuosos
m<-0:60   # cantidad de defectuosos posibles en la muestra

prob<-choose(k,m)*choose(n-k,r-m)/choose(n,r)
prob

#Con la función

dhyper(m,k,n-k,r) #Cantidad de defectuosos posibles en la muestra (m)
                  #Total de defectuosos (k)
                  #Total de no defectuosos (n-k)
                  #Tamaño de la muestra tomada (r)

prob==dhyper(m,k,n-k,r) #No son exactamente iguales las probabilidades :( Oh no

all.equal(prob, dhyper(m,k,n-k,r)) #Pero sí aproximadamente iguales! 
                                   #(cuestión de decimales)


#Gráfico:

par(family="Bookman Old Style") #Tipo de letra a usar: Bookman Old Style

plot(prob, xlab="Productos defectuosos en la muestra", ylab="P(m)", main="Control de calidad de productos", pch=19, las=1, col="tan3")

#Valor esperado es k*r/n

rhyper(10000,k,n-k,r) #Muestra aleatoria 

mean(rhyper(10000,k,n-k,r))

k*r/(n)  #En el fondo es la proporción de defectuosos en la muestra por la cantidad
         #de defectuosos en la muestra!
