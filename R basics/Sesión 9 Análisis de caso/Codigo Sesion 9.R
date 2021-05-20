#### PARTE A) ANÁLISIS PREVIO 
#Esta parte corresponde a trabajo previo de los datos para
#su análisis. Puede corresponder a "limpieza de datos", 
#recodificación de variables, revisión del formato de las
#variables, determinar si no existen registros duplicados, 
#etcétera. Esta parte no se incluye en el informe o reporte.

#a.1)

library(rio) #Cargamos la librería rio

anuncios<-import(file.choose()) #La función import carga los datos
                                #file.choose() abre una nueva ventana 
                                #para seleccionar el archivo de datos

dim(anuncios) #Vemos la dimensión de la tabla de datos

names(anuncios) #Nombre de las variables, ojo con el Identificador

str(anuncios$Estacionamientos) #Revisar siempre el formato
                               #Y verificar que se lean 
                               #en el formato adecuado

table(anuncios$Estacionamientos) #Vemos que existe un valor "No"

unique(anuncios$Estacionamientos) #Valores de la variable

#Otra forma de determinar los registros que no son numericos:

as.numeric(anuncios$Estacionamientos) #Cuando aparece NA era texto

anuncios$Estacionamientos[is.na(as.numeric(anuncios$Estacionamientos))] #El valor No es el que ocasiona problemas

#Cambiamos todos los registros "No" a cero:
anuncios$Estacionamientos[which(anuncios$Estacionamientos=="No")]<-0

#Le aplicamos formato as.numeric:
anuncios$Estacionamientos<-as.numeric(anuncios$Estacionamientos)

str(anuncios$Estacionamientos) #Ahora se lee en formato numérico! :)


#a.2)

unique(anuncios$Tipo) #Sólo toma el valor "Casa"

#a.3)

table(table(anuncios$Identificador))


#### PARTE B) ANÁLISIS ESTADÍSTICO

#En esta parte se realizan todos los análisis solicitados,
#ya nuestros datos se encuentran limpios y trabajables. 
#Esta parte va en el reporte, se trabajan estadísticas,
#gráficos, tablas, entre otros, y lo más importante es que 
#cada recurso debe estar acompañado de comentarios valiosos
#e informativos.

unique(anuncios$Comuna) #Comunas con anuncios de casas en venta

table(anuncios$Comuna) #Tabla con cantidad de anuncios por comuna

comunas<-data.frame(table(anuncios$Comuna)) #Guardamos la info en una dataframe

comunas[which.max(comunas$Freq), ] #Comuna con mayor n° de anuncios de casas en venta

comunas[which.min(comunas$Freq), ] #Comuna con menor n° de anuncios de casas en venta

comunas<-comunas[order(comunas$Freq, decreasing=TRUE),] #Ordenamos de forma decreciente

head(comunas, 5) #Comunas con mas anuncios de casas en venta
tail(comunas, 5) #Comunas con menos anuncios de casas en venta

summary(comunas$Freq) #Información de la distribución de anuncios

barplot(comunas$Freq) #Muestra n° de anuncios por comuna

barplot(comunas$Freq, 
        names.arg=comunas$Var1) #Le agregamos los nombres respectivos

barplot(comunas$Freq, 
        names.arg=comunas$Var1, 
        las=2) #Cambia la orientación para que se vean mejor los nombres

barplot(comunas$Freq, 
        names.arg=comunas$Var1, 
        las=2,
        cex.names=0.6) #Cambio tamaño de letra de los nombres

barplot(comunas$Freq, 
        names.arg=comunas$Var1, 
        las=2,
        cex.names=0.6,
        col="lightslateblue") #Añadimos un color lindo

barplot(comunas$Freq, 
        names.arg=comunas$Var1, 
        las=2,
        cex.names=0.6,
        col="lightslateblue",
        density=15) #Un diseño bonito

barplot(comunas$Freq, 
        names.arg=comunas$Var1, 
        las=2,
        cex.names=0.6,
        col="lightslateblue",
        density=15,
        space=0.8) #Más espacio entre barras

barplot(comunas$Freq, 
        names.arg=comunas$Var1, 
        las=2,
        cex.names=0.6,
        col="lightslateblue",
        density=15,
        space=0.8,
        main="Anuncios de casas en venta por comuna en Mayo")

#El título jamás debe faltar!


#install.packages("extrafont")
#library(extrafont)

#font_import()
#loadfonts()     #Importa tipos de letra de windows
#fonts()    #Muestra todos los tipos de letra

#par(family="Leelawadee UI Semilight") #Se define el tipo de letra

barplot(comunas$Freq, 
        names.arg=comunas$Var1, 
        las=2,
        cex.names=0.6,
        col="lightslateblue",
        density=15,
        space=0.8,
        main="Anuncios de casas en venta por comuna en Mayo")

#También se pudo hacer un boxplot

boxplot(comunas$Freq,
        col="lightskyblue3", #Color
        main="Número de anuncios por comunas en Mayo")

summary(comunas$Freq) #Estadísticas

#par(family="Segoe UI Symbol")

#O también un gráfico de la función de distribución empírica
plot(ecdf(comunas$Freq), main="Función de distribución acumulada empírica", las=1, col="lightslateblue")
quantile(comunas$Freq, c(0.2, 0.4, 0.6, 0.8))

#b.2.1)

boxplot(anuncios$`Metros cuadrados`~anuncios$Comuna)

boxplot(anuncios$`Metros cuadrados`~anuncios$Comuna, 
        xlab="",
        ylab="Metros cuadrados",
        main="Metros cuadrados de casas en venta por comunas",
        las=2,
        cex.axis=0.6,
        col=rainbow(32, alpha=0.5, start=0))


### LA SESIÓN 10 ACABÓ AQUÍ 

#Pero podríamos añadir colores más informativos!

library(dplyr)

#dplyr es una librería SÚPER útil para obtener
#estadísticas 

anuncios%>%
  group_by(Comuna)%>% #Agrupa por comuna
  summarise(Min=min(`Metros cuadrados`)) #Para cada comuna calcula el minimo de metros cuadrados


df<-anuncios%>%
  group_by(Comuna)%>%
  summarise(Min=min(`Metros cuadrados`), #Le podemos añadir más cosas!
            median=median(`Metros cuadrados`),
            Media=mean(`Metros cuadrados`),
            Max=max(`Metros cuadrados`),
            n=n()) #n() cuenta la cantidad de registros por comuna (n° de anuncios)


df #Ahora tengo una tabla con estadísticas de metros cuadrados por comuna


# Pondremos con determinado color aquellas comunas en las que 
#se presenta mayor mediana (entre el 80% mayor) de metros cuadrados

colors<-ifelse(df$median>quantile(df$median, 0.8),  "turquoise3",
                                                    "turquoise4")

#par(family="Segoe UI Symbol")

boxplot(anuncios$`Metros cuadrados`~anuncios$Comuna,
        xlab="",
        ylab="Metros cuadrados",
        main="Metros cuadrados de casas en venta por comunas",
        col=colors,
        las=2,
        cex.axis=0.6)

#Se pueden quitar los outliers con outline=FALSE
#Lo recomiendo SOLO si quieren ver un Zoom de los datos
#Los outliers pueden ser muy importantes!

#b.2.2)

hist(df$Media)

hist(df$Media, 
     main="Histograma de los Metros Cuadrados Promedio",
     xlab="Metros cuadrados promedio",
     ylab="Frecuencia relativa",
     las=1)

hist(df$Media, 
     main="Histograma de los Metros Cuadrados Promedio",
     xlab="Metros cuadrados Promedio",
     ylab="Frecuencia relativa",
     las=1,
     breaks=30,
     freq=FALSE)

#Podemos editar los ticks de los ejes:

hist(df$Media, main="Histograma de los Metros Cuadrados Promedio",
     xlab="Metros cuadrados Promedio",
     ylab="Frecuencia relativa",
     las=1,
     breaks=30,
     freq=FALSE,
     col="cyan3",
     xlim=c(0, max(df$Media)),
     ylim=c(0, 0.016), 
     axes=FALSE)

axis(1, at=round(seq(0, max(df$Media), len=15),0), cex.axis=0.8, las=2)
axis(2, at=seq(0, 0.016, by=0.002), cex.axis=0.9, las=1)

#Añadimos la curva:
curve(dnorm(x, mean=mean(df$Media), sd=sd(df$Media)),
      col="maroon", lwd=2,add=TRUE, lty=1)

#Se observa que quizás los parámetros usados no sean los más adhoc

#Probamos con otros parámetros:
curve(dnorm(x, mean=130, sd=28), #Normal adicional
      col="navyblue", lwd=2,add=TRUE, lty=2)

legend(x=177, y=0.016,legend=c("Curva Normal media y desviación empíricas", "Curva Normal alternativa"),
       col=c("maroon", "navyblue"),cex=0.8,pch="", bty="n", lty=1:2)



#b.3.1)

#Necesitamos calcular por comuna el promedio de habitaciones y
#el promedio de metros cuadrados

df2<-anuncios%>%
  group_by(Comuna) %>%
  summarise(muhabitaciones=mean(Habitaciones), 
            mumetros=mean(`Metros cuadrados`))

head(df2)

cor(df2$muhabitaciones,df2$mumetros) #Correlación de Pearson

plot(df2$muhabitaciones, df2$mumetros)
plot(df2$muhabitaciones, df2$mumetros, 
     las=1)

plot(df2$muhabitaciones, df2$mumetros, 
     las=1, 
     xlab="Promedio de habitaciones", 
     ylab="Promedio de metros cuadrados")

plot(df2$muhabitaciones, df2$mumetros, 
     las=1, 
     xlab="Promedio de habitaciones", 
     ylab="Promedio de metros cuadrados", 
     pch=16)

plot(df2$muhabitaciones, df2$mumetros, 
     las=1, 
     xlab="Promedio de habitaciones", 
     ylab="Promedio de metros cuadrados", 
     pch=16, 
     main="Metros cuadrados y habitaciones en comunas")

plot(df2$muhabitaciones, df2$mumetros, 
     las=1, 
     xlab="Promedio de habitaciones",
     ylab="Promedio de metros cuadrados", 
     pch=16, 
     main="Metros cuadrados y habitaciones en comunas", 
     col="palevioletred4")


plot(df2$muhabitaciones, df2$mumetros, 
     las=1, 
     xlab="Promedio de habitaciones", 
     ylab="Promedio de metros cuadrados", 
     pch=16, 
     main="Metros cuadrados y habitaciones en comunas", 
     col="palevioletred4")

abline(lm(df2$mumetros~df2$muhabitaciones), 
       col="maroon1", 
       lwd=2)

text(5.8, 370, "Correlación de 0.56")
