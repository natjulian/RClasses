install.packages("rio") #Instala el paquete o librería rio
library(rio) #Carga o llama el paquete rio

?import #Usaremos la función import del paquete rio

#Los datos se guardaran en un objeto llamado datos

datos<-import(file.choose()) 

#length(.packages(all.available = TRUE)) cantidad de paquetes instalados

#file.choose abre una ventana para seleccionar la ubicación del archivo

View(datos) #Vemos una vista previa de los datos

#Análisis de la estructura de los datos:

dim(datos) #Dimensiones

names(datos) #Nombre de las variables

str(datos) #Formato de las variables

#¿Cuántos platos hay? 
length(unique(datos$Item)) #Cuenta los nombres únicos que hay

#¿Cuántas categorías hay?
length(unique(datos$Category)) #9 categorías

## ¿Cómo se distribuyen los platos en las categorías?

table(datos$Category)

df<-data.frame(table(datos$Category)) #Guarda los datos de la tabla de manera vectorizada
       
df[which.max(df$Freq),] #¿Cuál categoría tiene más productos?

df[which.min(df$Freq),] #¿Cuál categoría tiene menos productos?

df[order(df$Freq, decreasing=TRUE),] #Muestra las categorías ordenadas por cantidad de productos

#################################### HASTA AQUÍ AVANZAMOS EN CLASES


#Tamaño de la porción

datos[which(datos$Category=="Breakfast"), 'Serving Size'] #Tamaño de la porcion codificada

substr(datos[which(datos$Category=="Breakfast"), 'Serving Size'], start = nchar(datos[which(datos$Category=="Breakfast"), 'Serving Size'])-5, stop=nchar(datos[which(datos$Category=="Breakfast"), 'Serving Size'])-3)

sizebreakfast<-as.numeric(substr(datos[which(datos$Category=="Breakfast"), 'Serving Size'], start = nchar(datos[which(datos$Category=="Breakfast"), 'Serving Size'])-5, stop=nchar(datos[which(datos$Category=="Breakfast"), 'Serving Size'])-3))

range(sizebreakfast) #Rango de valores en gramos

#Pero quiero realizar el análisis conociendo el nombre del producto...

productosbreakfast<-data.frame(datos$Item[which(datos$Category=="Breakfast")], sizebreakfast)

head(productosbreakfast)

names(productosbreakfast)<-c("Producto", "Porcion")

head(productosbreakfast)

#¿Cuál es el producto en el desayuno con menor porcion en gramos?

productosbreakfast[which.min(productosbreakfast$Porcion),] #Sausage McMuffin con 111 gramos

#¿Cuál es el producto en el desayuno con mayor porcion en gramos?

productosbreakfast[which.max(productosbreakfast$Porcion),] #Big Breakfast with Hotcakes and Egg Whites con 437 gramos

#¿Cuál es la mediana del tamaño de la porcion de estos productos?

summary(productosbreakfast$Porcion)

#¿Qué productos están bajo la mediana en tamaño de porcion en gramos?

productosbreakfast$Producto[which(productosbreakfast$Porcion<median(productosbreakfast$Porcion))]
