####### Tema 1.2 Asociación entre variables categóricas


#Carga la base de datos con Import Dataset

library(readr)
depress <- read_csv("Tema 1.2 analisis de asociacion variables categoricas/depress.csv")
View(depress)


#Algunas inferencias iniciales:

nrow(depress)   #1429 registros 
ncol(depress)   #23 columnas

colnames(depress)

#La primera columna corresponde al ID, verificamos que es único

table(depress$Survey_id)  #Tabla completa

table(table(depress$Survey_id))  #Efectivamente se repite una vez
                                 #cada ID


### Variable target (variable de interés: depressed)

table(depress$depressed)   #Conteos de casos 

table(depress$depressed)/nrow(depress)   #Porcentaje

#Un 16,6% de la muestra presentó depresión 


### Variable sexo 

table(depress$sex)

table(depress$sex)/nrow(depress)

#La muestra está compuesta en un 91.8% por mujeres


### Variable estado marital

table(depress$Married)

table(depress$Married)/nrow(depress)

# El 77.2% de la muestra se encontraba casado

################ Test de asociación Xi cuadrado


table(depress$sex,depress$depressed)

sexo<-ifelse(depress$sex==1,"Femenino","Masculino")
depresion<-ifelse(depress$depressed==1,"Deprimido","No deprimido")

table(sexo,depresion)   #tabla de contingencia


addmargins(table(sexo,depresion))  #tabla de contingencia más totales

prop.table(table(sexo,depresion),margin=1)  #tabla de proporciones

barplot(prop.table(table(sexo,depresion),margin=1), beside=TRUE)



################ Test de asociación Xi cuadrado

contingency<-table(sexo,depresion)   #tabla de contingencia


chisq.test(contingency)

chisq.test(contingency)$p.value

chisq.test(contingency)$p.value<0.05


################ Estado marital y tener depresión

table(depress$Married,depress$depressed)

depresion<-ifelse(depress$depressed==1,"Deprimido","No deprimido")
marital<-ifelse(depress$Married==1,"Casado","No casado")

table(marital,depresion)


prop.table(table(marital,depresion),margin=1) 


barplot(prop.table(table(marital,depresion),margin=1), beside=TRUE)


################ Test de asociación Xi cuadrado

contingency2<-table(marital,depresion)   #tabla de contingencia


chisq.test(contingency2)

chisq.test(contingency2)$p.value

chisq.test(contingency2)$p.value<0.05



############### Test exacto de Fisher


fisher.test(contingency2)$p.value

#Se utiliza con muestras muy pequeñas



