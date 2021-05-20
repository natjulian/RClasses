install.packages("dplyr") #Instala el paquete

library(dplyr) #Carga el paquete

data(iris)

head(iris)

length(unique(iris$Species))

unique(iris$Species)


summary(iris$Petal.Length[iris$Species=="setosa"])
summary(iris$Petal.Length[iris$Species=="versicolor"])
summary(iris$Petal.Length[iris$Species=="virginica"])


iris%>%   #Partimos con la dataframe
  group_by(Species)%>% #Agrupamos por especie
    summarise(Min=min(Petal.Length), #Calculamos el minimo de petal.length por grupo
            firsq=quantile(Petal.Length, 0.25), #Calculamos el primer cuartil de petal.length por grupo
            Median=median(Petal.Length), #Calculamos la mediana de petal.length por grupo
            Mean=mean(Petal.Length), #Calculamos la media de petal.length por grupo
            thirdq=quantile(Petal.Length, 0.75), #Calculamos el tercer cuartil de petal.length
            Max=max(Petal.Length)) #Calculamos el maximo de petal.length por grupo

(tabla<-iris%>%   
  group_by(Species)%>% 
  summarise(Min=min(Petal.Length),
            firsq=quantile(Petal.Length, 0.25), 
            Median=median(Petal.Length),
            Mean=mean(Petal.Length), 
            thirdq=quantile(Petal.Length, 0.75), 
            Max=max(Petal.Length)))


tabla[2,-1] - tabla[3, -1 ] #diferencia versicolor - virginica

#Distancias:
sqrt(sum((tabla[2,-1] - tabla[3, -1 ])**2)) #versicolor - virginica
sqrt(sum((tabla[1,-1] - tabla[2, -1 ])**2)) #setosa - versicolor 
sqrt(sum((tabla[1,-1] - tabla[3, -1 ])**2)) #setosa - virginica


iris%>%
  group_by(Species)%>%
  slice(1)


iris%>%
  group_by(Species)%>%
  slice(length(Sepal.Length))

iris%>%
  group_by(Species)%>%
  slice(1:5)


#Forma 1 selecciona registros versicolor o setosa

filtro<-iris[which(iris$Species=="versicolor"|iris$Species=="setosa"),]

#Forma 2 filtra registros iguales a versicolor o setosa

filtro<-subset(iris, Species=="versicolor"|Species=="setosa")

#Forma 3 quita los registros virginica

filtro<-iris[-which(iris$Species=="virginica"), ]

#Forma 4 filtra los casos distintos a virginica

filtro<-subset(iris, Species!="virginica")


iris%>%
  filter(Species=="versicolor"|Species=="setosa")

#O también:

iris %>%
  filter(Species!="virginica")


iris %>%
  filter(Species!="virginica", Sepal.Length>5)


#Forma 1: Selecciona el número de las columnas de interés

iris[,c(2,5)]

#Forma 2: Selecciona el nombre de las columnas de interés

iris[, c("Sepal.Width", "Species")]

#Forma 3: Quita las columnas que no son de interés

iris[, -c(1, 3, 4)]

#Forma 4: con dplyr, selecciona las columnas de interés 


#Forma 1 usual:

iris%>%
  select(Sepal.Width, Species)

iris$radio<-2*iris$Petal.Length*iris$Petal.Width/(iris$Petal.Length+iris$Petal.Width)

head(iris)

#Forma 2 con dplyr:

iris%>%
  mutate(radio=2*Petal.Length*Petal.Width/(Petal.Length+Petal.Width))


#Forma 1: 

iris[order(iris$Petal.Length, decreasing=TRUE), ]

#Forma 2 con dplyr:

iris%>%
  arrange(desc(Petal.Length))


#Forma 1: 

iris[order(iris$Petal.Length), ]

#Forma 2 con dplyr:

iris%>%
  arrange(Petal.Length)


#%>% llamado pipe indica la tubería:


iris%>%
  filter(Species!="versicolor")%>%
  mutate(radio=2*Petal.Length*Petal.Width/(Petal.Length+Petal.Width)) %>%
  group_by(Species)%>%
  summarise(Min=min(Petal.Length),
              firsq=quantile(Petal.Length, 0.25), 
              Median=median(Petal.Length),
              Mean=mean(Petal.Length), 
              thirdq=quantile(Petal.Length, 0.75), 
              Max=max(Petal.Length), 
              Avgradio=mean(radio))%>%
  arrange(desc(Max))
