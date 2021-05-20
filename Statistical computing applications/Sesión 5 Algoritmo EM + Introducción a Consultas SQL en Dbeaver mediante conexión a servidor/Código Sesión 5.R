#install.packages("saemix")
library(saemix)
data(cow.saemix)

saemixcow<-saemixData(cow.saemix,
                        header=TRUE,
                        name.group=c("cow"),
                        name.predictors=c("time"),
                        name.response=c("weight"), 
                        name.covariates=c("birthyear","twin","birthrank"),  
                        units=list(x="days",y="kg",covariates=c("yr","-","-")))

growthcow<-function(psi,id,xidep) {
  
  x<-xidep[,1]
  A<-psi[id,1]
  b<-psi[id,2]
  k<-psi[id,3]
  
  f<-A*(1-b*exp(-k*x))
  return(f)
}


saemix.model<-saemixModel(model=growthcow,description="Modelo de crecimiento Exponencial", #Descripción del modelo
                          psi0=matrix(c(700,0.9,0.02,0,0,0),ncol=3,byrow=TRUE, 
                                      dimnames=list(NULL,c("A","B","k"))), #Valores iniciales para los efectos fijos A, B y k 
                          transform.par=c(1,1,1), #Distribución para cada parámetro, 1=lognormal, 0=normal, 2=probit, 3=logit
                          fixed.estim=c(1,1,1), #1 Indica que sean estimados y no iguales al valor inicial
                          error.model="constant") #puede ser proportional, combined o exponential

#Otras opciones que pueden cambiarse:
# - covariate model
# - covariance model
# - omega.init

#Ver más: 

?saemixModel

saemix.options<-list(nbiter.saemix=c(200,100), #Número de iteraciones en cada paso
                     save=FALSE,save.graphs=FALSE)

#Más opciones:

?saemixControl

saemix.fit<-saemix(saemix.model,saemixcow,saemix.options)


#install.packages("extrafont")
library(extrafont)

font_import()
loadfonts()     #Importa tipos de letra
fonts()    #Muestra todos los tipos de letra

par(family="Bookman Old Style", bg=NA)

plot(saemix.fit,plot.type="data", col="green4", main="Cow growth saemixData")

#Convergencia de los parámetros A, B y k, también de a la componente constante del error

plot(saemix.fit, plot.type = "convergence", smooth = TRUE) #Convergencia de parámetros

plot(saemix.fit,plot.type="observations.vs.predictions", level=0) #Ajuste del modelo

plot(saemix.fit,plot.type="individual.fit",ilist=c(1:6),ask=TRUE) #6 individuos

plot(saemix.fit,plot.type="vpc") #Incluye intervalos de confianza


#SQL en R

#install.packages("RPostgres")
library(RPostgres)

conexion <- dbConnect(
  Postgres(),
  user = "student",
  password = "tx5mvyRQqD",
  dbname = "nycflights13",
  host = "db-edu.pacha.dev"
)


library(dplyr)

tbl(conexion, "airlines") #Extrae una previsualización de la tabla


#Si la queremos extraer completa:

tbl(conexion, "flights") %>%
  collect()

tbl(conexion, "weather") %>%
  filter(humid>60) %>%
  count()
  
tbl(conexion, "weather") %>%
  filter(humid>60) %>%
  count() %>%
  show_query()

#Ejemplo con cruces de tablas

tbl(conexion, "flights") %>%
  count() #336776 filas

tbl(conexion, "planes") %>%
  count() #3322 filas

intersect(colnames(tbl(conexion, "flights")), colnames(tbl(conexion, "planes")))

tbl(conexion, "flights") %>% 
  right_join(.,tbl(conexion, "planes") , by="tailnum")%>%
  show_query()


dbDisconnect(conexion)

