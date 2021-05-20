library(ISR3)
library(dplyr)


regresion <- function(x,y){ #x: matriz de predictores y:variable respuesta
  W <- cbind(1,x,y) 
  Z <- crossprod(W)
  p <- dim(Z)[1]-1 
  gl.e <- length(y)-p 
  gl.m <- p-1
  Z <- SWP(Z,1:p) 
  SCT <- crossprod(y-mean(y))
  SCE <- Z[p+1, p+1]
  SCM <- SCT-SCE
  CME <- SCE/gl.e
  CMM <- SCM/gl.m
  R2 <- as.numeric(SCM/SCT)
  beta<-data.frame(Z[1:p,p+1], sqrt(diag(-CME*(Z[1:p,1:p]))))
  names(beta)<-c("Estimate", "std.error")
  test<-beta %>% 
          mutate(t=Estimate/std.error, pvalue=2*(1-pt(abs(t),gl.e)))
  coeficientes <- round(test,4)
  row.names(coeficientes)<-c("Intercept", colnames(x))
  list(R.squared = R2, Coef = coeficientes)
  }
  
load(file.choose())

X<-cbind(acousticness,danceability,energy,key,liveness,loudness,
         speechiness,instrumentalness)

y<-happyness

regresion(X, happyness)$Coef
summary(lm(happyness~X))$coefficients

regresion(X, happyness)$R.squared
summary(lm(happyness~X))$r.squared


#Ejercicio 1

PCA<-prcomp(X,scale=TRUE)  #Es necesario estandarizar pues escalas de las variables interfieren

names(PCA)

?prcomp

summary(PCA)

library(factoextra)
get_eigenvalue(PCA)

fviz_eig(PCA,main="Varianza explicada por componente",
         xlab="Componente",
         ylab="Porcentaje de varianza explicada",
         barcolor="lightskyblue2",
         linecolor = "navyblue",
         barfill = "mediumslateblue",ylim=c(0,100),addlabels=TRUE)

#Gráfico tiene que ser de 0 a 100, es un gráfico de porcentaje

fviz_pca_biplot(PCA)

fviz_pca_biplot(PCA, axes=c(7,8))

fviz_pca_var(PCA, col.var = "deepskyblue2") #Primeras dos componentes

fviz_pca_var(PCA, col.var = "deepskyblue2", axes=c(3,4))


#Criterios para elegir n° de componentes principales:

# Lo que se busca: ¿Reducir dimensionalidad? ¿Ortogonalidad?
# eigenvalue>1, que explique mas de una unidad de varianza
# grafico de sedimentacion, ver cuando exista un cambio en la pendiente
# varianza explicada minima (depende del contexto)


Comp <- get_pca_var(PCA)

library(corrplot)

# Influencia de cada variable en cada componente:
corrplot(Comp$contrib, is.corr = FALSE,tl.col = "darkcyan")

get_eigenvalue(PCA) 

#Las nuevas variables ortogonales se pueden obtener de dos formas:

head(scale(X)%*%PCA$rotation,4)

head(predict(PCA,X),8)

round(cor(scale(X)%*%PCA$rotation),1)

model<-lm(happyness~scale(X)%*%PCA$rotation)

summary(model)

#Naturalmente al menos las primeras componentes dan significativas
#Pero esto puede variar dependiendo de las variables que tienen
#mayor contribucion en la componente

#Ejercicio 2

#Obtiene un vector con el nombre de los archivos csv en el directorio


setwd("C:/Users/HP/Desktop/Trabajo/2020-2/Introducción a la Computación/Ayudantia 3 PCA y Newton Raphson")


nombres<- list.files(path = getwd(),
                     pattern = "\\.csv$", 
                     full.names = FALSE)

nombres

library(readr)

Datas<-lapply(nombres,"read_csv")  #Carga todos los archivos csv 

names(Datas)<-substr(nombres,1,nchar(nombres)-4)

names(Datas)


dim(Datas$products)
dim(Datas$order_products_train)
dim(Datas$orders)


intersect(names(Datas$orders), names(Datas$order_products_train))

intersect(c(names(Datas$orders), names(Datas$order_products_train)), names(Datas$products))

#Hay dos llaves, order_id y product_id

dim(Datas$orders) #Numero de orden (pedido) y datos del cliente
dim(Datas$order_products_train) #Info del orden de los productos por pedido
dim(Datas$products) #Info de todos los productos

#Cruce

library(dplyr)

#Necesitamos primero informacion de las dos tablas para poder obtener reordered por pedido
# por lo tanto primero realizamos un innerjoin

#Luego para añadir la informacion del producto, realizamos left join, necesitamos mantener
#la informacion del primer cruce solamente, el resto es complementario

table(table(Datas$orders["order_id"]))

cruce<-inner_join(Datas$orders, Datas$order_products_train, by="order_id") %>%
        left_join(., Datas$products, by="product_id")

dim(cruce)

View(cruce)

#dimension de un cruce con inner_join a lo mas tendra nfilas=min(nrow(A,B))
#Inner join(A,B) interseccion, mantiene la información completa de tablas A y B
# no quedan registros con NA, se preservan registros que están en A y B

#left join(A,B) complementa la información de A con B, pudiendo quedar observaciones
# de A que no se linkearon con B (NA's). Los registros de A se buscan en B

#right join(A, B) complementa la información de B con A, pudiendo quedar observaciones
# de B que no se linkearon con A (NA's), los registros de B se buscan en A

#Note que right join(B, A)= left join(A, B)

#full join union, mantiene la informacion de todas las filas de A y B, multiples NA's

dim(cruce)

View(cruce)

#Calcular por cliente y pedido, la proporcion:

proporcion<-cruce%>%
            group_by(user_id,order_id)%>%
            mutate(prop=sum(reordered)/max(add_to_cart_order))%>%
            slice(1)%>% #slice(1) corta el cubito en el primer registro de userid y orderid
            select(order_id, user_id, prop)

View(proporcion)

#group_by me agrupa de acuerdo a una o mas variables
#mutate crea nuevas variables a partir de variables de la data, tambien pueden redefinirse
#otras variables

#slice me corta el cubito, esto sirve cuando al cruzar bases de datos hay réplicas
#select me selecciona ciertas variables de la data, no todas

#Otras funciones utiles:

#summarise() util posterior de realizar un groupby, me crea estadisticas por grupo
#Ejemplo:

cruce%>% 
  group_by(product_id)%>%
  summarise(n=n())

cruce%>% 
  group_by(product_id)%>%
  summarise(media=mean(days_since_prior_order)) #Promedio por grupo


proporcion<-proporcion%>%
  filter(prop>0, prop<1)

summary(proporcion$prop)

x<-as.vector(proporcion$prop)

# Newton Raphson

beta.NR<-function(theta0, x, error, i=0){
  alpha.old<-theta0[1]
  beta.old<-theta0[2]
  n<-length(x)
  sumlogx<-sum(log(x))
  sumlog1x<-sum(log(1-x))
  
  grad.l<-c(n*digamma(alpha.old+beta.old)-n*digamma(alpha.old)+sumlogx,
            n*digamma(alpha.old+beta.old)-n*digamma(beta.old)+sumlog1x)
  
  hess.l<-n*matrix(c(trigamma(alpha.old+beta.old)-trigamma(alpha.old),
                     trigamma(alpha.old+beta.old),trigamma(alpha.old+beta.old),
                     trigamma(alpha.old+beta.old)-trigamma(beta.old)),2,2)
  
  theta.new<-theta0-solve(hess.l)%*%grad.l
  
  Rerror<-sqrt(sum((theta.new-theta0)^{2}))
  
  if(Rerror>error){
    beta.NR(theta.new,x, error, i=i+1)
  }
  
  else{
    list(alphaybeta=theta.new,"iteraciones"=i,hess.l=hess.l,
         estandarerror=c(sqrt(-solve(hess.l)[1,1]),sqrt(-solve(hess.l))[2,2]))
  }
}


beta.NR(c(0.1,0.1),x,error=0.001)

library(Rfast)

beta.mle(x)

beta.NR(c(2.155885,1.7241),x,error=0.01)

beta.NR(c(7,0.2),x,error=0.01)

?tmle #Para t student

