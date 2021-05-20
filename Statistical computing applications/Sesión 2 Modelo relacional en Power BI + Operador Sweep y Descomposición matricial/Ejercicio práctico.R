####################################################################################
####################################################################################
#############################################################Ejercicio práctico aquí
####################################################################################
####################################################################################

load(file.choose())

X<-cbind(acousticness,danceability,energy,key,liveness,loudness,
         speechiness,instrumentalness)
head(X,2)


# Aquí pueden hacer la matriz de correlación, algún gráfico, etc.

library("PerformanceAnalytics")
chart.Correlation(cbind(X,happyness),hist=T,pch="+")

cor(X,happyness)

y<-happyness

A<-crossprod(cbind(1,X))  #Consideramos intercepto

b<-crossprod(cbind(1,X),y)


det(A)                   


# ¿Como evaluar multicolinealidad? Ver aquí:
# https://onlinelibrary.wiley.com/doi/10.1111/j.1600-0587.2012.07348.x

kappa(X, exact=TRUE) #Numero Condicionamiento mayor a 30 alta multicolinealidad

det(cor(X)) #Cerca de cero existe multicolinealidad alta

model1 <- lm(acousticness~danceability+energy+key+liveness+loudness+
               speechiness+instrumentalness)

car::vif(model1) #Valor 1 indica no correlacion con otras variables, valores mayores a 1
#indican grado de correlacion con otras variables


#Descomposición espectral

eig <- eigen(A)
D <- diag(sqrt(eig$values))   # Valores singulares (sqrt(valores propio))
V <- eig$vec                  
(beta.de<-solve(tcrossprod(V%*%D))%*%b)

# V matriz unitaria
# D diagonal con valores propios

benchmark(Espectral<-{
  eig <- eigen(A)
  D <- diag(sqrt(eig$values))   
  V <- eig$vec                  
  (beta.de<-solve(tcrossprod(V%*%D))%*%b)},columns=c("elapsed", "user.self"))

#Cholesky

R<-t(chol(A))                     
w<-forwardsolve(R,b)     
(beta_chol<-backsolve(t(R),w))     

benchmark(cholesky<-{R<-t(chol(A))                     
w<-forwardsolve(R,b)     
(beta_chol<-backsolve(t(R),w))},columns=c("elapsed", "user.self"))

identical(beta.de,beta_chol)
all.equal(beta.de,beta_chol)


lm(happyness~X)$coef

