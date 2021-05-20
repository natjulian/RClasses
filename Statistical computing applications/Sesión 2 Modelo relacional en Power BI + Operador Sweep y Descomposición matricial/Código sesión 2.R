#Defino el directorio a trabajar:

getwd() #Indica directorio actual

setwd("C:/Users/HP/Desktop/Trabajo/2020-2/Introducción a la Computación/Ayudantía 1")


#Obtiene un vector con el nombre de los archivos csv en el directorio:

nombres<- list.files(path = getwd(),
                     pattern = "\\.csv$", 
                     full.names = FALSE)

library(readr)

#Carga todos los archivos csv en una lista:

Datas<-lapply(nombres,"read_csv")  

#Añade nombre respectivo a cada data:

names(Datas)<-substr(nombres,1,nchar(nombres)-4) 

class(Datas)


#Cruce de las tablas:

dim(Datas$employee_survey)
dim(Datas$general)
dim(Datas$manager_survey)


intersect(names(Datas$general), names(Datas$employee_survey))
intersect(names(Datas$general), names(Datas$manager_survey))
intersect(names(Datas$employee_survey), names(Datas$manager_survey))


library(dplyr)

#merge

cruce<-full_join(Datas$general,Datas$employee_survey,by="EmployeeID") %>% 
  full_join(., Datas$manager_survey, by="EmployeeID")

dim(cruce)

names(cruce)

View(cruce)

#Matriz de diseño

#Indicador de desempeño variable dummy:

unique(cruce$PerformanceRating)

perf<-ifelse(cruce$PerformanceRating=="3", 1, 0)

#Sexo variable dummy:

unique(cruce$Gender)

gen<-ifelse(cruce$Gender=="Female", 1, 0)

all(perf==gen)  #no deben ser iguales ambas variables dummy

X<-cbind(1, perf, gen, cruce$YearsAtCompany, cruce$Age)

colnames(X)<-c("Intercepto", "Performance", "Gender", "Years at Company", "Age")

print(X)


#####Resolveremos Ax=b, con A=XtX y b=XtY


#Descomposición Cholesky

#Analizamos la matrix XtX y su estabilidad numerica:
(A<-crossprod(X,X)) 

det(A)

Y<-cruce$PercentSalaryHike

b<-crossprod(X,Y)  #XtY

R<-t(chol(A)) #Matriz inferior G
w<-forwardsolve(R,b) #Resuelve Rx=b con R triangular inferior, sustitucion hacia adelante
(beta_chol<-backsolve(t(R),w)) #Resuelve triangular superior, sustitucion hacia atras


library(devtools)
#devtools::install_github("eddelbuettel/rbenchmark")
library(rbenchmark)

benchmark(choleskyfast<-{R<-t(chol(A))
w<-forwardsolve(R,b)
beta_chol<-backsolve(t(R),w)},columns=c("elapsed", "user.self"))


#Y si solo usaramos solve?

benchmark(choleskylow<-{R<-t(chol(A))
w<-solve(R,b)
beta_chol<-solve(t(R),w)},columns=c("elapsed", "user.self"))


#Descomposicion QR

library(matlib)

Q<-QR(A)$Q #Matriz Q ortonormal
R<-QR(A)$R #Matriz R triangular superior no singular
c<-crossprod(Q,b) #Qtb=c
(beta_qr=backsolve(R,c)) #Resolvemos la triangular superior

#QR no es unica, en ocasiones se puede obtener Q con distinto signo
# esto significa reflexion de los vectores

benchmark(QRfast<-{Q<-QR(A)$Q 
R<-QR(A)$R
c<-crossprod(Q,b) 
beta_qr=backsolve(R,c)},columns=c("elapsed", "user.self"))


benchmark(QRlow<-{Q<-QR(A)$Q 
R<-QR(A)$R
c<-t(Q)%*%b 
beta_qr=solve(R,c)},columns=c("elapsed", "user.self"))


library(matrixcalc)

#Descomposicion LU

A_LU<-lu.decomposition(A) 
L<-A_LU$L #L triangular inferior
U<-A_LU$U #U triangular superior

#A veces existe una P y descomposicion PALU

t<-forwardsolve(L,b)
(beta_lu=backsolve(U,t))


benchmark(LUfast<-{A_LU<-lu.decomposition(A) 
L<-A_LU$L 
U<-A_LU$U 
t<-forwardsolve(L,b)
beta_lu=backsolve(U,t)},columns=c("elapsed", "user.self"))

benchmark(LUlow<-{A_LU<-lu.decomposition(A) 
L<-A_LU$L 
U<-A_LU$U 
t<-solve(L,b)
beta_lu=solve(U,t)},columns=c("elapsed", "user.self"))

#Descomposicion Espectral

eig <- eigen(A)
D <- diag(sqrt(eig$values)) #Matriz D (diagonal de raiz de autovalores)
V <- eig$vec #Matriz de autovectores
(beta.de<-solve(tcrossprod(V%*%D))%*%b)  #VD(VD)t

benchmark(espectfast<-{eig <- eigen(A)
D <- diag(sqrt(eig$values)) 
V <- eig$vec 
beta.de<-solve(tcrossprod(V%*%D))%*%b},columns=c("elapsed", "user.self"))

benchmark(espectlow<-{eig <- eigen(A)
D <- diag(sqrt(eig$values))
V <- eig$vec 
beta.de<-solve(V%*%D%*%t(V%*%D))%*%b},columns=c("elapsed", "user.self"))


(beta<-lm(Y~X[,-1])$coef)

#Considerando una performance de nivel 3 respecto a 4 se disminuye un
# 7.8% el incremento porcentual del sueldo de un año respecto otro

# Mujeres ven un incremento de 0.21% del sueldo respecto a hombres

#Edad y Años en la empresa no sugieren incremento


#Operador SWEEP

Z<-crossprod(cbind(X,Y))

p<-dim(Z)[1]-1  #Se sweepea la cantidad de betas a estimar

library(ISR3)

for(i in 1:p){
  Z<-SWP(Z,i)
}

(SCE<-Z[p+1,p+1])

n<-length(Y)

(CME<-SCE/(n-p))


(Varbeta<-Z[1:p,1:p]*-CME)

(hatbeta<-as.numeric(round(Z[1:p,p+1], 5)))

(tbeta0<-hatbeta[1]/sqrt(Varbeta[1,1])) #Intercepto
2*pt(abs(tbeta0),n-p,lower=FALSE)

(tbeta1<-hatbeta[2]/sqrt(Varbeta[2,2])) #Performance
2*pt(abs(tbeta1),n-p,lower=FALSE)

(tbeta2<-hatbeta[3]/sqrt(Varbeta[3,3])) #Gender
2*pt(abs(tbeta2),n-p,lower=FALSE)

(tbeta3<-hatbeta[4]/sqrt(Varbeta[4,4]))  #Years at company
2*pt(abs(tbeta3),n-p,lower=FALSE)

(tbeta4<-hatbeta[5]/sqrt(Varbeta[5,5])) #Age
2*pt(abs(tbeta4),n-p,lower=FALSE)


summary(lm(Y~X[,-1]))

anova(lm(Y~X[,-1]))



####################################################################################
####################################################################################
#############################################################Ejercicio práctico aquí
####################################################################################
####################################################################################


