##################################################
#Caso Poisson con prioris gamma - Modelo jerárquico

años<-1936:1951
y<-c(7,9,8,7,7,6,6,5,5,7,9,10,8,8,8,7) #Datos y_i
(n<-length(y))

T <- 20000

beta <- rep(NA, T)  #Cadena para beta
lambda <- matrix(0, nrow=T, ncol=n) #n cadenas, una por cada lambda_i

#### Fijamos alpha, delta y gamma (son fijos)

alpha <- 1
delta <- 1
gamma <- 1

###Valores iniciales:

beta[1] <- 1
lambda[1,] <- rep(1, n)


#Muestreamos a partir de las distribuciones condicionales completas!

for(i in 2:T) #Partimos desde 2, pues la posición 1 ya se ha llenado
  {
    for(j in 1:n) #Se debe usar doble índice porque tenemos n lambdas
    { 
      #p(lambda_i|resto) ~ Gamma(alpha+y_i, beta+1)
      lambda[i,j] <- rgamma(1, alpha + y[j], beta[i - 1] + 1) 
      
      #p(beta|resto) ~ Gamma(n*alpha+gamma, delta+sum(lambda_i))
      beta[i] <- rgamma(1, n*alpha + gamma, delta + sum(lambda[i,])) 
    }
}


head(round(lambda,2), 4) #Matriz de las cadenas para cada lambda_i
head(round(beta,2), 4)   #Vector de la cadena para beta

round(apply(lambda, 2, mean), 2) #Promedios de lambda_i

#Si quemaramos hasta 4000:
round(apply(lambda[4000:T,], 2, mean), 2)

#No se observan muchas diferencias. Al final, el tema de la quema es para
#"asegurarse" de que estamos ya en una zona probable para el parámetro.

plot(años,round(apply(lambda[4000:T,], 2, mean), 2), type="l", 
     xlab="lambda", 
     ylab="Media de la cadena", 
     main="Gibbs Sampler modelo jerárquico Poisson Gamma",
     axes=FALSE)

axis(1, at=años, las=2)
axis(2, at=seq(min(round(apply(lambda[4000:T,], 2, mean), 2)), max(round(apply(lambda[4000:T,], 2, mean), 2)), by=0.5), las=2)

#La segunda guerra mundial fue en el año 1939, notar que desde ese año hay una
#baja en la cantidad de matrimonios por mil personas

cbind(y, round(apply(lambda[4000:T,], 2, mean), 2), años)

plot(1:T, lambda[,1], type="l", xlab="Iteraciones", ylab="lambda año 1936")

acf(lambda[,1])

#Verificar que no vaya a existir autocorrelación con lag>1

##################################
#Caso multinomial - Datos faltantes

T <- 1000

theta <- rep(NA, T)

theta[1] <- 0.1 #Valor inicial theta

z <- rep(NA, T)

z[1] <- 1  #Valor inicial z

for(i in 2:T)
  {
    #p(z|resto) ~ bin(x1, (1/2)/(1/2+theta/4))
    z[i] <- rbinom(1, 125, ((1/2)/((1/2) + (theta[i-1]/4)))) 
    
    #p(theta|resto) ~ beta(160-z, 39)
    theta[i] <- rbeta(1, 160 - z[i], 39)
}

mean(z) #zeta: n° de animales que quedó en la primera posición
mean(z[200:T]) #Quema

mean(theta)  #theta: parámetro de la multinomial
mean(theta[200:T]) #Quema

