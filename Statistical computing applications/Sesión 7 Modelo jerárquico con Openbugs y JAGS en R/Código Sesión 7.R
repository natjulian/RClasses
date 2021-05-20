#install.packages("R2OpenBUGS")

library(R2OpenBUGS)
data("schools")

J <- nrow (schools)   #Se deben recorrer todas las filas
y <- schools$estimate #Corresponden a Y_j
sigma.y <- schools$sd #Desviaciones estándar son

data <- list(J=J, y=y, sigma.y=sigma.y) #Se guardan los datos

#Los parámetros que tenemos son:
# - theta_j (para cada y_j) ~ N(mu.theta, tau_theta)
# - mu.theta (para theta_j) ~ N(0,1)
# - sigma_theta (para tau_theta) ~ U(0,1000)

inits <- function() #Valores iniciales
  list (theta=rnorm(J,0,100), mu.theta=rnorm(1,0,100),
        sigma.theta=runif(1,0,1000))

#Model.file es un archivo de tipo txt con el modelo a utilizar!

#Tendremos una simulacion MCMC para theta, mu.theta y sigma.theta (parámetros)

schools.sim <- bugs(data, inits, model.file = "C:/Users/HP/Desktop/Clases UC/2020-2/Introducción a la Computación/Ayudantía 7/school.txt",
                      parameters = c("theta", "mu.theta", "sigma.theta"),
                      n.chains = 3, #3 cadenas por parámetro
                      n.iter = 50) #50 iteraciones (largo de la cadena

print(schools.sim)

#Deviance

#Cálculo  https://en.wikipedia.org/wiki/Deviance_information_criterion
#Básicamente es una medida de calidad del ajuste y se
#basa en el cálculo de la log-verosimilitud utilizando la posterior de cada parámetro. 
#Penaliza por modelos más complejos también, permite comparar


plot(schools.sim)

#install.packages("lattice")
library(lattice)

#install.packages("coda")
library(coda)

openbcoda<-as.mcmc.list(schools.sim) #Para bugs es necesario definirla como
                                     #as.mcmc.list

densityplot(openbcoda) #Muestra las densidades para las 3 cadenas

densityplot(openbcoda[1:3][, 3]) #Sigma.theta para las 3 cadenas

xyplot(openbcoda[1:3][, 4:6]) #primeros 3 thetas

xyplot(openbcoda[1:3][,2]) #mu.theta

#install.packages("mcmcplots", dependencies = TRUE, repos = "https://cloud.r-project.org")

library(mcmcplots)

mcmcplot(openbcoda) 


##Vemos en los gráficos un problema de autocorrelación y convergencia, 
#¿Por qué?

#- Tomamos una cadena muy corta (sólo 1000 iteraciones)
#- Nos falta quemar también


schools.simbig <- bugs(data, inits, model.file = "C:/Users/HP/Desktop/Clases UC/2020-2/Introducción a la Computación/Ayudantía 7/school.txt",
                    parameters = c("theta", "mu.theta", "sigma.theta"),
                    n.chains = 3, 
                    n.iter = 10000, 
                    n.burnin = 3000) #Quemamos las primeras 3000 obs

print(schools.simbig)

openbcodabig<-as.mcmc.list(schools.simbig)

#Miramos ahora...

mcmcplot(openbcodabig)

#Aún nos quedan algunas autocorrelaciones de lag>1

schools.simbigsaltos <- bugs(data, inits, model.file = "C:/Users/HP/Desktop/Clases UC/2020-2/Introducción a la Computación/Ayudantía 7/school.txt",
                       parameters = c("theta", "mu.theta", "sigma.theta"),
                       n.chains = 3, 
                       n.iter = 10000, 
                       n.burnin = 3000, 
                       n.thin = 20) #Saltos cada 20 obs

openbcodabigsaltos<-as.mcmc.list(schools.simbigsaltos)

mcmcplot(openbcodabigsaltos)

traplot(openbcodabigsaltos, parms = c("mu.theta", "sigma.theta"))

caterplot(openbcodabigsaltos, parms = paste("theta[", 1:8, "]", sep="")) #Intervalos


########## JAGS

#El modelo se define de manera diferente a bugs:

model <- textConnection("
model {
for (j in 1:J)
{
y[j] ~ dnorm (theta[j], tau.y[j])
theta[j] ~ dnorm (mu.theta, tau.theta)
tau.y[j] <- pow(sigma.y[j], -2)
}
mu.theta ~ dnorm (0.0, 1.0E-6)
tau.theta <- pow(sigma.theta, -2)
sigma.theta ~ dunif (0, 1000)
}
")

#install.packages("rjags")
library(rjags)

schools.sim <- jags.model(file=model, data, inits, n.chains=3)

# Quema
update(schools.sim, 3000)

# MCMC muestra

post.samples <- coda.samples(schools.sim, variable.names= c("theta", "mu.theta", "sigma.theta"), 
                             n.iter=10000, 
                             thin=20)

summary(post.samples)

class(post.samples) #Es una lista

plot(post.samples[1:3][,1]) #Cadena 1, 2 y 3 para mu.theta
plot(post.samples[1:3][,2]) #Cadena 1, 2 y 3 para sigma.theta

plot(post.samples[[2]][,1]) #Cadena 2, mu.theta
plot(post.samples[[2]][,2]) #Cadena 2, sigma.theta

#install.packages("bayesplot")
library(bayesplot)

mcmc_areas(
  post.samples,            
  pars = c("mu.theta"),    
  prob = 0.9)

mcmc_areas(
  post.samples,            
  pars = c("mu.theta", "sigma.theta"),    
  prob = 0.9)

mcmc_trace(post.samples, pars = "mu.theta") 

mcmc_trace(post.samples, pars =  c("mu.theta", "sigma.theta")) 

library(dplyr)
library(ggplot2)

#install.packages("ggformula")
library(ggformula)

mcmc_trace(post.samples, pars = "mu.theta") %>%
  gf_facet_grid(chain ~ .) %>%
  gf_refine(scale_color_viridis_d())

#Mas ejemplos de JAGS aquí: https://rstudio-pubs-static.s3.amazonaws.com/272658_ae4d482c86514674be17042c852ebbfc.html
#Mas graficos aqui: https://rpruim.github.io/Kruschke-Notes/jags-just-another-gibbs-sampler.html

####### WINBUGS

#Es muy igual a open bugs pero no cargar open bugs y winbugs simultáneamente

#Ejemplo aca http://www.jkarreth.net/files/Lab3-4_JAGS-BUGS.html#fitting_bayesian_models_using_r2winbugsr2openbugs_(windows_only)
#Ejemplo http://zoe.bme.gatech.edu/~bv20/isye6414/Bank/RWBR.pdf

#pero es muy similar a openbug (Practicamente igual solo que tendrian que tener instalado Winbugs y no Openbugs)


