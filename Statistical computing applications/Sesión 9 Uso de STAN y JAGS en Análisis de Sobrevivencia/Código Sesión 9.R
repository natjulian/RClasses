### SOBREVIVENCIA DE PACIENTES CON CANCER DE MAMA POST
#MASTECTOMIA

#Extirpación quirúrgica de una o ambas mamas de manera parcial o completa

#install.packages("HSAUR")
library("HSAUR")

data("mastectomy", package = "HSAUR")

print(mastectomy)

View(mastectomy)

mean(mastectomy$time)

log(1/96.61364) #Estimación para el intercepto (gamma)

mastectomy$metastized<-as.numeric(ifelse(mastectomy$metastized=="no", 0, 1))

str(mastectomy)

library(rstan)

scode<-" data {
    int<lower=1> N;  
    int<lower=1> N_uncensored;                                      
    int<lower=1> N_censored;                                        
    int<lower=0> NC;                                                
    matrix[N_censored,NC] X_censored;                               
    matrix[N_uncensored,NC] X_uncensored;                           
    vector<lower=0>[N_censored] times_censored;                          
    vector<lower=0>[N_uncensored] times_uncensored;                      
}

parameters {
    vector[NC] betas;                                     
    real intercept;                                 
}

model {
    betas ~ normal(0,2);                                                            
    intercept ~ normal(-5,2);      
    
    target += exponential_lpdf(times_uncensored | exp(intercept+X_uncensored*betas)); //Función de densidad para casos no censurados f(t)
    target += exponential_lccdf(times_censored | exp(intercept+X_censored*betas));  //Función de densidad acumulada complementaria 1-F(t)
}

generated quantities {
     vector[N] log_lik;
 
     for(i in 1:N_uncensored){
            log_lik[i] = exponential_lpdf(times_uncensored[i] | exp(intercept+X_uncensored[i]*betas));
      }
      for(i in 1:N_censored){
            log_lik[i+N_uncensored] = exponential_lccdf(times_censored[i] | exp(intercept+X_censored[i]*betas));
      }
}
"

(N <- nrow(mastectomy)) #Número de registros
(X <- as.matrix(mastectomy$metastized)) #Columna del predictor
(is_censored <- mastectomy$event==0) #Vector de censura (TRUE) y no censura (FALSE)
(times <- mastectomy$time) #Tiempos
(N_censored <- sum(is_censored)) #Casos censurados

stan_data <- list(N_uncensored=N-N_censored, #Casos no censurados N-casos censurados
                  N_censored=N_censored,  #Casos censurados
                  X_censored=as.matrix(X[is_censored,]), #Variable casos censurados
                  X_uncensored=as.matrix(X[!is_censored,]), #Variable casos no censurados
                  times_censored=times[is_censored], #Tiempos casos censurados
                  times_uncensored = times[!is_censored], #Tiempos casos no censurados
                  NC=ncol(X), #Numero de variables (en este caso es 1)
                  N=N)

fit1 <- stan(model_code=scode, 
             data=stan_data,        
             warmup=150, #quema
             iter=4000, #largo de la cadena
             chains=3) #cadenas

fit1

library(ggplot2)
library(bayesplot)

mcmc_dens_overlay(fit1, pars=c("betas[1]", "intercept")) 

mcmc_intervals(fit1, pars=c("betas[1]", "intercept")) 

library(dplyr)

fit1%>% #Cuando todo converge, debe verse todo en el 1 aprox.
  rhat() %>%
  mcmc_rhat() +
  yaxis_text()


library(ggformula)
mcmc_trace(fit1, pars = "betas[1]") %>%
  gf_facet_grid(chain ~ .) %>%
  gf_refine(scale_color_viridis_d())


mcmc_areas(
  fit1,            
  pars = c("betas[1]"),    
  prob = 0.9)

color_scheme_set("blue")
mcmc_pairs(fit1, pars=c("betas[1]", "intercept"))

mcmc_hex(fit1, pars=c("betas[1]", "intercept"))

color_scheme_set("mix-blue-red")
mcmc_trace(fit1, pars=c("betas[1]", "intercept"),
           facet_args = list(ncol = 1, strip.position = "left"))

library(shinystan)
fit1shiny <- launch_shinystan(fit1) 

library(survival)

#Survreg crea un modelo de regresión para los tiempos de sobrevida

modelphe<-survreg(Surv(time, event)~metastized, data=mastectomy, dist="exponential")

modelphe$coefficients #Dan en direcciones opuestas porque modelan cosas distintas!

#Survreg modela los tiempos de sobrevida
#Modelo de riesgos modela los riegos

#El riesgo es (exactamente) inversamente proporcional a la vida media 
#en modelos exponenciales.


#### Evaluación del modelo, planteemos otro modelo cambiando priori

scode2<-" data {
    int<lower=1> N;  
    int<lower=1> N_uncensored;                                      
    int<lower=1> N_censored;                                        
    int<lower=0> NC;                                                
    matrix[N_censored,NC] X_censored;                               
    matrix[N_uncensored,NC] X_uncensored;                           
    vector<lower=0>[N_censored] times_censored;                          
    vector<lower=0>[N_uncensored] times_uncensored;                      
}

parameters {
    vector[NC] betas;                                     
    real intercept;                                 
}

model {
    betas ~ normal(0,10);    
    target += exponential_lpdf(times_uncensored | exp(intercept+X_uncensored*betas)); //Función de densidad para casos no censurados f(t)
    target += exponential_lccdf(times_censored | exp(intercept+X_censored*betas));  //Función de densidad acumulada complementaria 1-F(t)
}

generated quantities {
     vector[N] log_lik;
 
     for(i in 1:N_uncensored){
            log_lik[i] = exponential_lpdf(times_uncensored[i] | exp(intercept+X_uncensored[i]*betas));
      }
      for(i in 1:N_censored){
            log_lik[i+N_uncensored] = exponential_lccdf(times_censored[i] | exp(intercept+X_censored[i]*betas));
      }
}
"

fit2 <- stan(model_code=scode2, 
             data=stan_data,        
             warmup=150, #quema
             iter=4000, #largo de la cadena
             chains=3) #cadenas


library(loo)
loglike1 <- extract_log_lik(fit1) #Extraemos la log-verosimilitud 
loglike2 <- extract_log_lik(fit2)

# WAIC
(waic1<-waic(loglike1)) #Menor waic es mejor
(waic2<-waic(loglike2))


# LOO-CV
(looc1<-loo(loglike1)) #menor looic mejor
(looc2<-loo(loglike2))

loo_compare(looc1, looc2)

# elpd: Expected log predictive density (ELPD)
#Modelo 1 es mejor, pero las diferencias no son tan considerables


### MODELO AFT DISTRIBUCIÓN VALORES EXTREMOS CON JAGS

rm(list=ls()) #Elimino todo lo anterior

library("HSAUR")

data("mastectomy", package = "HSAUR")

mastectomy$metastized<-as.numeric(ifelse(mastectomy$metastized=="no", 0, 1))

cens <- mastectomy$time  #tiempos
mastectomy$time[mastectomy$event == 0] <- NA  #Casos censurados se llenan con NA
cens[mastectomy$event == 1] <- 0 #Casos observados
is.censored <- as.numeric(is.na(mastectomy$time)) #Indica censura
mastectomy$metastized<-mastectomy$metastized
X <- model.matrix(~ metastized, data=mastectomy)

#Cens es un vector que tiene 0 para los tiempos observados
#Para los no censurados tiene las cotas inferiores 

#is.censored[i]~ dinterval(time[i], cens[i]) indica que ya sabemos
#cuales son los casos censurados y no censurados

model <- textConnection("
model{
for(i in 1:n){
is.censored[i]~ dinterval(time[i], cens[i])
time[i] ~ dweib(alpha,lambda[i])
lambda[i] <- exp(-mu[i]*alpha)
mu[i]<-inprod(beta[],X[i,])
}

#Priordistributions
for(l in 1:Nbetas){beta[l]~dnorm(0,0.001)}
sigma~dunif(0,100)
alpha<- 1/sigma
}
")

library(rjags)

d.jags <- list(n=nrow(mastectomy), time=mastectomy$time, cens=cens, X=X,
                  is.censored=is.censored, Nbetas=ncol(X))

i.jags <- function(){ list(beta=rnorm(ncol(X)), sigma=runif(1)) }
p.jags <- c("beta", "alpha")

m1 <- jags.model(data=d.jags, file=model, inits=i.jags, n.chains=3)

update(m1, 1000) #Quema

res <- coda.samples(m1, variable.names=p.jags, n.iter=10000, thin=10)

class(res) #Es una lista

head(res[1])

plot(res[1:3][,1]) #Cadena 1, 2 y 3 alpha

library(dplyr)
library(bayesplot)
library(ggformula)
mcmc_trace(res, pars = "beta[2]") %>%
  gf_facet_grid(chain ~ .) %>%
  gf_refine(scale_color_viridis_d())

summary(res[1])

#Otra librería: library(R2jags) entrega el DIC

