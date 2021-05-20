library(R2OpenBUGS)
data("schools")

(schools_data <- list(
  J = nrow(schools),
  y = schools$estimate,
  sigma = schools$sd
))

#Manual de instalación aquí: https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

#install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)

library(rstan)


################################# MODELO 1 #####################################


#En este modelo se combina la información de las escuelas por SAT, pero asumiendo
#diferencias entre ellas

scode<-"data {
int<lower=0> J; //  Numero de escuelas
real y[J]; // Puntaje SAT estimado
real<lower=0> sigma[J]; // error estandar por escuela
}
parameters {
real theta[J]; // Puntaje medio por escuela
real mu; // Puntaje medio global entre todas las escuelas
real<lower=0> tau; // Variabilidad de puntajes SAT entre escuelas
}
model {
mu~normal(0, 20);  //Priori para mu: normal(0,20)
tau~chi_square(8); //Priori para tau: chi square (8df)
theta ~ normal(mu, tau); //Modelo jerárquico
y ~ normal(theta, sigma);
}
generated quantities { //Aquí extraemos la logverosimilitud
  vector[J] log_lik;
  for (j in 1:J) {
    log_lik[j] = normal_lpdf(y[j] | theta[j], sigma[j]);
  }
}
"

fit1 <- stan(model_code=scode, 
             data=schools_data,        
             warmup=150, #quema
             iter=1000, #largo de la cadena
             chains=3) #cadenas

############################### CONVERGENCIA #################################

fit1

#Rhat=1 indica convergencia
#n_eff idealmente sea alto, número efectivo de la muestra para dicho parámetro

check_divergences(fit1) #Indica si hubo divergencia en alguna iteración

#check_divergences prints the number (and percentage) of iterations 
#that ended with a divergence

library(dplyr)
library(purrr)
library(ggsci)

mack_diagnostics <- rstan::get_sampler_params(fit1) %>% 
  set_names(1:3) %>% 
  map_df(as_tibble,.id = 'chain') %>% 
  group_by(chain) %>% 
  mutate(iteration = 1:length(chain)) %>% 
  mutate(warmup = iteration <= 150) #Se indica cuantas se quemaron

mack_diagnostics %>% 
  group_by(warmup, chain) %>% 
  summarise(percent_divergent = mean(divergent__ >0)) %>% 
  ggplot() +
  geom_col(aes(chain, percent_divergent, fill = warmup), position = 'dodge', color = 'black') + 
  scale_y_continuous(labels = scales::percent, name = "% Divergent Runs")  + 
  scale_fill_npg() +theme_minimal()


#Hay divergencia en la etapa previo y post a la quema!! D:


#Aumentamos el largo de la cadena y añadimos saltos:

fit2 <- stan(model_code=scode, 
             data=schools_data,        
             warmup=8000, #quema
             iter=15000, #largo de la cadena
             thin=30, #saltos
             chains=3, #cadenas
             control = list(adapt_delta = 0.99)) #Tasa aceptación

fit2 #Rhat=1

print(fit2,pars=c("mu","tau"), probs=c(.1,.5,.9))


mack_diagnostics2 <- rstan::get_sampler_params(fit2) %>% 
  set_names(1:3) %>% 
  map_df(as_tibble,.id = 'chain') %>% 
  group_by(chain) %>% 
  mutate(iteration = 1:length(chain)) %>% 
  mutate(warmup = iteration <= 8000) #Se indica cuantas se quemaron

mack_diagnostics2 %>% 
  group_by(warmup, chain) %>% 
  summarise(percent_divergent = mean(divergent__ >0)) %>% 
  ggplot() +
  geom_col(aes(chain, percent_divergent, fill = warmup), position = 'dodge', color = 'black') + 
  scale_y_continuous(labels = scales::percent, name = "% Divergent Runs")  + 
  scale_fill_npg() +theme_minimal()

#Hay divergencia pero previo a la quema :)

############################### VISUALIZACIÓN #################################

#Gráficos por default:

plot(fit2) 

traceplot(fit2,pars=c("mu","tau"), inc_warmup=TRUE, nrow=3)

traceplot(fit2,pars=c("mu","tau"), inc_warmup=FALSE, nrow=3)

pairs(fit2,pars=c("theta[1]","theta[2]","theta[3]"))

bh_summary <- summary(fit2)$summary %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_data_frame()

bh_summary %>% head()

bh_summary %>%  #n efectivos
  ggplot(aes(n_eff)) + 
  geom_histogram() + theme_minimal()


bh_summary %>%  #Boxplot de los valores a posteriori al 95%
  filter(variable %in% c('mu','tau')) %>% 
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free')+theme_minimal()


bh_summary %>%  #Boxplot de los valores a posteriori al 95%
  filter(variable %in% c('theta[1]','theta[2]', 'theta[6]')) %>% 
  ggplot() + 
  geom_linerange(aes(variable, ymin = `2.5%`,ymax = `97.5%`)) + 
  geom_crossbar(aes(variable, mean, ymin = `25%`, ymax = `75%`), fill= 'grey') + 
  facet_wrap(~variable, scales = 'free')+theme_minimal()


bh_mcmc <- fit2 %>% 
  rstan::extract()

#Si se quiere incluir la etapa de quema en los graficos 
#usar extract(inc_warmup = TRUE)

bh_pars <- bh_mcmc[ c('mu','tau')] %>% 
  map_df(as_data_frame, .id = 'variable')

bh_pars %>% 
  ggplot(aes(value, fill = variable)) + 
  geom_density() + 
  facet_wrap(~variable, scales = 'free') + 
  coord_flip() + 
  scale_fill_locuszoom()+theme_minimal()

#Todos los gráficos que vimos la sesión pasada con bayesplot son aplicables
#a STAN

library(bayesplot)

mcmc_areas(
  fit2,            
  pars = c("mu"),    
  prob = 0.9)

mcmc_trace(fit2, pars = "mu") 

library(ggformula)

mcmc_trace(fit2, pars = "mu") %>%
  gf_facet_grid(chain ~ .) %>%
  gf_refine(scale_color_viridis_d())


fit2%>% #Cuando todo converge, debe verse todo en el 1 aprox.
  rhat() %>%
  mcmc_rhat() +
  yaxis_text()


#Rhat es una relación entre la varianza de todas las cadenas 
#conjuntamente versus una ponderación por separado.
#Cuando hay convergencia debería resultar muy cerca de uno, 
#como máximo 1.05 o 1.1. Algunos autores incluso consideran 1.2, 
#pero creo que si todo va bien no debería superar 1.1.

#Estamos bien :)

#Novedad!! Se entrega un dashboard en shiny full completo para los resultados:

library(shinystan)
fit1shiny <- launch_shinystan(fit2)  #Abre un shiny que viene con todo!


################################# MODELO 2 #####################################


#En este modelo se asume independencia entre los puntajes SAT de las escuelas

model2<-"data {
  int<lower=0> J; 
  real y[J]; 
  real<lower=0> sigma[J]; 
}
parameters {
  real theta[J]; 
}
model {
  y ~ normal(theta, sigma);
}

generated quantities { //Aquí extraemos la logverosimilitud
  vector[J] log_lik;
  for (j in 1:J) {
    log_lik[j] = normal_lpdf(y[j] | theta[j], sigma[j]);
  }
}"


fit3 <- stan(model_code=model2, 
              data=schools_data,  
              warmup=8000, #quema
              iter=15000, #largo de la cadena
              thin=30, #saltos
              chains=3, #cadenas
              control = list(adapt_delta = 0.99)) #Tasa aceptación



fit3

library(loo)  #Leave-one-out cross-validation

#these measures are approximations to a score that compares some 
#predictive distribution to a true data generating process, 
#with various levels of approximation

#intended to estimate the expected log predictive density (ELPD) 
#for a new dataset

log_lik_1 <- extract_log_lik(fit2) #Modelo jerárquico combina escuelas
log_lik_2 <- extract_log_lik(fit3) #Modelo asumiendo independencia


(loofit1<-loo(log_lik_1))
(loofit2<-loo(log_lik_2))


loo_compare(loofit1, loofit2)

#El modelo 1 (jerárquico) es mejor que el modelo 2 (independencia entre escuelas)



#Fuentes:

#https://www.weirdfishes.blog/blog/fitting-bayesian-models-with-stan-and-r/
#https://pluto.coe.fsu.edu/svn/common/rgroup-shiny/Bayesian/Schools8Stan.nb.html
#https://www.datascienceblog.net/post/machine-learning/probabilistic_programming/
#https://astrostatistics.psu.edu/su14/lectures/Daniel-Lee-Stan-2.pdf


#Más ejemplos:

#https://rstudio-pubs-static.s3.amazonaws.com/455021_9628fb7a86fc4516b51baf676265e016.html

#Otra manera de plantear modelos en stan:

#http://mc-stan.org/rstanarm/reference/priors.html

