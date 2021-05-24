data("mtcars")

mtcars

class(mtcars)

library(dplyr)

?mtcars 

table(mtcars$vs)

mtcars%>%
  group_by(vs)%>%
  count()

mtcars%>%
  group_by(vs)%>%
  summarise(n=n())

mtcars%>%
  group_by(vs)%>%
  mutate(prom=mean(disp)) %>% #Calcula promedio de disp para vs=0 y vs=1
  mutate(promwt=mean(wt))  #Calcula promedio de wt para vs=0 y vs=1

mtcars%>%
  group_by(vs)%>%
  mutate(prom=mean(disp)) %>% #Calcula promedio de disp para vs=0 y vs=1
  ungroup()%>%
  mutate(promwt=mean(wt))  #Calcula promedio de wt para todas las observaciones


mtcars%>%
  sample_n(10)


mtcars%>%
  group_by(vs)%>%
  sample_n(10)


mtcars%>%
  group_by(vs)%>%
  sample_n(10, weight = 1/gear)


mtcars%>%
  group_by(vs)%>%
  sample_frac(0.3, weight = 1/gear)

library(tidyverse)

(mtcars<-mtcars%>%
  rownames_to_column())

mtcars%>%
  filter(grepl("Mazda|Toyota", rowname)) 
#grepl indica si se encuentra el texto Mazda o Toyota en 
#la variable rowname


mtcars%>%
  rename(modelo=rowname)%>% #le cambia el nombre de la variable rowname a modelo
  mutate(marca=sub("\\ .*", "", modelo))

mtcars%>%
  rename(modelo=rowname)%>% #le cambia el nombre de la variable rowname a modelo
  mutate(marca=sub("\\ .*", "", modelo))%>% #Le extrae la primera palabra al modelo (marca)
  group_by(marca)%>% #agrupa por marca
  summarise(ncyl=n_distinct(cyl))%>% #cuenta valores diferentes que toma el n° de cilindros por marca
  arrange(desc(ncyl)) #ordena según esta nueva variable


mtcars%>%
  mutate(vs=ifelse(vs==1, "Motor Recto", "Motor Forma V"))

mtcars%>%
  mutate(vs=recode(vs, "0"="Motor Forma V", "1"="Motor Recto"))

mtcars%>%
  mutate(vs=case_when(vs==1 ~ "Motor Recto", TRUE~ "Motor Forma V"))


mtcars%>% 
  mutate(categoriahp = cut(hp,
                            breaks = c(50, 122, 180, Inf),
                            labels = c("Bajo", "Alto", "Potente"),
                            right = TRUE))

(mtcars<-mtcars%>%
  mutate(categoria2hp=cut_width(hp, width=60)))


mtcars%>%
  filter(categoria2hp %in% c("[30,90]", "(150,210]"))


