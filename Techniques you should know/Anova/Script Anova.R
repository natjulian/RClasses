
#### Droga y tiempos de memoria

##Se carga la base de datos

#install.packages("readr")

library(readr)
drugs <- read_delim(file.choose(), 
                    ";", escape_double = FALSE, trim_ws = TRUE)


print(drugs)


table(drugs$Drug)

#Alprazolam:

drugs$`Memory score`[which(drugs$Drug=="A")]

#Triazolam:

drugs$`Memory score`[which(drugs$Drug=="T")]

#Ninguno

drugs$`Memory score`[which(drugs$Drug=="N")]


#Grafico 


drugs$Drug<-factor(ifelse(drugs$Drug=="T", "Triazolam", ifelse(drugs$Drug=="A", "Alprazolam", "Ninguno")))
df<-data.frame(drugs)

library(ggplot2)

(p<-ggplot(data = df, aes(y = drugs$`Memory score`, x = seq_along(drugs$`Memory score`), color=drugs$Drug)) +
  geom_point() + 
  theme_minimal()+
  ggtitle("Distribución de tiempos de demora en test de memoria")+
  ylab("Tiempos de demora en test de memoria")+
  xlab("")+
  scale_y_continuous(breaks = round(seq(min(drugs$`Memory score`), max(drugs$`Memory score`), by = 7),0))+
  scale_color_manual(name = "Tratamiento", values=c("darkslateblue","gray67", "lightblue3"))+
  theme(axis.text.x = element_blank(), plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))
)

#ggsave(p, filename = "myplot.png",  bg = "transparent")  #Guarda en formato png el gráfico

library(ggpubr)

(p2<-ggboxplot(df, y="drugs$`Memory score`", x="Drug", fill="Drug", add = "jitter")+  
  xlab("")+ 
  ylab("Tiempos de demora")+ 
  scale_y_continuous(breaks = round(seq(min(drugs$`Memory score`), max(drugs$`Memory score`), by = 7),0))+
  ggtitle("Comparación tiempos de demora por droga")+ 
  scale_fill_manual(values=c("darkslateblue", "lightblue3", "gray67"))+ 
  theme_minimal()+
  theme(legend.position="bottom", axis.text.x = element_blank(), plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent")))

#ggsave(p2, filename = "myplot2.png",  bg = "transparent")  

library(dplyr)

(Medias<-drugs %>%
    group_by(Drug)%>%
    summarise(Media=mean(`Memory score`))%>%
    mutate(dif=Media-mean(drugs$`Memory score`)))


##Modelo

Time<-drugs$`Memory score`
Drug<-factor(drugs$Drug)   #Se debe definir como variable de tipo factor

contrasts(Drug)<-contr.sum #contraste suma

model<-aov(Time~Drug)

head(model.matrix(model))

coef(model)

levels(Drug)

#alpha1 se asocia al primer nivel del factor Droga (Drug1), es decir Alprazolam 
#alpha2 se asocia al segundo (Drug2), es decir, Triazolam

#Bajo el contraste suma, alpha3=-(alpha1+alpha2)

#alpha3 (Drug3: No droga)

-sum(coef(model)[2:3]) #alpha3

6.815822-4.263191-2.552631  #suma de coeficientes da cero

colnames(drugs)[2]<-"Tiempo"


#Dos opciones de tener la tabla anova:

#Manera tradicional:

anova(model)

#Manera alternativa, la funcion anova_test es muy util para efectos aleatorios

library(tidyverse)
library(rstatix)
anova_test(data = drugs, #base de datos
           dv = Tiempo,  #variable dependiente
           between = Drug, type="1") #variable efecto fijo


#### Estado psicologico

mental <- read_delim(file.choose(), 
                     ";", escape_double = FALSE, trim_ws = TRUE)


print(head(mental))

df<-data.frame(mental)

df%>%
  group_by(Type)%>%
  summarise(n=n(),
            Media=mean(Right_answers),
            Mediana=median(Right_answers), 
            Minimo=min(Right_answers), 
            Maximo=max(Right_answers), 
            Diferencia=mean(Right_answers)-mean(mental$Right_answers))

(p3<-ggboxplot(df, y="mental$Right_answers", x="Type", fill="Type", add = "jitter")+  
    xlab("")+ 
    ylab("Respuestas correctas")+ 
    scale_y_continuous(breaks = round(seq(min(mental$Right_answers), max(mental$Right_answers), by = 2),
                                      0))+
    ggtitle("Comparación respuestas correctas por estado psicológico")+ 
    scale_fill_manual(values=c("lightgreen", "lightpink", "lightskyblue3", "sandybrown"))+ 
    theme_minimal()+
    theme(legend.position="left", axis.text.x = element_blank(), plot.title=element_text(hjust=0.5, 
                                                                                         size=18),
          panel.background = element_rect(fill = "transparent"), 
          plot.background = element_rect(fill = "transparent", color = NA),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(), 
          legend.background = element_rect(fill = "transparent"),
          legend.box.background = element_rect(fill = "transparent")))

#ggsave(p3, filename = "myplot3.png",  bg = "transparent") 


#Modelo 

mental$Type<-factor(mental$Type)

contrasts(mental$Type)<-contr.sum

model<-aov(Right_answers~Type,data=mental)

anova(model)


#test t

t.test(Right_answers~Type, data=subset(mental, Type=="BD I"|Type=="BD II"))

t.test(Right_answers~Type, data=subset(mental, Type=="BD I"|Type=="Control"))

t.test(Right_answers~Type, data=subset(mental, Type=="BD II"|Type=="Control"))

t.test(Right_answers~Type, data=subset(mental, Type=="UD"|Type=="Control"))

t.test(Right_answers~Type, data=subset(mental, Type=="UD"|Type=="BD I"))

t.test(Right_answers~Type, data=subset(mental, Type=="UD"|Type=="BD II"))


# Comparaciones multiples de Tukey


TukeyHSD(model)


plot(TukeyHSD(model))


#Customizando el grafico:

tky <-as.data.frame(TukeyHSD(model)$Type)
tky$pair <- rownames(tky)

(p4<-ggplot(tky, aes(colour=cut(`p adj`, c(0, 0.01, 0.05, 1), 
                           label=c("Rechaza al 1%","Rechaza al 5%","No rechaza")))) +
  geom_hline(yintercept=0, lty="11", colour="grey30", lwd=1) +
  geom_errorbar(aes(pair, ymin=lwr, ymax=upr), width=0.3, lwd=1.5) +
  geom_point(aes(pair, diff), size=4) +
  labs(colour="Resultado")+
  xlab("Comparación")+
  ylab("Diferencia")+ 
  ggtitle("Test de comparaciones múltiples")+
  scale_colour_manual(values=c("brown2", "darkgoldenrod2", "chartreuse3"))+ 
  theme_minimal()+
  theme(legend.position="left", plot.title=element_text(hjust=0.5, size=18),
        panel.background = element_rect(fill = "transparent"), 
        plot.background = element_rect(fill = "transparent", color = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.background = element_rect(fill = "transparent"),
        legend.box.background = element_rect(fill = "transparent"))
)

#ggsave(p4, filename = "myplot4.png",  bg = "transparent") 

