library(survival)

?veteran

data(veteran)

str(veteran)

#Trt es variable factor

veteran$trt<-factor(ifelse(veteran$trt=="1", "Standard", "Quimioterapia test"))

contrasts(veteran$trt) #Quimioterapia queda anidado 

#Si lo queremos cambiar:

contrasts(veteran$trt)<-c(1,0)

contrasts(veteran$trt) 

#La interpretación debe hacerse respecto a la terapia estandar

#Prior está codificado con 0 y 10:

veteran$prior<-factor(ifelse(veteran$prior=="0", "No", "Yes"))

contrasts(veteran$prior) 

contrasts(veteran$celltype)

#¿y si queremos cambiar los contrastes de celltype? 

contrasts(veteran$celltype)<-rbind(diag(1, nrow=3), c(0, 0, 0))

contrasts(veteran$celltype)

vsurvfit <- survfit(Surv(time, status) ~ 1, data = veteran)

### Gráfico

library(survminer)

ggsurvplot(fit = vsurvfit, 
           data = veteran,
           title = "Overall Survival",
           subtitle = "Lung cancer",
           font.title = c(22, "bold", "black"),
           ggtheme = theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
             theme(plot.subtitle = element_text(hjust = 0.5, size = 16, face = "italic")), 
           xlab="Days",
           ylab = "Survival Probability",
           font.x=c(18,"bold"),
           font.y=c(18,"bold"), 
           font.xtickslab=c(14,"plain"), 
           font.ytickslab=c(14,"plain"),
           censor.shape="|",
           censor.size = 5,
           palette="blue",
           conf.int = TRUE,
           conf.int.fill = "purple", 
           surv.median.line = "hv",
           legend.title = " ",
           legend.labs = "All Patients", 
           risk.table = TRUE, 
           risk.table.height = 0.25,
           risk.table.fontsize = 4.5,
           legend.position="n"
)

#Modelo de Cox:

coxfit<-coxph(Surv(time,status)~trt+celltype+karno+diagtime+age+prior+prior*trt,data=veteran)

summary(coxfit)

#Los predictores con un efecto significativo sobre la sobrevivencia de los pacientes son: Karnofsky performance scale, cell type, 
#tratamiento y la interración de Prior therapy con la therapy actual. Esto se deduce de los resultados del test de Wald.

summary(coxfit)$coefficients #<---- Matriz con info de las variables

summary(coxfit)$coefficients[1,2] #exp(trt1 vs trt2)

levels(veteran$trt)

#Los pacientes en el grupo de Quimioterapia test tienen un 77 por ciento más 
#de riesgo de morir en relación a los que reciben la terapia estándar.

summary(coxfit)$coefficients[9,2] #exp(trt1:priorYes)

#Los pacientes que recibieron terapia previa y ahora están recibiendo la quimioterapia de prueba, tienen un 58
#por ciento menos de riesgos de morir.

#Se recomienda aplicar la quimioterapia test a los pacientes que recibieron una terapia previa.

summary(coxfit)$coefficients[5,2] #exp(Karno)

# Es un factor protector, de hecho el riesgo de morir disminuye en un 3 por ciento por 
# cada unidad de aumento en el kps.

summary(coxfit)$coefficients[2:4,2] #exp(cells vs large)

levels(veteran$celltype)

#Los pacientes con células tipo squamous tienen un 33 por ciento menos de riesgo de morir en relación a los que
#tienen células large. (Factor protector)

#Los pacientes con células small tienen un 61 por ciento más de riesgo de morir en relación a los que tienen 
#células large. (Factor de riesgo)

#Los pacientes con celulas tipo adeno tienen un 120 por ciento más de riesgo de morir en relación a los que
#tienen células large. (Factor de riesgo)

coxkps<-coxph(Surv(time,status)~karno,data=veteran)

S_0<-exp(-basehaz(coxkps)[,1]) #S_0(t)=exp(-H_0(t))

coefficients(coxkps)

s_ajustada<-(S_0)^exp((coefficients(coxkps)*25)) #S(t)=S_0^exp(XBeta)

model.S0<-data.frame("time"=basehaz(coxkps)[,2], "survival"=S_0)
model.kps25<-data.frame("time"=basehaz(coxkps)[,2], "survival"=s_ajustada)

plot(x=model.S0[,1],y=model.S0[,2], type="s", ylab="Sobrevivencia", xlab="Tiempo", ylim=c(0,1), las=1)
lines(x=model.kps25[,1],y=model.kps25[,2], type="s",col="red", lty=2)
legend(600,0.9, legend=c("S_0", "S(t|Kps=25)"), lty=1:2, col=c("black", "red"),cex=0.8)

test<-cox.zph(coxkps)

test #Se rechaza el supuesto de riesgos proporcionales

