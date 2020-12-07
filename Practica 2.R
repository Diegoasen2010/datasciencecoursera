#colocas al r en el set working y elegis la carpeta con los archivos
library(ggplot2)
library(gplots)
library(dplyr)
library(foreign)
library(descr)
library(DescTools)
library(haven)
library(tidyverse)
library(psych)
library(rio)
library(Rmisc)
library(onewaytests)
library(vcd)
data = import("LAPOP_PERU_2019.dta")
summary(data)
class(data)
names(data)

#1) ejercicio
summary(data$lib1)
summary(data$lib2c)
data$indice = data$lib1 + data$lib2c
summary(data$indice)
data$libertad = (((data$indice)-2)/4)*10
summary(data$libertad)
data <- data[!is.na(data$libertad),]
ci.indicador <- CI(data$libertad, ci=0.99)
ci.indicador
describe.by(data$libertad)
"indice de percepcion de libertad de expresion" = data$libertad






#2 ejercicio
summary(data$prot3)
table(data$prot3)

table(data$prot3)
data <- data[!is.na(data$prot3),]
ci.indicador2 <- CI(data$prot3, ci=0.95)
ci.indicador2
ci.indicador3 <- CI(data$prot3, ci=0.99)
ci.indicador3
data$prot3 =as.factor(data$prot3)
levels(data$prot3)= c("si", "no")
table(data$prot3)






#3 ejercicio

plotmeans(data$libertad~data$prot3, p=0.95, xlab = "participa en una protesta", ylab = "indice de percepcion de libertad de expresion"
              , main= "participacion y percepcion")

group.CI (data$libertad~data$prot3, data)
describe.by(data$libertad, group = data$prot3)





#4 ejercicio
class(data$e5)
names(data$e5)
str(data$e5)
table(data$e5)
summary(data$e5)
data <- data[!is.na(data$e5),]
class(data$pol1)
names(data$pol1)
str(data$pol1)
data <- data[!is.na(data$pol1),]
data$pol1 =as.factor(data$pol1)
levels(data$pol1)
levels(data$pol1) = c("mucho", "algo", "poco" , "nada")
table(data$pol1)
describe.by(data$e5, group = data$pol1)
plotmeans(data$e5~data$pol1, p=0.95, xlab= "interes en la politica", ylab = "aprobacion a las manifestaciones permitidas por la ley" ) 






#5 ejercicio
class(data$q10new)
names(data$q10new)
str(data$q10new)
summary(data$q10new)
data <- data[!is.na(data$q10new),]
summary(data$q10new)
ci.indicador4= CI(data$q10new, ci=0.95)
ci.indicador4
hist(data$q10new)
class(data$q1)
str(data$q1)
data$q1 = as.factor(data$q1)
levels(data$q1) = c("hombre", "mujer")
table(data$q1)
describe.by(data$q10new , group = data$q1)
plotmeans(data$q10new~data$q1, p=0.95, xlab = "sexo", ylab = "escala economica", main= "distribucion economica por sexo")            
group.CI (data$q10new~data$q1, data)






#6 ejercicio
class(data$ed)
str(data$ed)
summary(data$ed)
data <- data[!is.na(data$ed),]
describe.by(data$ed, group=data$prot3)
plotmeans(data$ed~data$prot3, p=0.95, xlab = "participa en protestas", ylab = "nivel educativo", main="educacion-protesta")





#7 ejercicio 
#a)
tabla1.1 = table(data$prot3, data$pol1)
prop.table(tabla1.1, 2)
tabla1.2 = prop.table(tabla1.1 ,2)*100
tabla1.2
ggplot(data=subset(data, !is.na(prot3) & !is.na(pol1)), 
       aes(x=prot3, fill= pol1)) + geom_bar(position = position_fill()) + theme_bw() + 
  xlab("protesta") + ylab("interes en la politica")
#b)
chisq.test(tabla1.1)
pruebachi1 <- chisq.test(tabla1.1)
pruebachi1$expected
Lambda(tabla1.1, direction = "row")
#la dependiente esta en nuestra fila
#hablando de relaciones direccionales, nos dice que no podemos pronosticar en nada una variable a predecir la otra
#c)
assocstats(tabla1.1)

#d)
GoodmanKruskalGamma(tabla1.1)
KendallTauB(tabla1.1)
StuartTauC(tabla1.1)
SomersDelta(tabla1.1, direction = "row")

