# TFG Alberto Jiménez Bonillo
# Intención de voto en base a encuestas indirectas: Caso de estudio de las Elecciones Generales en Andalucía.
# Universidad Carlos III de Madrid

library(networkscaleup) 
library(NSUM)
library(digest)
library(openxlsx)
library(data.table)
library(readr)

#Cargamos los datos
Pollfish_Survey_Elecciones_generales_Andalucia_381775548 <- read_delim("Downloads/Pollfish_Survey_Elecciones_generales_Andalucia_381775548.csv", 
                                                                       delim = ";", escape_double = FALSE, trim_ws = TRUE)
View(Pollfish_Survey_Elecciones_generales_Andalucia_381775548)

DatosAnd<-Pollfish_Survey_Elecciones_generales_Andalucia_381775548

#Eliminamos la columna de permisos
df <- as.data.frame(DatosAnd[,-4]) # The column with the permission is removed

colnames(df) <- c("Gender","Age","Year.Birth","PP","PSOE","Vox","Sumar","Blanco","NoSabe","edad.18.34","edad.35.54","edad.mas.54","autonomo","medico","desempleado","Education","Employment.Status","Income","CP")
colnames(df)

# Contar cuantos encuestados de cada CP (provincia)
tabla_frecuencia <- table(df$CP)
print(tabla_frecuencia)

#############################################################################################################################################

#Procesamiento de datos:

#Agrupación de variables de interés:

voting.inputs <- c("PP","PSOE","Vox","Sumar","Blanco") 
voting.inputs

socio.demo.inputs <- c("Gender","Age","Year.Birth")
socio.demo.inputs

control.inputs <- c("edad.18.34","edad.35.54","edad.mas.54","autonomo","medico","desempleado")
control.inputs

#Nuevo data frame 
dat <- df[,c(socio.demo.inputs,voting.inputs,control.inputs)]
colnames(dat)
dim(dat)

#Eliminamos filas (encuestados) incompletas
dat <- na.omit(df) 
dim(dat)

#############################################################################################################################################

#Atípicos

# Funciones:
median_abs_deviation <- function(x) {
  qq.scaled <- quantile(scale(x), c(.25, .5, .75), na.rm = T)
  quantile(abs(x - quantile(x, c(0.5), na.rm = T)), c(0.5), na.rm = T) * 1.4826
}

is_mad_outlier <- function(x,threshold.mad=5) {
  abs(x - quantile(x, c(0.5), na.rm = T)) / median_abs_deviation(x) > threshold.mad
}


#Criterio 'Mayoría'
threshold.net.size.vote.intention <- 0.90 # Modificable: 0.80,0.90,0.95 
quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

#Criterio 'Network'
quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)

#Criterio 'Blanco'
#threshold.net.size.vote.blank <- 9 # Si se opta por valor predeterminado

outliers.vote.blank.flag <- is_mad_outlier(dat[,voting.inputs[5]],5) # Modificable: 2, 3 y 5 
outliers.vote.blank.flag

threshold.net.size.vote.blank <- max(dat[!outliers.vote.blank.flag,voting.inputs[5]]) # MAD
threshold.net.size.vote.blank

#Aplicación de los 3 métodos a la vez
input.dat <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.9 & dat[,voting.inputs[5]]<=threshold.net.size.vote.blank),] #Customized filtering 
dim(input.dat)

#Aplicación de los métodos por separado (quitar # al que se quiera emplear)

# Network
#input.dat <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention)) ,] 
#dim(input.dat)

# Mayoría
#input.dat <- dat[which(apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.9),] 
#dim(input.dat)

#Blanco
#input.dat <- dat[which(dat[,voting.inputs[5]]<=threshold.net.size.vote.blank),] 
#dim(input.dat)

#Sin filtros
#input.dat <- dat
#dim(input.dat)

#############################################################################################################################################

#Variables auxiliares de control

#Datos para Andalucía:

# 18 <= edad <= 34: 
joven<-186108+436358+414336+455822
# 35 <= edad <= 54: 
adulto<-496564+598427+662810+640165
# edad >= 55: 
avanzado<-633356+551088+449705+365373+316453+213186+249845

# https://www.ine.es/jaxi/Tabla.htm?tpx=59778&L=0

autonomos<-563174

# https://www.mites.gob.es/ficheros/ministerio/sec_trabajo/autonomos/economia-soc/NoticiasDoc/NoticiasPortada/2022/Nota_Afiliacin-trabajo-autnomo_marzo_2022.pdf

medicos<-25928

# https://www.sanidad.gob.es/estadEstudios/sanidadDatos/tablas/tabla13.htm  

desempleados<-724400

# https://www.ine.es/jaxiT3/Datos.htm?t=4245


And.subpopulation.sizes <- c(joven,adulto,avanzado,autonomos,medicos,desempleados) 


Total <- 6669596 
Total
#https://www.ine.es/jaxi/Datos.htm?tpx=48409

And.subpopulation.sizes 
round(And.subpopulation.sizes/Total,4) # Proporciones
names(And.subpopulation.sizes) <- control.inputs

#################################################################################################################

# Estimaciones NSUM

input.dat.nsum <- input.dat[,c(voting.inputs,control.inputs)]
dim(input.dat.nsum)

#Preparación de los datos

control.ind <-  which(colnames(input.dat.nsum) %in% control.inputs)
control.ind

#NAIVE:

and.naive.results <- colMeans(input.dat.nsum[,voting.inputs]/rowSums(input.dat.nsum[,voting.inputs]),na.rm = TRUE)
and.naive.results

#NAIVE2:

and.naive2.results <- colSums(input.dat.nsum[,voting.inputs])/sum(rowSums(input.dat.nsum[,voting.inputs]),na.rm = TRUE)
and.naive2.results

#MoS

and.MoS.degrees <- Total*rowMeans(input.dat.nsum[,control.inputs[1:3]]/And.subpopulation.sizes[1:3])
and.MoS.degrees
and.MoS.sizes <- Total*apply(input.dat.nsum[,voting.inputs],2,function (x) mean(x[and.MoS.degrees>0]/and.MoS.degrees[and.MoS.degrees>0],na.rm = TRUE))
and.MoS.sizes

and.MoS.results <- prop.table(and.MoS.sizes)
and.MoS.results


#ROS

and.RoS.results <- colSums(input.dat.nsum[,voting.inputs])/sum(input.dat.nsum[,voting.inputs])
and.RoS.results


#MLE 

and.mle.est <- networkscaleup::killworth(input.dat.nsum,known_sizes=And.subpopulation.sizes,known_ind=control.ind,N=Total,model="MLE")
and.mle.results <- prop.table(Total*apply(input.dat.nsum[,voting.inputs],2,function (x) mean(x)/mean(and.mle.est$degrees))) 
and.mle.est$sizes/sum(and.mle.est$sizes)

#PIMLE

and.pimle.est <- networkscaleup::killworth(input.dat.nsum,known_sizes=And.subpopulation.sizes,known_ind=control.ind,N=Total,model="PIMLE")
and.pimle.results <- prop.table(Total*apply(input.dat.nsum[,voting.inputs],2,function (x) mean(x[and.pimle.est$degrees>0]/and.pimle.est$degrees[and.pimle.est$degrees>0]))) 

and.pimle.results


#Matriz de resultados:

Summary.Estimation.Matrix <- data.frame(naive.est = and.naive.results,naive2.est = and.naive2.results,MoS.est = and.MoS.results,RoS.est=and.RoS.results,mle.est = and.mle.results, pimle.est = and.pimle.results)
Summary.Estimation.Matrix

#Resultados reales:
resultados.reales <- c(0.3641,0.3348,0.1532,0.1199)
names(resultados.reales) <- c("PP","PSOE","Vox","Sumar")
resultados.reales

#Resumen:
t(round(100*Summary.Estimation.Matrix[c("Vox","PP","PSOE","Sumar","Blanco"),],1))
round(100*resultados.reales[c("Vox","PP","PSOE","Sumar")],1)

# D'Hondt 

compute_dhont_seats <- function(votes, seats) {
  party_seats <- rep(0, length(votes))
  for (i in 1:seats) {
    allocation_ratios <- votes / (party_seats + 1)
    party_with_highest_ratio <- which.max(allocation_ratios)
    party_seats[party_with_highest_ratio] <- party_seats[party_with_highest_ratio] + 1
  }
  return(party_seats)
}

#Estimación de escaños (D'Hont) 

total.number.seats <- 61

DHont.pimle <- compute_dhont_seats(Summary.Estimation.Matrix$pimle.est[1:4]/sum(Summary.Estimation.Matrix$pimle.est[1:4]),total.number.seats)
names(DHont.pimle) <- rownames(Summary.Estimation.Matrix["pimle.est"])[sort(Summary.Estimation.Matrix[-5,"pimle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.pimle

DHont.mle <- compute_dhont_seats(Summary.Estimation.Matrix$mle.est[1:4]/sum(Summary.Estimation.Matrix$mle.est[1:4]),total.number.seats)
names(DHont.mle) <- rownames(Summary.Estimation.Matrix["mle.est"])[sort(Summary.Estimation.Matrix[-5,"mle.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.mle

DHont.MoS <- compute_dhont_seats(Summary.Estimation.Matrix$MoS.est[1:4]/sum(Summary.Estimation.Matrix$MoS.est[1:4]),total.number.seats)
names(DHont.MoS) <- rownames(Summary.Estimation.Matrix["MoS.est"])[sort(Summary.Estimation.Matrix[-5,"MoS.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.MoS

DHont.naive <- compute_dhont_seats(Summary.Estimation.Matrix$naive.est[1:4]/sum(Summary.Estimation.Matrix$naive.est[1:4]),total.number.seats)
names(DHont.naive) <- rownames(Summary.Estimation.Matrix["naive.est"])[sort(Summary.Estimation.Matrix[-5,"naive.est"],decreasing=TRUE,index.return=TRUE)$ix]
DHont.naive

political.groups <- c("Vox","PP","PSOE","Sumar")

Summary.Estimation.Matrix.DHont <- data.frame(naive.est = DHont.naive[political.groups],MoS.est = DHont.MoS[political.groups],mle.est = DHont.mle[political.groups], pimle.est = DHont.pimle[political.groups])
Summary.Estimation.Matrix.DHont

# GAD3 
# https://www.gad3.com/el-tracking-diario-de-gad3-para-abc-arroja-la-siguiente-estimacion-de-escanos-por-circunscripcion-gad3-esta-realizando-un-tracking-diario-para-abc-este-pasado-domingo-publicaron-una-estimacio/

# CIS
# https://www.rtve.es/noticias/20230705/cis-encuesta-preelectoral-elecciones-generales-2023/2451231.shtml

#ESCAÑOS POR PROVINCIA
#Repetir todos los pasos anteriores para obtener los escaños de cada provincia, para ello hay que quitar el # de los datos de cada provincia:
#Para ello, df de este código ser igual a los df de cada provincia, por ejemplo, en huelva df será dfhuelva, y así con cada provincia.

#HUELVA
dfhuelva<- df[df$CP == 21, ]

#SEVILLA 
dfsevilla<- df[df$CP == 41, ]

#CÁDIZ 
dfcadiz<- df[df$CP == 11, ]

#MÁLAGA
dfmalaga<- df[df$CP == 29, ]

#CÓRDOBA
dfcordoba<- df[df$CP == 14, ]

#JAÉN
dfjaen<- df[df$CP == 23, ]

#GRANADA
dfgrx<- df[df$CP == 18, ]

#ALMERÍA (daba error, creo que por el 0) y lo tuve que hacer de la siguiente manera:

df$CP <- ifelse(df$CP == "04", "alm", df$CP)
dfalmeria<- df[df$CP == "alm", ]


#CLUSTER

#Redefinir la limpieza de datos que se quiere usar:
#input.dat <- dat[which(apply(dat[,voting.inputs],1,sum) < quantile(apply(dat[,voting.inputs],1,sum),probs = threshold.net.size.vote.intention) & apply(dat[,voting.inputs[1:4]],1,function(x) max(prop.table(x)))<=0.9 & dat[,voting.inputs[5]]<=threshold.net.size.vote.blank),] 
#dim(input.dat)

# Crear nueva base de datos solo con las variables requeridas
dat_cluster <- input.dat[, c("PP", "PSOE", "Sumar", "Vox", "Income", "Education")]

# Convertir variables a numéricas
dat_cluster$PP <- as.numeric(dat_cluster$PP)
dat_cluster$PSOE <- as.numeric(dat_cluster$PSOE)
dat_cluster$Sumar <- as.numeric(dat_cluster$Sumar)
dat_cluster$Vox <- as.numeric(dat_cluster$Vox)

# Calcular la suma de las cuatro variables para cada fila (sacar total para sacar el %)
total <- dat_cluster$PP + dat_cluster$PSOE + dat_cluster$Sumar + dat_cluster$Vox

# Calcular las proporciones
dat_cluster$PP_prop <- dat_cluster$PP / total
dat_cluster$PSOE_prop <- dat_cluster$PSOE / total
dat_cluster$Sumar_prop <- dat_cluster$Sumar / total
dat_cluster$Vox_prop <- dat_cluster$Vox / total

#Nos quedamos solo con las proporciones
dat_cluster <- dat_cluster[, c("PP_prop", "PSOE_prop", "Sumar_prop", "Vox_prop", "Income", "Education")]

# Eliminar filas con valores nulos
dat_cluster <- na.omit(dat_cluster)

# Eliminar filas con "prefer_not_to_say" en la columna 'Income' 
dat_cluster <- dat_cluster[dat_cluster$Income != "prefer_not_to_say", ]

# Recodificar la variable Income como categórica
dat_cluster$Income <- as.numeric(factor(dat_cluster$Income, levels = c("lower_i", "lower_ii", "middle_i", "middle_ii", "high_i", "high_ii")))

# Recodificar la variable Education como categórica
dat_cluster$Education <- as.numeric(factor(dat_cluster$Education, levels = c("elementary_school", "middle_school", "high_school", "vocational_technical_college", "university", "postgraduate")))

# Visualizar la nueva base de datos
str(dat_cluster)

# Estandarizar los datos
scaled_data <- scale(dat_cluster)

library(NbClust)
library(tidyverse)
k2 <- kmeans(scaled_data, centers = 4, nstart = 25)
k2

#GRÁFICAS DE INCOME Y EDUCATION POR PARTIDO Y CATEGORÍA

# Convertir Income y Education a factores 
dat_cluster$Income <- factor(dat_cluster$Income, 
                             levels = 1:6, 
                             labels = c("lower_i", "lower_ii", "middle_i", "middle_ii", "high_i", "high_ii"))

dat_cluster$Education <- factor(dat_cluster$Education, 
                                levels = 1:6, 
                                labels = c("elementary_school", "middle_school", "high_school", "vocational_technical_college", "university", "postgraduate"))

# Reconvertir cada dato de partido por fila (un mismo encuestado da 4 datos, es decir, 4 filas)
df_long <- dat_cluster %>%
  gather(key = "Party", value = "Proportion", PP_prop:Vox_prop)

# Filtrar los valores NA
df_long <- df_long %>%
  filter(!is.na(Proportion))

# Calcular las medias por grupo para Income
means_income <- df_long %>%
  group_by(Income, Party) %>%
  summarise(mean_proportion = mean(Proportion))

# Definir los colores para cada partido
party_colors <- c("PP_prop" = "blue", "PSOE_prop" = "red", "Vox_prop" = "purple", "Sumar_prop" = "green")

# Crear el boxplot con puntos y líneas para las medias por Income
ggplot(df_long, aes(x = Income, y = Proportion, fill = Party)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  geom_point(data = means_income, aes(x = Income, y = mean_proportion, color = Party), 
             position = position_dodge(width = 0.8), size = 3) +
  geom_line(data = means_income, aes(x = Income, y = mean_proportion, group = Party, color = Party), 
            position = position_dodge(width = 0.8), size = 1) +
  scale_fill_manual(values = party_colors, 
                    labels = c("PP", "PSOE", "Vox", "Sumar")) +
  scale_color_manual(values = party_colors, 
                     labels = c("PP", "PSOE", "Vox", "Sumar")) +
  labs(title = "Nivel de Ingresos vs. Proporción de conocidos que votan por cada partido",
       x = "Nivel de Ingresos",
       y = "Proporción de conocidos que votan",
       fill = "Partido",
       color = "Partido") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calcular las medias por grupo para Education
means_education <- df_long %>%
  group_by(Education, Party) %>%
  summarise(mean_proportion = mean(Proportion))

# Crear el boxplot con puntos y líneas para las medias por Education
ggplot(df_long, aes(x = Education, y = Proportion, fill = Party)) +
  geom_boxplot(alpha = 0.7, position = position_dodge(width = 0.8)) +
  geom_point(data = means_education, aes(x = Education, y = mean_proportion, color = Party), 
             position = position_dodge(width = 0.8), size = 3) +
  geom_line(data = means_education, aes(x = Education, y = mean_proportion, group = Party, color = Party), 
            position = position_dodge(width = 0.8), size = 1) +
  scale_fill_manual(values = party_colors, 
                    labels = c("PP", "PSOE", "Vox", "Sumar")) +
  scale_color_manual(values = party_colors, 
                     labels = c("PP", "PSOE", "Vox", "Sumar")) +
  labs(title = "Nivel de Educación vs. Proporción de conocidos que votan por cada partido",
       x = "Nivel de Educación",
       y = "Proporción de conocidos que votan",
       fill = "Partido",
       color = "Partido") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# --------
  
# Martín Arevalillo, J., Fernández Anta, A., y Elvira Lillo, R. (2024). Network Scale-up Methods on Aggregated Relational Data to Estimate Voting Intention. Submitted.
