#**************************************************************************************#
#**************************************************************************************#
#
#            Análisis de Encuesta Nacional de Empleo, Desempleo y Subempleo                        
#                       Transiciones y trayectorias laborales
#                               sep15-dic15-sep16-dic16
#
#     Responsable:            Andrés Peña M. 
#     Fecha de elaboración:   16/02/2019
#     Última actualización:   16/02/2020 - 21/05/2020 - 29/06/2020
#     Actualizado por:        Andrés Peña M.               
#     Contacto:               Andrés Peña M. (andres.pena.montalvo@gmail.com)
#     Organización:           FLACSO - México
#                             
#
#**************************************************************************************#
#**************************************************************************************#

rm(list=ls())
#rm(base, sec.seq, wide_cov, alphabet, sec.labels)

#Paquetes
library(TraMineR)
library(prettyR)
library(foreign)
library(cluster)
library(gmodels)
library(foreign)
library(TraMineR)
library(memisc)
library(car)
library(stratification)
library(TraMineRextras)
library(WeightedCluster)
library(questionr)
library(weights)
library(nnet)
library(texreg)
library(DAMisc)
library(generalhoslem)
library(brant)
library(expss)
library(sjPlot)
library(ggeffects)
library(effects)
library(stringr) #pad
library(cluster)
library(FactoMineR)
library(factoextra)
library(dplyr)

#Fijar el archivo de trabajo 
#file.choose()
setwd("C:\\Users\\usuario\\Desktop\\5to cuatrimestre\\Mercados de trabajo IV\\Cap4_Modelo\\Trayectorias\\bdd")

base <- read.spss("CONDACT_15años y más_sep15_dic15_sep16_dic16_final.sav", 
                  to.data.frame = T, use.value.labels = F)

#Base de datos a trabajar
base<-base[ , c(
  "CONDACTN_49",
  "CONDACTN_50",
  "CONDACTN_53",
  "CONDACTN_54",
  "PEAMSIU_49",
  "PEAMSIU_50",
  "PEAMSIU_53",
  "PEAMSIU_54",
  "SEXO_49",
  "EDAD_49",
  "P06_49",
  "AREA",
  "NIVINST_49",
  "P15_49",
  "fexpmatch",
  "CIUDAD", #"RAMA_49", "CATETRAB_49",
  "RELJEFE_49",
  "RAMA_49", "CATETRAB_49", "RAMA_50", "CATETRAB_50",
  "RAMA_53", "CATETRAB_53", "RAMA_54", "CATETRAB_54",
  "GRUPO_49", "GRUPO_50", "GRUPO_53", "GRUPO_54"
)]


#Recodificación de ramas

# base$RAMA_49<-str_pad(base$RAMA_49, 4, pad = "0")
# base$rama1<-as.numeric(substr(base$RAMA_49, 1, 2))
# base$r1 <- car:: recode(base$rama1, "1:9='1'; 10:43='2'; 45:99='3'; else='4'")
# table(base$CONDACTN_49)
# table(base$r1, base$CONDACTN_49)
# 
# base$RAMA_50<-str_pad(base$RAMA_50, 4, pad = "0")
# base$rama2<-as.numeric(substr(base$RAMA_50, 1, 2))
# base$r2 <- car:: recode(base$rama2, "1:9='1'; 10:43='2'; 45:47='3'; 49:99='4'; else='5'")
# table(base$CONDACTN_50)
# table(base$r2, base$CONDACTN_50)
# 
# base$RAMA_53<-str_pad(base$RAMA_53, 4, pad = "0")
# base$rama3<-as.numeric(substr(base$RAMA_53, 1, 2))
# base$r3 <- car:: recode(base$rama3, "1:9='1'; 10:43='2'; 45:47='3'; 49:99='4'; else='5'")
# table(base$CONDACTN_53)
# table(base$r3, base$CONDACTN_53)
# 
# base$RAMA_54<-str_pad(base$RAMA_54, 4, pad = "0")
# base$rama4<-as.numeric(substr(base$RAMA_54, 1, 2))
# base$r4 <- car:: recode(base$rama4, "1:9='1'; 10:43='2'; 45:99='3'; else='4'")
# table(base$CONDACTN_54)
# table(base$r4, base$CONDACTN_54)


#Recodificación de ramas 2
base$RAMA_50<-str_pad(base$RAMA_50, 4, pad = "0")
base$rama2<-as.numeric(substr(base$RAMA_50, 1, 2))
base$ra2 <- car:: recode(base$rama2, 
                         "1:3='1'; 
                           5:9='2'; 
                         10:33='3'; 
                         41:43='4';
                         45:47='5'; 55:56='5'; 
                         35:39='6'; 49:53='6'; 58:63='6';
                         64:84='7';
                         85:96='8';
                         else='9'")
table(base$ra2)
round(prop.table(table(base$ra2)), 2)

base$ra2 <- factor(unclass(base$ra2), levels = 1:9, 
                   labels = c("AgrR",
                              "Min",
                              "Man",
                              "Con",
                              "Com",
                              "SB",
                              "SF",
                              "SSP",
                              "Nin"))


base$RAMA_53<-str_pad(base$RAMA_53, 4, pad = "0")
base$rama3<-as.numeric(substr(base$RAMA_53, 1, 2))
base$ra3 <- car:: recode(base$rama3, 
                         "1:3='1'; 
                           5:9='2'; 
                         10:33='3'; 
                         41:43='4';
                         45:47='5'; 55:56='5'; 
                         35:39='6'; 49:53='6'; 58:63='6';
                         64:84='7';
                         85:96='8';
                         else='9'")
table(base$ra3)
round(prop.table(table(base$ra3)), 2)

base$ra3 <- factor(unclass(base$ra3), levels = 1:9, 
                   labels = c("AgrR",
                              "Min",
                              "Man",
                              "Con",
                              "Com",
                              "SB",
                              "SF",
                              "SSP",
                              "Nin"))


#Recodificación de categorías de ocupación
# base$o1 <- car:: recode(base$CATETRAB_49, "1:4='1'; 7:10='1'; 5:6='2'; else='3'")
# table(base$CATETRAB_49)
# table(base$o1, base$CATETRAB_49)
# 
# base$o2 <- car:: recode(base$CATETRAB_50, "1:4='1'; 7:10='1'; 5:6='2'; else='3'")
# table(base$CATETRAB_50)
# table(base$o2, base$CATETRAB_50)
# 
# base$o3 <- car:: recode(base$CATETRAB_53, "1:4='1'; 7:10='1'; 5:6='2'; else='3'")
# table(base$CATETRAB_53)
# table(base$o3, base$CATETRAB_53)
# 
# base$o4 <- car:: recode(base$CATETRAB_54, "1:4='1'; 7:10='1'; 5:6='2'; else='3'")
# table(base$CATETRAB_54)
# table(base$o4, base$CATETRAB_54)


#Recodificación de categorías de ocupación 2
base$po2 <- car:: recode(base$CATETRAB_50, 
                         "1='1';
                          2:3='2';
                          4='3';
                          5='4';
                          6='5';
                          7:10='6'; 
                          else='7'")
table(base$CATETRAB_50)
table(base$po2, base$CATETRAB_50)
table(base$po2)
round(prop.table(table(base$po2)), 2)

base$po2 <- factor(base$po2, levels = 1:7, 
                   labels = c("Gob",
                              "Priv",
                              "Jor",
                              "Patr",
                              "CPr",
                              "THog",
                              "NoT"
                   ))


base$po3 <- car:: recode(base$CATETRAB_53, 
                         "1='1';
                          2:3='2';
                          4='3';
                          5='4';
                          6='5';
                          7:10='6'; 
                          else='7'")
table(base$CATETRAB_53)
table(base$po3, base$CATETRAB_53)
table(base$po3)
round(prop.table(table(base$po3)), 2)

base$po3 <- factor(base$po3, levels = 1:7, 
                   labels = c("Gob",
                              "Priv",
                              "Jor",
                              "Patr",
                              "CPr",
                              "THog",
                              "NoT"
                   ))


#Recodificación CIUO 08 - Militares al grupo 5 
base$GRUPO_50<-str_pad(base$GRUPO_50, 4, pad = "0")
base$co2<-as.numeric(substr(base$GRUPO_50, 1, 1))
base$co2 <- car:: recode(base$co2, 
                         "0='5';
                          1='1';
                          2='2';
                          3='3';
                          4='4';
                          5='5';
                          6='6';
                          7='7';
                          8='8';
                          9='9';
                          else='10'")
table(base$co2)
round(prop.table(table(base$co2)), 2)

base$co2 <- factor(unclass(base$co2), levels = 1:10, 
                   labels = c("Dire",
                              "Inte",
                              "ProM",
                              "Admi",
                              "SyV",
                              "AgrO",
                              "OMyA",
                              "OInd",
                              "Elem",
                              "Ning"))



base$GRUPO_53<-str_pad(base$GRUPO_53, 4, pad = "0")
base$co3<-as.numeric(substr(base$GRUPO_53, 1, 1))
base$co3 <- car:: recode(base$co3, 
                         "0='5';
                          1='1';
                          2='2';
                          3='3';
                          4='4';
                          5='5';
                          6='6';
                          7='7';
                          8='8';
                          9='9';
                          else='10'")
table(base$co3)
round(prop.table(table(base$co3)), 2)

base$co3 <- factor(unclass(base$co3), levels = 1:10, 
                   labels = c("Dire",
                              "Inte",
                              "ProM",
                              "Admi",
                              "SyV",
                              "AgrO",
                              "OMyA",
                              "OInd",
                              "Elem",
                              "Ning"))


# Etiquetado de variables

# base$r1 <- as.factor(base$r1)
# base$r2 <- as.factor(base$r2)
# 
# base$r2 <- factor(unclass(base$r2), levels = 1:5, 
#                   labels = c("1rio",
#                              "2rio",
#                              "Com",
#                              "Ser",
#                              "NoAc"))
# table(base$rama2, base$r2)
# base$r2 <- relevel(base$r2, 
#                    ref = "Ser")
# 
# 
# 
# base$r3 <- factor(unclass(base$r3), levels = 1:5, 
#                   labels = c("1rio",
#                              "2rio",
#                              "Com",
#                              "Ser",
#                              "NoAc"))
# table(base$rama3, base$r3)
# base$r3 <- relevel(base$r3, 
#                    ref = "Ser")
# 
# base$r4 <- as.factor(base$r4)
# 
# # 
# base$o1 <- as.factor(base$o1)
# 
# base$o2 <- factor(unclass(base$o2), levels = 1:3, 
#                   labels = c("Asal",
#                              "Auto",
#                              "NoAc"))
# table(base$CATETRAB_50, base$o2)
# base$o2 <- relevel(base$o2, 
#                    ref = "Asal")
# 
# 
# base$o3 <- factor(unclass(base$o3), levels = 1:3, 
#                   labels = c("Asal",
#                              "Auto",
#                              "NoAc"))
# table(base$CATETRAB_53, base$o3)
# base$o3 <- relevel(base$o3, 
#                    ref = "Asal")
# 
# 
# base$o4 <- as.factor(base$o4)



#Tabulados
table(base$EDAD_49)
table(base$CONDACTN_49)
round(prop.table(table(base$CONDACTN_49)),2)
table(base$CONDACTN_49, base$PEAMSIU_49)

table(base$PEAMSIU_49)
round(prop.table(table(base$PEAMSIU_49)),2)

#Eliminando la Inactividad en los 4 períodos
#base <- base %>% 
#        filter(!(CONDACTN_49==9 & CONDACTN_50==9 & 
#               CONDACTN_53==9 & CONDACTN_54==9))

base <- base %>%
  filter(AREA==1)

# base <- base %>%
#   filter(AREA==1 & SEXO_49==1)



#------------------------------------------------------------------------- 
#------------------------------------------------------------------------- 

#Categorias


#Formal e informal
base<-base%>%mutate(form49=if_else(PEAMSIU_49==1, 1,
                                   if_else(PEAMSIU_49==5, 99, 
                                           if_else(PEAMSIU_49==2|PEAMSIU_49==3,2,999))))
table(base$PEAMSIU_49, base$form49)

base<-base%>%mutate(form50=if_else(PEAMSIU_50==1, 1,
                                   if_else(PEAMSIU_50==5, 99,                                    
                                           if_else(PEAMSIU_50==2|PEAMSIU_50==3,2,999))))
table(base$PEAMSIU_50, base$form50)

base<-base%>%mutate(form53=if_else(PEAMSIU_53==1, 1,
                                   if_else(PEAMSIU_53==5, 99,                                    
                                           if_else(PEAMSIU_53==2|PEAMSIU_53==3,2,999))))
table(base$PEAMSIU_53, base$form53)

base<-base%>%mutate(form54=if_else(PEAMSIU_54==1, 1,
                                   if_else(PEAMSIU_54==5, 99,
                                           if_else(PEAMSIU_54==2|PEAMSIU_54==3,2,999))))
table(base$PEAMSIU_54, base$form54)


#Condición de actividad
base<-base%>%
  mutate(act49=if_else(CONDACTN_49==1,1,
                       if_else(CONDACTN_49==6,99,
                               if_else(CONDACTN_49>=2 & CONDACTN_49<=5,2,
                                       if_else(CONDACTN_49>=7 & CONDACTN_49<=8,3,
                                               if_else(CONDACTN_49==9,4,999
                                               )
                                       )
                               )
                       )
  )
  )
table(base$CONDACTN_49, base$act49)

base<-base%>%
  mutate(act50=if_else(CONDACTN_50==1,1,
                       if_else(CONDACTN_50==6,99,
                               if_else(CONDACTN_50>=2 & CONDACTN_50<=5,2,
                                       if_else(CONDACTN_50>=7 & CONDACTN_50<=8,3,
                                               if_else(CONDACTN_50==9,4,999
                                               )
                                       )
                               )
                       )
  )
  )
table(base$CONDACTN_50, base$act50)

base<-base%>%
  mutate(act53=if_else(CONDACTN_53==1,1,
                       if_else(CONDACTN_53==6,99,
                               if_else(CONDACTN_53>=2 & CONDACTN_53<=5,2,
                                       if_else(CONDACTN_53>=7 & CONDACTN_53<=8,3,
                                               if_else(CONDACTN_53==9,4,999
                                               )
                                       )
                               )
                       )
  )
  )
table(base$CONDACTN_53, base$act53)

base<-base%>%
  mutate(act54=if_else(CONDACTN_54==1,1,
                       if_else(CONDACTN_54==6,99,
                               if_else(CONDACTN_54>=2 & CONDACTN_54<=5,2,
                                       if_else(CONDACTN_54>=7 & CONDACTN_54<=8,3,
                                               if_else(CONDACTN_54==9,4,999
                                               )
                                       )
                               )
                       )
  )
  )
table(base$CONDACTN_54, base$act54)


#----------------------------------------------------------------------------
#----------------------------------------------------------------------------
#Categorización final

base<-base%>%
  mutate(cat49=if_else(form49==1 & act49==1,1,
                       if_else(form49==1 & act49==2,2,
                               if_else(form49==2 & act49==1,3,
                                       if_else(form49==2 & act49==2,4,
                                               if_else(act49==3,5,                                        
                                                       if_else(act49==4,6,99
                                                       )
                                               )
                                       )
                               )
                       )
  )
  )
table(base$CONDACTN_49, base$cat49)
table(base$act49, base$cat49)
base$cat49[base$cat49==99] <- NA


base<-base%>%
  mutate(cat50=if_else(form50==1 & act50==1,1,
                       if_else(form50==1 & act50==2,2,
                               if_else(form50==2 & act50==1,3,
                                       if_else(form50==2 & act50==2,4,
                                               if_else(act50==3,5,                                        
                                                       if_else(act50==4,6,99
                                                       )
                                               )
                                       )
                               )
                       )
  )
  )
table(base$CONDACTN_50, base$cat50)
table(base$act50, base$cat50)
base$cat50[base$cat50==99] <- NA


base<-base%>%
  mutate(cat53=if_else(form53==1 & act53==1,1,
                       if_else(form53==1 & act53==2,2,
                               if_else(form53==2 & act53==1,3,
                                       if_else(form53==2 & act53==2,4,
                                               if_else(act53==3,5,                                        
                                                       if_else(act53==4,6,99
                                                       )
                                               )
                                       )
                               )
                       )
  )
  )
table(base$CONDACTN_53, base$cat53)
table(base$act53, base$cat53)
base$cat53[base$cat53==99] <- NA


base<-base%>%
  mutate(cat54=if_else(form54==1 & act54==1,1,
                       if_else(form54==1 & act54==2,2,
                               if_else(form54==2 & act54==1,3,
                                       if_else(form54==2 & act54==2,4,
                                               if_else(act54==3,5,                                        
                                                       if_else(act54==4,6,99
                                                       )
                                               )
                                       )
                               )
                       )
  )
  )
table(base$CONDACTN_54, base$cat54)
table(base$act54, base$cat54)
base$cat54[base$cat54==99] <- NA



"*********************************************************************************"
"*********************************************************************************"

"Covariables"
## genero factores con etiquetas en mis covariables de interes
wide_cov <- base[,9:15]

wide_cov$SEXO_49 <- factor(wide_cov$SEXO_49, levels=1:2, labels=c("Hombre", "Mujer")) 
table(wide_cov$SEXO_49)

#Nivel de educación
wide_cov$educa <- recode(wide_cov$NIVINST_49, 
                         "1:5='1';6:7='2';8:10='3'; else=NA")
wide_cov$educa <- factor(wide_cov$educa, levels = 1:3, 
                         labels = c("Básica",
                                    "Media",
                                    "Superior"))
table(wide_cov$NIVINST_49, wide_cov$educa)

#Etnia
wide_cov$etnia <- car::recode(wide_cov$P15_49,
                              "c(6,7,8)='1';c(1,5)='2';c(2,3,4)='3'")

wide_cov$etnia <- factor(wide_cov$etnia, levels = 1:3,
                         labels = c("Mes-Bla",
                                    "Indi-mont",
                                    "Afro"))
table(wide_cov$P15_49, wide_cov$etnia)



#Edad
wide_cov$edad <- with(wide_cov, cut(EDAD_49, c(15, 29, 54, 98), 
                                    include.lowest = T, labels=F)
)
table(wide_cov$EDAD_49, wide_cov$edad)
##Recodificación de edad
wide_cov$edad <- factor(wide_cov$edad, levels = 1:3,
                        labels = c("Joven",
                                   "Adulto",
                                   "Mayor"))
table(wide_cov$edad)
prop.table(table(wide_cov$edad))


#Estado civil
wide_cov$ec <- car::recode(wide_cov$P06_49, 
                           "c(1,5)='1';2:4='2';6='3'; else=NA")

wide_cov$ec <- factor(wide_cov$ec, levels = 1:3, 
                      labels = c("Cas-Uni",
                                 "Sep-Div-Viu",
                                 "Soltero"))

table(base$P06_49, wide_cov$ec)



"Definición de objeto de secuencia"

names(base)
names(base)[names(base)=="cat49" | 
              names(base)=="cat50" |
              names(base)=="cat53" |
              names(base)=="cat54"
            ] <- c("sep15", "dic15", "sep16", "dic16")

base <- base %>%
  mutate(sep15 = if_else(is.na(sep15), 7, sep15),
         dic15 = if_else(is.na(dic15), 7, dic15),
         sep16 = if_else(is.na(sep16), 7, sep16),
         dic16 = if_else(is.na(dic16), 7, dic16))

## Determino etiquetas y codigos del alfabeto 
#ver el alfabeto de estados seleccionando las columnas que contienen a los mismos (de la 2 a la 26)
seqstatl(base[, c("sep15", "dic15", "sep16", "dic16")])

#defino etiquetas en un vector de estados
sec.labels <- c("F_A", "F_I", "I_A", "I_I", "Des", "Ina", "N/C")
sec.labels

alphabet <- c("F_A", "F_I", "I_A", "I_I", "Des", "Ina", "N/C")

#genero el objeto de secuencia
names(base)

sec.seq <- seqdef(base, var=c("sep15", "dic15", "sep16", "dic16"), levels= 1:7, 
                  states= sec.labels, labels=sec.labels, 
                  right=NA, left=NA, missing=NA, weights = wide_cov$fexpmatch)


seqlength(sec.seq[1,])
summary(sec.seq)


#------------------------------------------------------------------------
"------------------------------------------------------------------------"
#------------------------------------------------------------------------

# Distribuci?n de frecuencias de estados seqtab() function 
## por defecto arroja las 10 secuencias m?s frecuentes.
## para obtener m?s o menos secuencias trabajar con idxs
## tambien el argumento format permite modificar el mismo, por defecto es SPS

seqtab(sec.seq, idxs=1:20, format="STS")

##establezco el vector de colores asociado a los estados de mi objeto de secuencia

#cpal(sec.seq) <- c("mistyrose2", "red", "cyan3", "yellow", "magenta3", "olivedrab3")


# una manera gr?fica de ver la tabla de distribuci?n de las 10 secuencias m?s frecuentes 
## con el ancho de las barras proporcional por frecuencia se obtiene con seqfplot() 
### se puede obtener por covariantes usado el argumento group

seqfplot(sec.seq, border = NA, cex.legend = 0.5)
seqfplot(sec.seq, group = wide_cov$SEXO_49, border = NA, 
         cex.legend = 0.6, cex.axis=0.8, cex.lab=0.9)
#seqfplot(sec.seq, group = wide_cov$AREA, border = NA, cex.legend = 0.5)
seqfplot(sec.seq, group = wide_cov$edad, border = NA, cex.legend = 0.5)
seqfplot(sec.seq, group = wide_cov$estado, border = NA, cex.legend = 0.5)  


# Sequence index plots 
##permite visualizar las secuencias individuales en barras horizontales de ancho fijo 
### con colores diferenciados por estados en sus sucesivas posiciones en el tiempo

## seqiplot() function genera estas gr?ficas, por defecto arroja las 10 secuencias, usar idxs para modificar.
seqiplot(sec.seq, border = NA, cex.legend = 0.5)

## si queremos graficar TODAS las secuencias individuales usamos seqIplot()
### podemos estableer el criterio de orden de las secuencias
### podemos usar covariantes

## si quiero ordenarlas por inicio de secuencia
seqIplot(sec.seq, border = NA, sortv = "from.start", with.legend=TRUE, main="", cex.legend = 0.5)

## si quiero ordenarlas por fin de secuencia
seqIplot(sec.seq, border = NA, sortv = "from.end", with.legend=TRUE, main="Tray.ind", cex.legend = 0.5)


"---------------------------"
##ordenadas por distancia a la secuencia más frecuente

dist.mostfreq <- seqdist(sec.seq, method = "LCS", refseq = 0, with.missing = TRUE)
gc()

seqIplot(sec.seq, border = NA, sortv = dist.mostfreq)
seqIplot(sec.seq, border = NA, group = wide_cov$SEXO_49, sortv = dist.mostfreq)

"---------------------------"

## si quiero ver por covariantes y por distiintos criterior de ordenaci?n de las secuencias individuales

seqIplot(sec.seq, border = NA, group = wide_cov$SEXO_49, 
         sortv = "from.start")
#seqIplot(sec.seq, border = NA, group = wide_cov$AREA, sortv = "from.start", cex.legend = 0.5)

seqIplot(sec.seq, border = NA, group = wide_cov$edad, sortv = "from.start")
seqIplot(sec.seq, border = NA, group = wide_cov$estado, sortv = "from.end")


# Tiempo promedio en cada estado
## en este caso no son necesariamente consecutivos los estados: seqmtplot()

seqmtplot(sec.seq,  cex.lab = 0.7, cex.main = 0.7, cex.legend=0.5)

## se puede obtener por covariantes
seqmtplot(sec.seq, group = wide_cov$SEXO_49, cex.legend=0.7)
seqmtplot(sec.seq, group = wide_cov$EDAD_49, cex.legend=0.7)
seqmtplot(sec.seq, group = wide_cov$AREA, cex.legend=0.7)
seqmtplot(sec.seq, group = wide_cov$estado, cex.legend=0.7)

## IMPORTANTE: ver ayuda de seqplot
##ah? se detalla la variedad de gr?ficos de objeto de secuencia 
## as? como los argumentos espec?ficos que tiene en cada caso gr?fico
?seqplot

##estad?sticos TRANSVERSALES
names(sec.seq)

##La funci?n seqstatd() nos proporciona la frecuencia de estados
# El n?mero de estados v?lidos y la entrop?a de la distribuci?n de los estados en cada posici?n (momento de observaci?n) en la secuencia
dim(sec.seq)
seqstatd(sec.seq, with.missing=TRUE)
seqstatd(sec.seq[wide_cov$SEXO_49=="Mujer",], with.missing=TRUE, weighted=T)
seqstatd(sec.seq[wide_cov$SEXO_49=="Hombre",], with.missing=TRUE)

## entrop?as transversales
seqHtplot(sec.seq, group = wide_cov$SEXO_49)

##la funci?n seqplot.tentrop() es la que permite muchas l?neas de entrop?a por gr?fico (covariantes)

seqplot.tentrop(sec.seq, group = wide_cov$SEXO_49, 
                main = "Sexo", ylim = c(0.7, 0.9), 
                cex = 0.6, cex.lab = 0.4, cex.main = 0.6,
                weighted=F)
seqplot.tentrop(sec.seq, group = wide_cov$edad, title = "Cohorte", ylim = c(0, 1), cex = 0.6, cex.lab = 0.4, cex.main = 0.6)
#seqplot.tentrop(sec.seq, group = wide_cov$AREA, title = "Estrato", ylim = c(0, 1), cex = 0.6, cex.lab = 0.4, cex.main = 0.6)
seqplot.tentrop(sec.seq, group = wide_cov$ec, title = "Estado civil", ylim = c(0, 1), cex = 0.6, cex.lab = 0.4, cex.main = 0.6)


# Gr?ficos de medidas transversales
## Histograma
seqdplot(sec.seq, with.missing=TRUE, cex.legend=0.7)

#por covariantes
seqdplot(sec.seq, group = wide_cov$SEXO_49, 
         with.missing=TRUE, cex.legend = 0.8,
         cex.axis = 0.8)
seqdplot(sec.seq, group = wide_cov$edad, with.missing=TRUE)
seqdplot(sec.seq, group = wide_cov$estado, with.missing=TRUE)


## Estados modales: muestra el estado m?s frevuente en cada punto en el tiempo
## el estado modal lo arroja la funci?n seqmodst() y la gr?fica la seqmsplot()

seqmsplot(sec.seq, with.missing=TRUE, cex.legend=0.7)
seqmsplot(sec.seq, group = wide_cov$SEXO_49, with.missing=TRUE)
#seqmsplot(sec.seq, group = wide_cov$AREA, with.missing=TRUE)
seqmsplot(sec.seq, group = wide_cov$edad, with.missing=TRUE)

#Número de transiciones
seqtransn(sec.seq)

##-------------------------------------------------------------------------

# Matriz de tasas de transici?n entre todos los estados  (considerando todo el periodo de observaci?n, totalidad de meses)
trate <-   seqtrate(sec.seq)
round(trate,3)
##con missing
#trate <- seqtrate(sec.seq, with.missing=TRUE, weighted=FALSE)
#round(trate, 3)

##Transiciòn entre trimestres - con time.varying
trate <- seqtrate(sec.seq, with.missing=F, weighted=FALSE, time.varying=TRUE)
# time.varying=TRUE: en este caso el tiempo se agrega como una tercera dimensi?n
#calcula la matriz para cada unidad de tiempo: ciclo educativo 08, ciclo 09, ciclo 10 y ciclo 11
round(trate, 3)

##con lag: un año atrás
#lag es el tiempo considerado entre dos estados para calcular las tasas de transici?n (1 por defecto).

trate <- seqtrate(sec.seq, with.missing=TRUE, weighted=FALSE, lag=3)
round(trate, 3)

##estados y sus duraciones
seqdss(sec.seq)


## Transiciones: seqtransn()

trans <- seqtransn(sec.seq, with.missing=FALSE)

trans #arroja el n?mero de transiciones de cada una de las secuencias individuales
table(trans) #arroja la distribuci?n de las transiciones: cu?ntas secuencias hicieron 0, 1, 2 o 3 transiciones
prop.table(table(trans))
prop.table(table(trans, wide_cov$SEXO_49))


##Estados y sus duraciones
print(sec.seq[1:10, ], format = "SPS")


"Turbulencia"
#Sequence turbulence

##Turbulence
seqsubsn(sec.seq[2, ], DSS = FALSE)
seqsubsn(sec.seq[7, ], DSS = T)
seqST(sec.seq[10, ])

base <- data.frame(base, seqST(sec.seq, norm = T))
tur1516 <- data.frame(seqST(sec.seq, norm = T), t=rep("15-16",nrow(base)))

unique(base$Turbulence)
plot(round(prop.table(table(base$Turbulence)), 2))
summary(base$Turbulence)
hist(base$Turbulence, main = "Sequence turbulences - trayectorias",
     col = "cyan", xlab = "Turbulence", breaks=50)

#cuantil_t <- quantile(base$Turbulence, 
#                       c(0, 0.33, 0.66, 1))

tur_est <- strata.cumrootf(base$Turbulence, 3)
abline(v=tur_est$bh, col=c("red", "red"), lty=c(2,2), lwd=c(3, 3))

turb.group <- cut(base$Turbulence, c(0, tur_est$bh, 1), 
                  include.lowest = T, labels = c("Turb. Baja", "Turb. Media", "Turb. Alta"))
barplot(prop.table(table(turb.group)), col = c("green", "yellow",  "red"))

seqfplot(sec.seq, group = turb.group, pbarw = TRUE,
         cex.lab=0.8, cex.axis=0.8, cex.legend=0.8)


"Entrophy"
base <- data.frame(base, seqient(sec.seq, norm =T))
ent1516 <- data.frame(seqient(sec.seq, norm =T), t=rep("15-16",nrow(base)))
plot(base$Turbulence, base$Entropy, col=c("blue"), ylim = 0:1)



"---------------------------------------------------------------------------------"

"Clusters"
set.seed(12345)

couts <- seqsubm(sec.seq, method = "TRATE")
biofam.om <- seqdist(sec.seq, method = "OM", indel = 3, sm = couts)
#clusterward$ac #Agglomerative coefficient
#clusterdiana <- diana(biofam.om, diss = TRUE)
clusterward <- agnes(biofam.om, diss = TRUE, method = "ward")
pltree(clusterward, cex=0.6, hang=-1)
rect.hclust(clusterward, k=3, border=c("#FDC086", "#F0027F", "#7FC97F"))
abline(h = 100, lty = 2, col="blue")


#Validación de los grupos
cluster3 <- cutree(clusterward, k=3)
#cluster4 <- cutree(clusterward, k=4)

cluster3 <- factor(cluster3,
                   levels = 1:3,
                   labels = c("T. informales-inestables",
                              "T. fuera ML e inserción precaria",
                              "T. ML buenas condiciones" 
                   ))


"Cluster Quality_15-16"

#Tiempo promedio en cada estado 
seqmtplot(sec.seq, group = cluster3, cex.lab=0.7, 
          cex.axis=0.8, cex.main=0.9)


#Super duper 
# set.seed(5665)
# fviz_dend(x = clusterward,
#           k = 3,
#           k_colors = c("#2E9FDF", "#00AFBB", "#E7B800"),
#           color_labels_by_k = TRUE,
#           rect = TRUE,
#           rect_border = c("#2E9FDF", "#00AFBB", "#E7B800"),
#           rect_fill = TRUE,
#           cex = 0.5,
#           main = "Dendrograma - Ward",
#           xlab = "Observaciones",
#           ylab = "Distancia",
#           sub = "")

# clusterpam <- pam(biofam.om, k=3, diss = TRUE)
# cluster3 <- clusterpam$clustering


# seqfplot(sec.seq, group = cluster3, pbarw = T, cex.lab=0.8, 
#          cex.legend=0.8, cex.axis=0.8)
# seqdplot(sec.seq, group = cluster3, with.missing=TRUE)
seqIplot(sec.seq, border = NA, group = cluster3, 
         sortv = "from.start", cex.lab=0.8,
         cex.legend=0.8, cex.axis=0.8, cex.main=0.9)

# seqIplot(sec.seq, border = NA, group = turb.group, 
#          sortv = "from.start")
# seqdplot(sec.seq, group = turb.group, with.missing=TRUE)
# seqfplot(sec.seq, border = NA, group = turb.group, 
#          sortv = "from.start")


base <- data.frame(base, turb.group, cluster3)


table(cluster3, turb.group)
x<-wtd.table(base$cluster3, base$turb.group, weights = base$fexpmatch)
wtd.chi.sq(base$cluster3, base$turb.group, weight=base$fexpmatch)
x<-cbind(x, rowSums(x))
x<-rbind(x, colSums(x))
x
write.table(x, "clipboard", sep = "\t", row.names = F)


"---------------------------------------------------------------------------------"

#Análisis de la tipología de trayectorias

tab1<-cro_rpct(base$SEXO_49, base$cluster3, weight = base$fexpmatch, total_statistic = "w_cases", total_row_position="none")
tab2<-cro_rpct(wide_cov$edad, base$cluster3, weight = base$fexpmatch, total_statistic = "w_cases", total_row_position="none")
tab3<-cro_rpct(wide_cov$etnia, base$cluster3, weight = base$fexpmatch, total_statistic = "w_cases", total_row_position="none")
tab4<-cro_rpct(wide_cov$educa, base$cluster3, weight = base$fexpmatch, total_statistic = "w_cases", total_row_position="none")
tab5<-cro_rpct(wide_cov$ec, base$cluster3, weight = base$fexpmatch, total_statistic = "w_cases", total_row_position="none")
tab6<-cro_rpct(base$turb.group, base$cluster3, weight = base$fexpmatch, total_statistic = "w_cases", total_row_position="none")
x<-rbind(tab1,tab2,tab3,tab4,tab5,tab6)
write.table(x, "clipboard", sep = "\t", row.names = F)

tab1<-cro_rpct(base$SEXO_49, base$turb.group, weight = base$fexpmatch, total_statistic = "w_cases", total_row_position="none")
tab2<-cro_rpct(wide_cov$edad, base$turb.group, weight = base$fexpmatch, total_statistic = "w_cases", total_row_position="none")
tab3<-cro_rpct(wide_cov$etnia, base$turb.group, weight = base$fexpmatch, total_statistic = "w_cases", total_row_position="none")
tab4<-cro_rpct(wide_cov$educa, base$turb.group, weight = base$fexpmatch, total_statistic = "w_cases", total_row_position="none")
tab5<-cro_rpct(wide_cov$ec, base$turb.group, weight = base$fexpmatch, total_statistic = "w_cases", total_row_position="none")
y<-rbind(tab1,tab2,tab3,tab4,tab5)
write.table(y, "clipboard", sep = "\t", row.names = F)

tabla <- cbind(x,y[,2:4])


"---------------------------------------------------------------------------------"
#Modelo logit multinomial
"Modelo 2015-2016"
"------------------------------------------------------------------"

"Covariables"

#Sexo
base$SEXO_49 <- factor(base$SEXO_49, levels=1:2, labels=c("Hombre", "Mujer")) 
table(base$SEXO_49)
names(base)[names(base) == "SEXO_49"] <- "sexo"

#Nivel de educación
base$educa <- car::recode(base$NIVINST_49, 
                          "1:5='1';6:7='2';8:10='3'; else=NA")
base$educa <- factor(base$educa, levels = 1:3, 
                     labels = c("Básica",
                                "Media",
                                "Superior"))
table(base$NIVINST_49, base$educa)

#Etnia
base$etnia <- car::recode(base$P15_49,
                          "c(6,7,8)='1';c(1,5)='2';c(2,3,4)='3'")

base$etnia <- factor(base$etnia, levels = 1:3,
                     labels = c("Mes-Bla",
                                "Indi-mont",
                                "Afro"))
table(base$P15_49, base$etnia)

#Edad
base$edad <- with(base, cut(EDAD_49, c(15, 29, 54, 98), 
                            include.lowest = T, labels=F)
)
table(base$EDAD_49, base$edad)
##Recodificación de edad
base$edad <- factor(base$edad, levels = 1:3,
                    labels = c("Joven",
                               "Adulto",
                               "Mayor"))
table(base$edad)
prop.table(table(base$edad))


#Estado civil
base$ec <- car::recode(base$P06_49, 
                       "c(1,5)='1';2:4='2';6='3'; else=NA")


base$ec <- factor(base$ec, levels = 1:3, 
                  labels = c("Cas-Uni",
                             "Sep-Div-Viu",
                             "Soltero"))

table(base$P06_49, base$ec)


#prov
base$prov <- as.numeric(substr(base$CIUDAD,1,2))
# base$reg <- car::recode(base$prov, 
#                        "c(17,1,18,11)='1';c(9,13,12,7)='2'; else='3'")
base$reg <- car::recode(base$prov, 
                        "c(17)='1';c(9)='2'; else='3'")

base$reg <- factor(base$reg,
                   levels = 1:3,
                   labels = c("Pichincha", 
                              "Guayas",
                              "Resto país"))
table(base$prov, base$reg)

base$reg <- relevel(as.factor(base$reg), 
                    ref = "Resto país")


#Jefe
base$jefe <- car::recode(base$RELJEFE_49, 
                         "1='1';else='2'")

base$jefe <- factor(base$jefe,
                    levels = 1:2,
                    labels = c("Jefe", 
                               "No jefe"))
table(base$RELJEFE_49, base$jefe)

base$jefe <- relevel(as.factor(base$jefe), 
                     ref = "No jefe")


"------------------------------------------------------------------"









"---------------------------------------------------------------------------------"
"---------------------------------------------------------------------------------"

#Análisis de correspondencias

tab1 <- table(cluster3, wide_cov$edad)
tab2 <- table(cluster3, wide_cov$SEXO_49)
tab3 <- table(cluster3, wide_cov$ec)
tab4 <- table(cluster3, wide_cov$educa)
tab5 <- table(cluster3, wide_cov$etnia)
tab6 <- table(cluster3, turb.group)

tab1 <- table(turb.group, wide_cov$edad)
tab2 <- table(turb.group, wide_cov$SEXO_49)
tab3 <- table(turb.group, wide_cov$ec)
tab4 <- table(turb.group, wide_cov$educa)
tab5 <- table(turb.group, wide_cov$etnia)
tab6 <- table(turb.group, cluster3)

tab <- cbind(tab1, tab2)

#haireye.ca <- ca(tab)
#plot(haireye.ca)

#Con FactorMiner
res.ca <- CA(tab, col.sup = 14:16)
res.ca <- CA(tab)
fviz_ca_biplot(res.ca, repel = TRUE, 
               geom.ind = "text", geom.var = "text",
               title = "MCA - Chingón", #habillage = "cluster3", addEllipses = T,
               col.var = "gray") 


"------------------------------------------------------------"
"------------------------------------------------------------"

basef <- base%>%
  select(cluster3, 
         # edad,
         # jefe,
         # ec,
         # educa,
         # ra2, po2, co2,
         ra3, 
         po3, 
         co3,
         turb.group
  )

basef <- base%>%
  select(cluster3, 
         # ra2, po2, co2,
         ra3, po3, co3,
         turb.group
  )




#base$etni, 
#base$turb.group, 
#base$reg, 


#MCA
# res.mca<-MCA(basef, quali.sup = 1, method ="Burt", graph = F)
# res.mca<-MCA(basef, quali.sup = 2:5, method ="Burt", graph = F)
# res.mca<-MCA(basef, quali.sup = 7:9, method ="Burt", graph = F)
 res.mca<-MCA(basef, method ="Burt", graph = F)


# fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)
# 
# fviz_mca_var(res.mca, 
#              repel = TRUE, # Avoid text overlapping (slow)
#              ggtheme = theme_minimal())

fviz_mca_biplot(res.mca, axes = c(1, 2),
                geom.ind = "point", geom.var = c("point","text"), repel = T,
                habillage = 1, addEllipses = T,
                ellipse.level = 0.75, title = "MCA - Chingón",
                col.var ="black", palette=c("yellow2", "lightpink", "limegreen"),
                col.quali.sup = "blue", select.ind = list(cos2 = 0.0),
                alpha.ind="cos2", shape.var = 10, pointsize = 3, ellipse.type = "norm"
                # invisible ="ind"
) + scale_shape_manual(values=c(2, 8, 11))


# To visualize the percentages of inertia explained by each MCA dimensions
fviz_screeplot(res.mca, addlabels = TRUE, ylim = c(0, 32))

# Extract the results for variable categories. 
var <- get_mca_var(res.mca)
var

var$coord
var$contrib
var$cos2

# var$coord: coordinates of variables to create a scatter plot
# var$cos2: represents the quality of the representation for variables on the factor map.
# var$contrib: contains the contributions (in percentage) of the variables to the definition of the dimensions.


# Correlation between variables and principal dimensions
fviz_mca_var(res.mca, choice = "mca.cor", 
             repel = TRUE, # Avoid text overlapping (slow)
             ggtheme = theme_minimal())


# cos2 of row categories on all the dimensions
library("corrplot")
corrplot(var$cos2, is.corr=FALSE)


# Cos2 of variable categories on Dim.1 and Dim.2
fviz_cos2(res.mca, choice = "var", axes = 1:2)


# Contributions of rows to dimension 1
fviz_contrib(res.mca, choice = "var", axes = 1, top = 15)
# Contributions of rows to dimension 2
fviz_contrib(res.mca, choice = "var", axes = 2, top = 15)

# Total contribution to dimension 1 and 2
fviz_contrib(res.mca, choice = "var", axes = 1:2, top = 15)


# The red dashed line on the graph above indicates the expected average value, 
# If the contributions were uniform. The calculation of the expected contribution 
# value, under null hypothesis, has been detailed in the principal component analysis 
# chapter.


# Elipses con dos variables
fviz_ellipses(res.mca, c("cluster3", "sexo"),
              geom = "point")





# 
# "-----------------------------------------------------------------"
# "Alegría"
# 
# levels(basef$wide_cov.SEXO_49) <- c("Hom-", "Muj-")
# levels(basef$wide_cov.estado) <- c("Ca", "So") 
# levels(basef$wide_cov.educa) <- c("Ba", "Me", "Su")
# 
# basef$perfil<-paste0(basef$wide_cov.SEXO_49, basef$wide_cov.edad,  
#                      basef$wide_cov.educa, basef$wide_cov.estado)
# 
# 
# perf<-table(basef$perfil, basef$turb.group)
# perf<-rbind(perf)
# 
# res.ca <- CA(perf, graph = F)
# fviz_ca_biplot(res.ca, repel = TRUE, 
#                geom.ind = "text", geom.var = "text",
#                geom.col = c("arrow", "text"),
#                title = "MCA - Chingón",
#                col.var = "gray") 





"------------------------------------------------------------------"
"Modelo multinomial"
"------------------------------------------------------------------"
#Recodificación de rama de actividad
base$r3 <- car:: recode(unclass(base$ra3), 
                        "1='1';
                          2='6';
                          3='2';
                          4='3';
                          5='4';
                          6='5';
                          7='6';
                          8='7';
                          else='8'")
table(base$r3)
round(prop.table(table(base$r3)), 2)

base$r3 <- factor(unclass(base$r3), levels = 1:8, 
                  labels = c("Agr",
                             "Man",
                             "Con",
                             "Com",
                             "SB",
                             "SF",
                             "SSP",
                             "Nin"))

base$r3 <- relevel(as.factor(base$r3), 
                   ref = "Man")

base$ra3 <- relevel(as.factor(base$ra3), 
                    ref = "Man")


#Categoría de referencia
#base <- data.frame(base, turb.group, cluster3)

#Cluster
base$cluster3 <- unclass(base$cluster3) 
base$cluster3 <- factor(base$cluster3,
                        levels = 1:3,
                        labels = c("T. I I",
                                   "T. F ML",
                                   "T. ML B"))

base$cluster3 <- relevel(as.factor(base$cluster3), 
                         ref = "T. I I")

#Modelo
m <- polr(cluster3 ~ sexo + edad + etnia + educa + ec + reg + turb.group, 
          data = base)
brant(m)


"Sexo"
m2 <- multinom(cluster3 ~ edad + etnia + educa + ec + jefe + 
                 reg + r3,  
               data = base)

m3 <- multinom(cluster3 ~ edad + etnia + educa + ec + jefe + 
                 reg + ra3,  
               data = base)

m4 <- multinom(cluster3 ~ edad + educa + ec +  
                 reg + r3,  
               data = base)

cramer.v(table(base$jefe, base$edad)) #colinealidad


anova(m2, m3)
anova(m3, m4)

x <- round(prop.table(table(base$ra3)), 2)
write.table(x, "clipboard", row.names = F, sep = "\t")

#OR
tr.or <- extract(m4)
tr.or[[1]]@coef <- exp(tr.or[[1]]@coef)
tr.or[[1]]@se <- numeric()
tr.or[[1]]@pvalues <- numeric()
tr.or[[1]]@model.name <- "OR"

tr.co <- extract(m4)
tr.co[[1]]@coef <- tr.co[[1]]@coef

tr.or[[2]]@coef <- exp(tr.or[[2]]@coef)
tr.or[[2]]@se <- numeric()
tr.or[[2]]@pvalues <- numeric()
tr.or[[2]]@model.name <- "OR"

tr.0 <- extract(m4)
tr.0[[2]]@coef <- plogis(tr.0[[2]]@coef)-plogis(tr.0[[2]]@coef)
tr.0[[2]]@se <- numeric()
tr.0[[2]]@pvalues <- numeric()
tr.0[[2]]@model.name <- "OR"


#LaTeX
screenreg(m4, include.log=T, include.bic=T, 
          include.deviance=T, single.row=T, digits = 2, bold = 0.05
          # ci.force = T, ci.force.level = 0.95
)

screenreg(list(tr.0[[2]], tr.0[[2]], tr.co[[1]], tr.or[[1]], tr.0[[2]], tr.co[[2]], tr.or[[2]]), 
          custom.model.names = c("Pr", "Pr", "T. F ML", "OR", "Pr", "T. ML B", "OR"),
          bold = 0.05, include.log=T, include.bic=F, include.deviance=T,
          dcolumn = T, single.row=T, #ci.force = T, ci.force.level = 0.95
          digits = 2)


anova(m1, m2, m3, m4, test = "Chisq")

plotreg(m5, omit.coef = "(Intercept)|(etnia)")
plotreg(m5, omit.coef = "etnia")
plotreg(m5)

#Resumen 4 modelos
texreg(list(m1, m2, m3, m4), bold = 0.05, include.log=F, 
       include.bic=T, include.deviance=F,
       dcolumn = T)

# Sólo modelo 4
texreg(list(tr.0[[2]], tr.0[[2]], tr.co[[1]], tr.or[[1]], tr.0[[2]], tr.co[[2]], tr.or[[2]]), 
       custom.model.names = c("Pr", "Pr", "T. F ML", "OR", "Pr", "T. ML B", "OR"), 
       caption="Modelo de hombres",
       groups = list(GrupoEdad=2:3, Educación=4:5, 
                     Turbulencia=6:17, Sector=11:14,
                     Ocupación=15:16,
                     SexoEdad=17:18, SexoEduca=19:20, 
                     SexoEstado=21), 
       custom.model.names=c("Tray. Inf. Ines.", "T. ML B. Cond."),
       custom.coef.names=c("Constante", "Mujer (Hombre=0)", "Adulto/a", 
                           "A. Mayor", 
                           "Media", "Superior", "Soltero/a (Cas=0)",
                           "Jefe/a (NoJ=0)",
                           "MediaT", "Alta", 
                           "Primario", "Secundario", "Comercio", "Ninguno",
                           "Autoempleado", "No trabaja",
                           "Mujer-Adulta",
                           "Mujer-Mayor", "Mujer-Ed. Media",
                           "Mujer-Ed. Superior", "Mujer-Soltera"), 
       bold = 0.05, include.log=T, include.bic=F, include.deviance=T,
       dcolumn = T, single.row=T, #ci.force = T, ci.force.level = 0.95
       digits = 2
)

# Todos los modelos
screenreg(list(m1, m2, m3, m4, m5), 
          caption="Tipología de trayectorias laborales",
          groups = list(GrupoEdad=3:4, Etnia=5:6,  
                        Educación=7:8, Provincia=10:11, 
                        Turbulencia=12:13,
                        SexoEdad=14:15, SexoEduca=16:17, 
                        SexoEstado=18), 
          custom.model.names=c("Tray. Inf. Ines.", "T. ML B. Cond."),
          custom.coef.names=c("Constante", "Mujer (Hombre=0)", "Adulto/a", 
                              "A. Mayor", "Indígena-Mont", "Afroecuatoriano", 
                              "Media", "Superior", "Soltero/a (Cas=0)",
                              "Pichincha", "Guayas",
                              "MediaT", "Alta", "Mujer-Adulta",
                              "Mujer-Mayor", "Mujer-Ed. Media",
                              "Mujer-Ed. Superior", "Mujer-Soltera"), 
          custom.header = list("Model 1" = 1:2,
                               "Model 2" = 3:4,
                               "Model 3" = 5:6,
                               "Model 4" = 7:8,
                               "Model 5" = 9:10),
          bold = 0.05, include.log=T, include.bic=F, include.deviance=T,
          dcolumn = T, single.row=T, #ci.force = T, ci.force.level = 0.95
)


#Hosmer and Lemeshow test (multinomial model)
logitgof(base$cluster3, fitted(m4), g=10)
mnlfit(m4)
pR2(m1)


"------------------------------------------------------------"

#Gráficas chingonas

#Efectos marginales 
library(margins)
library(ggeffects)
library(sjPlot)
# install.packages("effects")
library(effects)
#Crea una base de datos del mismo tamaño de la
#original con las predicciones, efectos marginales 
#y varianzas
efectos <- margins(m0j, design=doj) 
summary(efectos)
plot(efectos,las = 2) #OJO: el gráfico está raro, mejor hacerlo en excel

ggeffect(m0j)


"Predicciones"
# ggpredict(m5)
# plot_model(m5, type = "pred",
#            title = "Predicciones") #Genera un gráfico x variable 
# 
# plot_model(m5, type = "pred",
#            title = "Predicciones",
#            terms = c("edad", "turb.group"))


"Interacciones"
# plot_model(m5, type = "int" )


"Efectos - OK"
round(Effect(c("edad"), mod=m4)$prob, 2)
round(Effect(c("educa"), mod=m4)$prob, 2)
round(Effect(c("ec"), mod=m4)$prob, 2)
round(Effect(c("reg"), mod=m4)$prob, 2)
round(Effect(c("r3"), mod=m4)$prob, 2)


# Effect(c("sexo","edad"), mod=m7)
Effect(c("sexo","educa"), mod=m4)
# Effect(c("sexo","ec"), mod=m7)

#allEffects(m7)
#plot(allEffects(m5))
#plot(Effect(c("sexo"), mod=m5))
#plot(Effect(c("edad"), mod=m5))
plot(Effect(c("sexo","edad"), mod=m7), 
     style = "stacked", colors = c("#F0027F", "#FDC086", "#7FC97F"))
plot(Effect(c("sexo", "educa"), mod=m4),  rug = FALSE, 
     style = "stacked", colors = c("#FDC086", "#7FC97F", "#F0027F"))
plot(Effect(c("ec", "sexo"), mod=m7),  rug = FALSE, 
     style = "stacked", colors = c("#F0027F", "#FDC086", "#7FC97F"))
# plot_model(m5, type = "int")


"Momios"
# plot <- plot_model(m0j, type = "est", show.values=T,
#                    title = "Momios del modelo de Empleo Adecuado", value.offset=0.4, value.size=3.5, group.terms = c(1, 2, 3, 3, 4, 4, 5, 5, 6, 7, 7, 7, 7, 7), 
#                    axis.labels=c("Otro", "Monop hij JM", "Monop hij JH", "Bip sin hijos",
#                                  "Unipersonal", "<5 años", "Superior", "Media", "Sep-Div-Viu",
#                                  "Cas-Uni", "Otros", "Indígena", "Rural", "Mujer"
#                    ),
#                    axis.lim=c(0.4, 7)
# )

plot <- plot_model(m5, type = "est", show.values=T,
                   title = "Momios del modelo de Empleo Adecuado", value.offset=0.4, value.size=3.5,
                   axis.lim=c(0.02, 22)
)

plot + scale_color_sjplot("metro", discrete = TRUE, reverse = F)
plot