setwd("~/Tesis/datos/base de datos arreglada")
library(readxl)
library(dplyr)
library(car)
library(ggplot2)
library(rpart)
library(caret)
library(tidyverse)
library(data.table)

datos_con_desercion <- read_excel("datos con desercion.xlsx", 
                                  sheet = "Datos_compleos1", col_types = c("numeric", 
                                                                           "numeric", "numeric", "numeric", 
                                                                           "text", "numeric", "numeric", "text", 
                                                                           "text", "text", "text", "text", "text", 
                                                                           "numeric", "text", "numeric", "text", 
                                                                           "text", "text", "text", "numeric", 
                                                                           "numeric", "numeric", "text"))

datos <- datos_con_desercion

datos1<- datos

datos<-mutate(datos,edad_est= trunc(2020-(1930.3+ 3.46* (datos$Rut_AlumnoE%/% 1000000))))
datos[,14] <- sapply(datos[,14],as.numeric)

datos<-datos[datos$Puntaje_Selección>0,]

datos1[datos1 == "S/I"] <- NA
datos1[datos1 == "S.I"]<- NA


# Eliminando los valores perdiods en algebra y cálculo

#datos_completos<- datos1 %>% filter(!is.na(Notas_Algebra),!is.na(Nota_Calculo_I))

#estadistica_pandemia <- datos_completos %>% filter(año > 2019, Carrera == "Matemáticas")
#dim(estadistica_pandemia)

############# LImpieza y preparacion
datos1<-datos

colnames(datos1)<-c('año','rut','Genero','preferencia','Carrera','lugar','Puntaje_Seleccion','comuna',
                    'establecimiento','rama_educacional','Dependencia','Region_colegio','Region','Año_egreso',
                    'Gratuidad','misma_region','nacionalidad','cobertura_salud','Etnia','Discapacidad','Nota_Calculo_I','Notas_Algebra','Año_retiro','Observaciones','edad_est')


datos1$Genero <- as.factor(ifelse(datos1$Genero == 1, 'Hombre', 'Mujer'))

datos1$Carrera <- Recode(datos1$Carrera , "'Ingeniería en Estadística' = 'Estadística' ;
                        'Licenciatura en Ciencias mención Biología o mención Química'= 'Ciencias';
                        'Licenciatura en Física mención Astronomía' = 'Física' ;
                        'Matemática' = 'Matemáticas';
                        'Licenciatura en Matemática' = 'Matemáticas';
                        'Pedagogía en Matemáticas o Licenciatura en Matemáticas' = 'Matemáticas';
                        'Ingeniería en Estadística y Ciencia del Datos' = 'Estadística';
                        'Pedagogía en Matemáticas o Licenciatura en Matemáticas' = 'Matemáticas';
                        'Licenciatura en Física mención Astronomía o mención Ciencias Atmosféricas'='Física';
                        'Licenciatura en Física mención Astronomía o mención Ciencias Atmosféricas o mención Computación Científica'='Física';
                        'Licenciatura en Física mención Astronomía o mención Ciencias Atmosféricas o mención Computación Científica'='Física'")

datos1$Carrera<-as.factor(datos1$Carrera)


datos1$rama_educacional <- Recode(datos1$rama_educacional, "'H1'= 'Científico humanista';
                     'H2'= 'Científico humanista';
                     'H3'= 'Científico humanista';
                     'H4'= 'Científico humanista';
                     'S/I'= 'Científico humanista';
                     'T1'= 'Técnico';
                     'T2'='Técnico';
                     'T3'='Técnico';
                     'T5'='Técnico'")
datos1$rama_educacional <-factor(datos1$rama_educacional)


datos1$Dependencia<-Recode(datos1$Dependencia, "'S/I'= 'Otro';
                           'SLE'= 'Otro';
                           'M'= 'Municipal';
                           'S'= 'Subvencionado';
                           'P'= 'Particular privado'",)
datos1$Dependencia<-factor(datos1$Dependencia,ordered = TRUE,levels = c("Otro","Municipal","Subvencionado","Particular privado"))

#datos1$Region_colegio[datos1$Region_colegio == "S/I"] <- 0

########## AQUI HAY Q BUSCAR EN LA VARIABLE REGION Y IMPUTAR 
########## 


datos1$ingreso1 <- ifelse(datos1$Año_egreso == (datos1$año - 1),"Ingreso inmediato","ingreso tardio") 
datos1$ingreso1<-as.factor(datos1$ingreso1)

datos1<-datos1 %>% mutate(demora_ingreso = año-Año_egreso)

datos1$Gratuidad <- ifelse(datos1$Gratuidad == 'Sin Gratuidad','No','Si')
datos1$Gratuidad<-factor(datos1$Gratuidad,ordered=TRUE)

datos1$misma_region<-as.factor(datos1$misma_region)

datos1$nacionalidad<-as.factor(ifelse(datos1$nacionalidad == 'EXTRANJERA','Extranjero','Chileno(a)'))



datos1$salud1 <- Recode(datos1$cobertura_salud, "'Capredena'='Otro';
                       'Otro'='Otro';
                       'S/I'='Otro';
                       'Fonasa'='Fonasa';
                        'Isapre'='Isapre'")

datos1$salud1<-factor(datos1$salud1,ordered = TRUE,levels= c("Otro","Fonasa","Isapre"))

datos1$Etnia <- Recode(datos1$Etnia, "'Atacameño'='Si';
                     'Aymara'=   'Si';
                     'Colla'=    'Si';
                     'Diaguita'= 'Si';
                     'Mapuche'=  'Si';
                     'S/I'= 'No'")
datos$Etnia<-as.factor(datos1$Etnia)


datos1$Discapacidad <- Recode(datos1$Discapacidad, "'Sí'='Si';
                     'No'=   'No';
                     'S/I'=    'No'")
datos1$Discapacidad<- as.factor(datos1$Discapacidad)

#datos1$demora_ingreso[datos1$demora_ingreso == NA] <- 1

# quitando los datos del 2022 ----
datos1<-filter(datos1,año != 2022)

#datos1$desercion <- ifelse(!is.na(datos1$Nota_Calculo_I) & !is.na(datos1$Notas_Algebra), "No", "Si")
#datos1$desercion <- factor(datos1$desercion, ordered =T, levels=c("No","Si"))





# 
# labels<-c(año= "Año Matrícula",
#           rut= "Rut",
#           edad_est="Edad estimada",
#           preferencia="Preferencia de postulación",
#           genero="Género Alumno",
#           Carrera="Carrera",
#           rama="Rama educacional",
#           Dependencia1="Dependencia",
#           lugar="Puesto de ingreso",
#           Puntaje_Seleccion= "Puntaje de selección",
#           comuna= "Comuna recinto educacional",
#           establecimiento="Nombre recinto educacional",
#           Region_colegio="Región del recinto",
#           Region="Region",
#           Año_egreso="Año egreso enseñanza media",
#           Gratuidad1="¿Alumno con gratuidad?",
#           nacionalidad1="Nacionalidad alumno",
#           etnia1="Alumno perteneces a pueblo originario",
#           discapacidad1="Alumno presenta discapacidad",
#           desercion="Alumno abandona estudios",
#           Nota_Calculo_I="Promedio en cálculo I",
#           Notas_Algebra="Promedio en álgebra",
#           ingreso1="Alumno ingresa a la carrera después de salir de la enseñanza media",
#           demora_ingreso="Años pasados desde el egreso de enseñanza media al ingreso a la carrera")
# label(datos1) = as.list(labels[match(names(datos1),names(labels))])
# label(datos1)
datos1$pandemia <- ifelse(datos1$año < 2020,"Previo pandemia","Durante pandemia")
datos1$pandemia <- factor(datos1$pandemia, levels=c("Previo pandemia","Durante pandemia"))




datos1[datos1 == "S/I"] <- NA

apply(X = is.na(datos1), MARGIN = 2, FUN = sum)
 


library(naniar)


vis_miss(datos1)


drop <- c("Observaciones","cobertura_salud")

datos1 = datos1[,!(names(datos1) %in% drop)]

sapply(datos1, function(x) sum(is.na(x)))

vis_miss(datos1)


columnas<-c("comuna","Region","Dependencia")
new_DF <- datos1[rowSums(is.na(datos1)) > 0,]

new_DF <- datos1[is.na(datos1$comuna),]
View(new_DF)



datos1$comuna[is.na(datos1$comuna)] <- "Valparaíso"



######### Variable respuesta ###############
datos1<-data.frame(datos1)

datos1$retiro<-ifelse(!is.na(datos1$Año_retiro),1,
                       ifelse(
(is.na(datos1$Notas_Algebra)| is.na(datos1$Nota_Calculo_I)) & datos1$año <= 2019, 1,
ifelse(
datos1$año > 2019 & (is.na(datos1$Nota_Calculo_I)  & is.na(datos1$Notas_Algebra)),
    1,0
  )))



View(datos1)
table(datos1$retiro)



datos1$retiro <- factor(datos1$retiro)

drop <- c("salud1","retiro1")

datos1 = datos1[,!(names(datos1) %in% drop)]


write.csv(x = datos1, file = "base_retiro_arreglado.csv", row.names = FALSE) 

