setwd("~/Tesis/datos/base de datos arreglada")

library(readxl)
library(dplyr)
library(car)
library(ggplot2)
library(rpart)
library(caret)
library(tidyverse)
library(data.table)
library(readr)
#install.packages("tidymodels")
library(tidymodels)
library(reshape2)
library(ggcorrplot)
library(MASS)
library(car)
library(readxl)
library(dplyr)
library(readr)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(pacman)
library(gtable)
library(rlang)
library(dplyr)
library(ggthemr)
pacman::p_load(tidymodels,themis,discrim,tidyposterior,
               corrr,readr,magrittr,stringr,forcats,
               skimr,patchwork,GGally)
# LEER DATOS ----
datos_variables_nuevas <- read_csv("Base_final_arreglada.csv")
datos1<-datos_variables_nuevas



cols=c("Genero","Carrera","rama_educacional","Dependencia","Gratuidad",
       "misma_region","nacionalidad","Etnia","Discapacidad","ingreso1","pandemia","retiro")
datos1[cols] <- lapply(datos1[cols], factor)
library(naniar)




library(gtsummary)
datos1 %>% dplyr::select(Genero,Carrera,rama_educacional,Dependencia,
                         Gratuidad,misma_region,nacionalidad,Etnia,Discapacidad,
                         ingreso1,pandemia,retiro) %>%
  tbl_summary(by=retiro,percent="row") %>% add_p() 

datos1 %>% dplyr::select(año,Puntaje_Seleccion,preferencia,
                         lugar,Año_egreso,demora_ingreso,edad_est,retiro) %>%
  tbl_summary(by=retiro,percent="row") %>% add_p()
datos1 %>% dplyr::select(Nota_Calculo_I,Notas_Algebra,distancia,puntaje,retiro) %>%
  tbl_summary(by=retiro,percent="row") %>% add_p()






variables <- datos1[c(2,4,5:8,10,11,15,16,22:27,29)]
pandemia<- datos1 %>% filter(pandemia == 'Durante pandemia')
pandemia$retiro1<-factor(pandemia$retiro,
                         levels = c(0,1),
                         labels = c("Continua los estudios","Se retira de la universidad"))

pandemia[c("Notas_Algebra","Nota_Calculo_I")][is.na(pandemia[c("Notas_Algebra","Nota_Calculo_I")])] <- 0



library(gtsummary)
pandemia %>% dplyr::select(Genero,Carrera,año,demora_ingreso,
                         ,Puntaje_Seleccion,Nota_Calculo_I,Notas_Algebra,retiro1) %>%
  tbl_summary(by=retiro1,percent="row") %>% add_p() 

pandemia %>% dplyr::select(año,Puntaje_Seleccion,preferencia,
                         lugar,Año_egreso,demora_ingreso,edad_est,retiro1) %>%
  tbl_summary(by=retiro1,percent="row") %>% add_p()
pandemia %>% dplyr::select(Nota_Calculo_I,Notas_Algebra,retiro1) %>%
  tbl_summary(by=retiro1,percent="row") %>% add_p()


ggthemr('fresh')


datos1 %>% group_by(año) %>% summarise(cuenta = length(año))
par(mfrow=c(2, 1))
library(ggpubr)
calculo<-datos1 %>% ggplot (aes(x=Nota_Calculo_I,fill=factor(pandemia)))+
  geom_density(alpha = 0.6) +labs(fill = "Periodo",
                                  title= "Notas Cálculo I por periodo",
                                  x= "Notas Cálculo I",
                                  y = "Densidad")


algebra<-datos1 %>% ggplot (aes(x=Notas_Algebra,fill=factor(pandemia)))+
  geom_density(alpha = 0.6)+labs(fill = "Periodo",
                                 title= "Notas Álgebra por periodo",
                                 x= "Notas Álgebra",
                                 y = "Densidad")

ggarrange(calculo,algebra,
          ncol=1,nrow=2)


datos1 %>% ggplot (aes(x=Puntaje_Seleccion,fill=factor(pandemia)))+
  geom_density(alpha = 0.8)+labs(fill = "Periodo",
                                 title= "Puntajes de ingreso según periodo",
                                 x= "Notas Álgebra",
                                 y = "Densidad")





# puntaje seleccion vs retiro ----
datos1 %>% ggplot (aes(x=Puntaje_Seleccion,fill=factor(retiro)))+
  geom_density(alpha = 0.3)+labs(title= "Grafíco de densidad Puntaje de Selección vs retiro de los estudios",
                                 x= "Puntaje de Selección",
                                 y = "Densidad")+  scale_fill_manual(name = "",values = c('steelblue', 'tomato3'),
                                                                     labels = c( "No se retira","Retira de los estudios"))





ggplot(datos1)+ aes(x = Carrera, y=Nota_Calculo_I)+geom_boxplot()+facet_wrap(~pandemia)+labs(title= "Boxplot por Carrrera del promedio y por periodo",
                                                                                             x= "Puntaje de Selección",
                                                                                             y = "Promedio Cálculo I")



ggplot(datos1)+ aes(x = Carrera, y=Notas_Algebra)+geom_boxplot()+facet_wrap(~pandemia)+labs(title= "Boxplot por Carrrera del promedio y por periodo",
                                                                                            x= "Puntaje de Selección",
                                                                                            y = "Promedio Álgebra")




ggplot(pandemia)+ aes(x = Nota_Calculo_I)+geom_density(alpha = 0.3)+facet_wrap(~Carrera)


ggplot(datos1) +
  aes(x = Carrera, y = Puntaje_Seleccion) +
  geom_boxplot()


ggplot(datos1) +
  aes(x = Carrera, y = Nota_Calculo_I) +
  geom_boxplot()

ggplot(datos1) +
  aes(x = Carrera, y = Notas_Algebra) +
  geom_boxplot() +facet_wrap(~ingreso1)



# fisica que retiraron -----

fisica1 <- filter(datos1,Carrera == 'Física')

fisica1 %>% ggplot (aes(x=Puntaje_Seleccion,fill=factor(retiro)))+
  geom_density(alpha = 0.3)


# el resto que retiran ----



resto <- filter(datos1,Carrera != 'Física')

resto %>% ggplot (aes(x=Puntaje_Seleccion,fill=factor(retiro)))+
  geom_density(alpha = 0.6)+labs(title= "Densidad para el puntaje de selección según estado",
                                 x= "Puntaje de Selección",
                                 y = "Densidad")+
  scale_fill_discrete(name = "Estado",
                       labels = c( "No presenta riesgo académico","Presenta riesgo académico"))+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=17))


resto %>% ggplot (aes(x=pandemia,fill=factor(retiro)))+
    geom_bar()




########  años de retiro -----


library(readxl)
datos_con_desercion <- read_excel("datos_con_desercion.xlsx", 
                                  sheet = "Datos_compleos1")

data <- datos_con_desercion
data$Año_Retiro
data[data == 'S/I'] <- NA

data<-data[!is.na(data$Año_Retiro),]
colnames(data)[1] <- 'Año_matricula'
data$Año_Retiro<-as.numeric(data$Año_Retiro)
data <- data %>% mutate (Años_uv = Año_Retiro - Año_matricula)
data<-mutate(data,Años_uv= Año_Retiro - Año_matricula)
años_en_uv<-as.data.frame(table(data$Años_uv))
años_en_uv<-filter(años_en_uv,Var1 != -1)
años_en_uv<-filter(años_en_uv,Freq >= 8)
aaños <- c("3", 10)
años_en_uv <- rbind(años_en_uv,aaños)
años_en_uv$Freq<-as.numeric(años_en_uv$Freq)
df <- años_en_uv %>%
  mutate(perc = `Freq` / sum(`Freq`)) %>% 
  arrange(perc) %>%
  mutate(labels = scales::percent(perc))
library(ggthemr)
ggthemr('fresh')
ggplot(df, aes(x="", y=Freq, fill=Var1)) +
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start=0)+theme_void()+
  geom_text(aes(label = labels),
            position = position_stack(vjust = 0.5)) +
  scale_fill_discrete(name = "Año de retiro",
                    labels = c( "Primer año","Segundo Año","Tercer año","Cuarto o más"))+
  labs(title= "Gráfico circular del año de retiro de los alumnos ")



