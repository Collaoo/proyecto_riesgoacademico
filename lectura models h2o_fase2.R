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
pacman::p_load(tidymodels,themis,discrim,tidyposterior,
               corrr,readr,magrittr,stringr,forcats,
               skimr,patchwork,GGally)
# LEER DATOS ----
datos_variables_nuevas <- read_csv("Base_final_arreglada.csv")
datos1<-datos_variables_nuevas



cols=c("Genero","Carrera","rama_educacional","Dependencia","Gratuidad",
       "misma_region","nacionalidad","Etnia","Discapacidad","ingreso1","pandemia","retiro")
datos1[cols] <- lapply(datos1[cols], factor)
# library(naniar)


#vis_miss(datos1)



variables <- datos1[c(2,4,5:8,10,11,15,16,22:27,29,20,21)]
# variables<-variables %>%
#   mutate_if(is.numeric, scale)
fisica <-filter(variables, Carrera == "Física")
fcuv <- filter(variables, Carrera != "Física")
drop <- c("Carrera")
fisica = fisica[,!(names(fisica) %in% drop)]
fcuv = fcuv[,!(names(fcuv) %in% drop)]
fisica$promedio <- rowMeans (fisica [, c (17,18)], na.rm = TRUE )
fcuv$promedio <- rowMeans (fcuv [, c (17,18)], na.rm = TRUE )
fisica[c("promedio")][is.na(fisica[c("promedio")])] <- 1
fcuv[c("promedio")][is.na(fcuv[c("promedio")])] <- 1

drop <- c("Nota_Calculo_I","Notas_Algebra")

fisica = fisica[,!(names(fisica) %in% drop)]
fcuv = fcuv[,!(names(fcuv) %in% drop)]



# train test ----


fisica<-fisica %>% dplyr::select(Genero,lugar,Puntaje_Seleccion,rama_educacional,Dependencia,
                                 Gratuidad,ingreso1,cociente,promedio,retiro)

fisica <- fisica %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))

fcuv<-fcuv %>% dplyr::select(lugar,Puntaje_Seleccion,Gratuidad,demora_ingreso,promedio,retiro)

fcuv <- fcuv %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))


fisica<-data.frame(fisica)
fcuv<-data.frame(fcuv)

set.seed(15)
split<- initial_split(fisica,prop = 4/7)
train_fisica <- training(split)
test_fisica <- testing(split)


split<- initial_split(fcuv,prop = 4/7)
train_fcuv <- training(split)
test_fcuv <- testing(split)



# Modelo para fisica -----
# leer modelo rf para fisica -----
library(h2o)
h2o.init()


rf_fisica <- h2o.loadModel("C:\\Users\\Usuario\\Documents\\Tesis\\datos\\base de datos arreglada\\rf_fisica_fase2_model_185")

predict_class<-h2o.predict(rf_fisica,as.h2o(test_fisica)) 
predict_class<-as.data.frame(predict_class)
confusionMatrix(predict_class$predict, test_fisica$retiro)
plot(h2o.performance(rf_fisica,
                     newdata= as.h2o(test_fisica)),valid=T,type='roc')

perf_rf_fisica <- h2o.performance(rf_fisica,as.h2o(test_fisica))
roc_rf_fisica<-as.data.frame(perf_rf_fisica@metrics$thresholds_and_metric_scores[c('tpr','fpr')])
roc_rf_fisica <- data.frame(modelo=rep("random_forest",117),roc_rf_fisica)


# leer modelo rna para fisica ----

rna_fisica <- h2o.loadModel("C:\\Users\\Usuario\\Documents\\Tesis\\datos\\base de datos arreglada\\dl_fisica_fase2_model_309")

predict_class<-h2o.predict(rna_fisica,as.h2o(test_fisica)) 
predict_class<-as.data.frame(predict_class)
confusionMatrix(predict_class$predict, test_fisica$retiro)
plot(h2o.performance(rna_fisica,
                     newdata= as.h2o(test_fisica)),valid=T,type='roc')
perf_rna_fisica <- h2o.performance(rna_fisica,as.h2o(test_fisica))
roc_rna_fisica<-as.data.frame(perf_rna_fisica@metrics$thresholds_and_metric_scores[c('tpr','fpr')])
roc_rna_fisica <- data.frame(modelo=rep("rna",119),roc_rna_fisica)





# leer modelo rf para el resto ----

rf_fcuv <- h2o.loadModel("C:/Users/Usuario/Documents/Tesis/datos/base de datos arreglada/rf_fcuv_fase2_model_16")

predict_class<-h2o.predict(rf_fcuv,as.h2o(test_fcuv)) 
predict_class<-as.data.frame(predict_class)
confusionMatrix(predict_class$predict, test_fcuv$retiro)
plot(h2o.performance(rf_fcuv,
                     newdata= as.h2o(test_fcuv)),valid=T,type='roc')

perf_rf_fcuv <- h2o.performance(rf_fcuv,as.h2o(test_fcuv))
roc_rf_fcuv<-as.data.frame(perf_rf_fcuv@metrics$thresholds_and_metric_scores[c('tpr','fpr')])
roc_rf_fcuv <- data.frame(modelo=rep("random_forest",148),roc_rf_fcuv)




# leer modelo rna para el resto -----
rna_fcuv <- h2o.loadModel("C:\\Users\\Usuario\\Documents\\Tesis\\datos\\base de datos arreglada\\dl_fcuv_fase2_model_19")

predict_class<-h2o.predict(rna_fcuv,as.h2o(test_fcuv)) 
predict_class<-as.data.frame(predict_class)
confusionMatrix(predict_class$predict, test_fcuv$retiro)
plot(h2o.performance(rna_fcuv,
                     newdata= as.h2o(test_fcuv)),valid=T,type='roc')
perf_rna_fcuv <- h2o.performance(rna_fcuv,as.h2o(test_fcuv))
roc_rna_fcuv<-as.data.frame(perf_rna_fcuv@metrics$thresholds_and_metric_scores[c('tpr','fpr')])
roc_rna_fcuv <- data.frame(modelo=rep("rna",155),roc_rna_fcuv)




# roc para fisica -----
roc_curves<- rbind(roc_rf_fisica,roc_logistic,roc_rna_fisica,roc_svm)
library(ggthemr)
ggthemr('fresh')
ggplot(roc_curves,aes(fpr,tpr))+geom_line(aes(color=modelo),size=1.5)+ geom_abline(slope = 1)+
  xlab('Tasa de Falsos positivos')+
  ylab('Tasa de verdaderos positivos')+
  ggtitle('Curva ROC para los modelos de la fase 2 para física')+
  scale_color_discrete(name= "Modelo",
                       labels=c("Modelo logístico","Random forest",
                                "Red neuronal artificial","Máquina de vectores de soporte"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  theme(legend.text = element_text(size=15))+ theme(legend.title = element_text(size=20))+ 
  theme(plot.title = element_text(size=25))











# curva roc para fcuv ----
roc_curves<- rbind(roc_rf_fcuv,roc_logistic,roc_rna_fcuv,roc_svm)

ggplot(roc_curves,aes(fpr,tpr))+geom_line(aes(color=modelo),size=1.5)+ geom_abline(slope = 1)+
  xlab('Tasa de Falsos positivos')+
  ylab('Tasa de verdaderos positivos')+
  ggtitle('Curva ROC para los modelos de la fase 2 para Cluster 2')+
  scale_color_discrete(name= "Modelo",
                       labels=c("Modelo logístico","Random forest",
                                "Red neuronal artificial","Máquina de vectores de soporte"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))+ 
  theme(legend.text = element_text(size=15))+ theme(legend.title = element_text(size=20))+ 
  theme(plot.title = element_text(size=25))

