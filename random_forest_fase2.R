setwd("~/Documents/tesis base arreglada")
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
# 01 Baseline Model -------------------------------------------------------
#Baseline Model with default parameter
library(h2o)
h2o.init(min_mem_size='3g',max_mem_size='10G')





# grid search para fisica ----

independent_var_fisica <- names(train_fisica)[1:9]
dependent_var_fisica <- names(train_fisica)[10]
# Hyper grid search fisica -----
hyper_grid.h2o <- list(ntrees= c(50,250,500,1000),
                       mtries= 2:8,
                       sample_rate= c(0.55,0.632,0.65),
                       max_depth = c(10,20),
                       min_rows= seq(1,3, by=1)
                       )

sapply(hyper_grid.h2o,length) %>% prod()

system.time(grid_cartesian <- h2o.grid(algorithm = "randomForest",
                                       x=independent_var_fisica,
                                       y=dependent_var_fisica,
                                       grid_id = "rf_fisica_fase2",
                                       seed=1234,
                                       nfolds= 0,
                                       training_frame = as.h2o(train_fisica),
                                       validation_frame = as.h2o(test_fisica),
                                       hyper_params = hyper_grid.h2o))
rf_grid_perf<- h2o.getGrid(grid_id = "rf_fisica_fase2",
                           sort_by = "logloss",
                           decreasing = F)
best_rf_fisica <- h2o.getModel(rf_grid_perf@model_ids[[1]])
best_rf_perf <- h2o.performance(model= best_rf_fisica,
                                newdata = as.h2o(test_fisica))
predict_class_fisica<-h2o.predict(best_rf_fisica,as.h2o(test_fisica)) 
predict_class_fisica<-as.data.frame(predict_class_fisica)
confusionMatrix(predict_class_fisica$predict, test_fisica$retiro)
plot(h2o.performance(best_rf_fisica,
                     newdata= as.h2o(test_fisica)),valid=T,type='roc')

model_path <- h2o.saveModel(object = best_rf_fisica, path = getwd(), force = TRUE)
model_path


# guardar modelo ----
saved_model <- h2o.loadModel(model_path)
predict_class_fisica<-h2o.predict(saved_model,as.h2o(test_fisica)) 
predict_class_fisica<-as.data.frame(predict_class_fisica)
confusionMatrix(predict_class_fisica$predict, test_fisica$retiro)


# modelo para el resto ----

independent_var <- names(train_fcuv)[1:5]
dependent_var <- names(test_fcuv)[6]


hyper_grid.h2o <- list(ntrees= c(50,150,500,1000),
                       mtries= 2:4,
                       sample_rate= c(0.55,0.6,0.632,0.65),
                       max_depth = c(5,10,20),
                       min_rows= seq(1,3, by=1)
)

sapply(hyper_grid.h2o,length) %>% prod()
system.time(grid_cartesian_fcuv <- h2o.grid(algorithm = "randomForest",
                                       x=independent_var,
                                       y=dependent_var,
                                       grid_id = "rf_fcuv_fase2",
                                       seed=1234,
                                       nfolds= 0,
                                       training_frame = as.h2o(train_fcuv),
                                       validation_frame = as.h2o(test_fcuv),
                                       hyper_params = hyper_grid.h2o))
rf_grid_perf<- h2o.getGrid(grid_id = "rf_fcuv_fase2",
                           sort_by = "auc",
                           decreasing = T)
rf_grid_perf
best_rf_fcuv <- h2o.getModel(rf_grid_perf@model_ids[[1]])
best_rf_perf <- h2o.performance(model= best_rf_fcuv,
                                newdata = as.h2o(test_fcuv))
predict_class_fcuv<-h2o.predict(best_rf_fcuv,as.h2o(test_fcuv)) 
predict_class_fcuv<-as.data.frame(predict_class_fcuv)
confusionMatrix(predict_class_fcuv$predict, test_fcuv$retiro)
plot(h2o.performance(best_rf_fcuv,
                     newdata= as.h2o(test_fisica)),valid=T,type='roc')


model_path <- h2o.saveModel(object = best_rf_fcuv, path = getwd(), force = TRUE)
model_path

