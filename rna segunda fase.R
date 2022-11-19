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
# 01 Baseline Model -------------------------------------------------------
#Baseline Model with default parameter
library(h2o)
h2o.init()




# grid search para fisica ----

independent_var_fisica <- names(train_fisica)[1:8]
dependent_var_fisica <- names(train_fisica)[9]
hyper_params <- list(
  activation = c("Rectifier", "Tanh","TanhWithDropout"), 
  hidden = list(c(5), c(15), c(50, 50, 50), c(100, 100, 100)),
  epochs = c(50, 100),
  rate = c(0, 01, 0.005, 0.001),
  rate_annealing = c(1e-8, 1e-6),
  input_dropout_ratio = c(0, 0.1, 0.2)
)
sapply(hyper_params,length) %>% prod()


dl_grid_fisica <- h2o.grid(algorithm = "deeplearning", 
                    x = independent_var_fisica,
                    y = dependent_var_fisica,
                    grid_id = "dl_fisica_fase2",
                    training_frame = as.h2o(train_fisica),
                    validation_frame = as.h2o(test_fisica),
                    nfolds = 0,                    
                    hyper_params = hyper_params,
                    seed = 42
)
gbm_gridperf1 <- h2o.getGrid(grid_id = "dl_fisica_fase2",
                             sort_by = "auc",
                             decreasing = T)

best_rna_fisica <- h2o.getModel(gbm_gridperf1@model_ids[[1]])
best_gbm_perf1 <- h2o.performance(model = best_rna_fisica,
                                  newdata= as.h2o(test_fisica))

h2o.auc(best_gbm_perf1)
plot(h2o.performance(best_rna_fisica,
                     newdata= as.h2o(test_fisica)),valid=T,type='roc')
predict_class<-h2o.predict(best_rna_fisica,as.h2o(test_fisica)) 
predict_class<-as.data.frame(predict_class)
confusionMatrix(predict_class$predict, test_fisica$retiro)

h2o.saveModel(object = best_rna_fisica, path = getwd(), force = TRUE)
#h2o.saveGrid(grid_directory = getwd(), grid_id = "dl_grid_fisica")


# grid search para el resto de las carreras ----

independent_var <- names(train_fcuv)[1:4]
dependent_var <- names(test_fcuv)[5]


dl_grid_fcuv <- h2o.grid(algorithm = "deeplearning", 
                    x = independent_var,
                    y = dependent_var,
                    grid_id = "dl_fcuv_fase2",
                    training_frame = as.h2o(train_fcuv),
                    validation_frame = as.h2o(test_fcuv),
                    nfolds = 0,                    
                    hyper_params = hyper_params,
                    seed = 42
)
ann_fcuv_grid <- h2o.getGrid(grid_id = "dl_fcuv_fase2",
                             sort_by = "auc",
                             decreasing = T)
best_rna_fcuv <- h2o.getModel(ann_fcuv_grid@model_ids[[1]])
best_rf_fcuv_perf <- h2o.performance(model = best_rna_fcuv,
                                  newdata= as.h2o(test_fcuv))
h2o.auc(best_rf_fcuv_perf)
plot(h2o.performance(best_rna_fcuv,
                     newdata= as.h2o(test_fcuv)),valid=T,type='roc')
predict_class_fcuv<-h2o.predict(best_rna_fcuv,as.h2o(test_fcuv)) 
predict_class_fcuv<-as.data.frame(predict_class_fcuv)
confusionMatrix(predict_class_fcuv$predict, test_fcuv$retiro)

h2o.saveModel(object = best_rna_fcuv, path = getwd(), force = TRUE)
#setwd("~/Tesis/datos/base de datos arreglada/grids")
#h2o.saveGrid(grid_directory = getwd(), grid_id = "dl_grid_fcuv")
