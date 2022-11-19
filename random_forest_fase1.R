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
#library(naniar)


#vis_miss(datos1)



variables <- datos1[c(2,4,5:8,10,11,15,16,22:27,29)]

fisica <-filter(variables, Carrera == "Física")
fcuv <- filter(variables, Carrera != "Física")
drop <- c("Carrera")
fisica = fisica[,!(names(fisica) %in% drop)]
fcuv = fcuv[,!(names(fcuv) %in% drop)]


# train test ----


fisica<-fisica %>% dplyr::select(Genero,preferencia,edad_est,ingreso1,pandemia,retiro)

fisica <- fisica %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))

fcuv<-fcuv %>% dplyr::select(Genero,preferencia,lugar,misma_region,edad_est,pandemia,Puntaje_Seleccion,retiro)
  
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



independent_var <- names(train_fisica)[1:5]
dependent_var <- names(train_fisica)[6]
# Modelo para fisica -----
# 01 Baseline Model -------------------------------------------------------
#Baseline Model with default parameter

library(h2o)
h2o.init()
baseline_model <- h2o.randomForest(
  x = independent_var, #vector containing predictor variables 
  y = dependent_var, #vector contaning response variable
  #training data frame
  mtries = 2, #default value = sqrt(p), p is number of predictors
  sample_rate = 0.6320000291, #Row sample rate per tree
  min_rows = 1, #Fewest allowed (weighted) observations in a leaf
  max_depth = 20, #Maximum tree depth
  ntrees = 50, #Number of trees
  seed = 10,
  verbose = F,
  training_frame = as.h2o(train_fisica),
  validation_frame = as.h2o(test_fisica),
  nfolds = 0)
h2o.performance(baseline_model,as.h2o(test_fisica[6]))
predict_class<-h2o.predict(baseline_model,as.h2o(test_fisica)) 
predict_class<-as.data.frame(predict_class)
confusionMatrix(predict_class, test_fisica$retiro)
plot(h2o.performance(baseline_model),valid=T,type="roc")





# Hyper grid search fisica -----
hyper_grid.h2o <- list(ntrees= c(50,150,500,1000),
                       mtries= 2:4,
                       sample_rate= c(0.50,0.6,0.632,0.65),
                       max_depth = c(10,20,30),
                       min_rows= seq(1,3, by=1)
                       )

sapply(hyper_grid.h2o,length) %>% prod()
independent_var <- names(train_fisica)[1:5]
dependent_var <- names(train_fisica)[6]

system.time(grid_cartesian <- h2o.grid(algorithm = "randomForest",
                                       x=independent_var,
                                       y=dependent_var,
                                       grid_id = "test_grid_rf",
                                       seed=1234,
                                       nfolds= 0,
                                       training_frame = as.h2o(train_fisica),
                                       validation_frame = as.h2o(test_fisica),
                                       hyper_params = hyper_grid.h2o))
rf_grid_perf<- h2o.getGrid(grid_id = "test_grid_rf",
                           sort_by = "logloss",
                           decreasing = F)
rf_grid_perf
best_rf_fisica <- h2o.getModel(rf_grid_perf@model_ids[[1]])
best_rf_perf <- h2o.performance(model= best_rf_fisica,
                                newdata = as.h2o(test_fisica))
best_rf_perf
predict_class_fisica<-h2o.predict(best_rf_fisica,as.h2o(test_fisica)) 
predict_class_fisica<-as.data.frame(predict_class_fisica)
confusionMatrix(predict_class_fisica$predict, test_fisica$retiro)


model_path <- h2o.saveModel(object = best_rf_fisica, path = getwd(), force = TRUE)
model_path


# guardar modelo ----
saved_model <- h2o.loadModel(model_path)
predict_class_fisica<-h2o.predict(saved_model,as.h2o(test_fisica)) 
predict_class_fisica<-as.data.frame(predict_class_fisica)
confusionMatrix(predict_class_fisica$predict, test_fisica$retiro)


# modelo para el resto ----

independent_var <- names(train_fcuv)[1:7]
dependent_var <- names(test_fcuv)[8]


hyper_grid.h2o <- list(ntrees= c(50,150,500,1000),
                       mtries= 2:7,
                       sample_rate= c(0.50,0.6,0.632,0.65),
                       max_depth = c(10,20,30),
                       min_rows= seq(1,3, by=1)
)

sapply(hyper_grid.h2o,length) %>% prod()

system.time(grid_cartesian_fcuv <- h2o.grid(algorithm = "randomForest",
                                       x=independent_var,
                                       y=dependent_var,
                                       grid_id = "test_grid_rf_fcuv",
                                       seed=1234,
                                       nfolds= 0,
                                       training_frame = as.h2o(train_fcuv),
                                       validation_frame = as.h2o(test_fcuv),
                                       hyper_params = hyper_grid.h2o))
rf_grid_perf<- h2o.getGrid(grid_id = "test_grid_rf_fcuv",
                           sort_by = "logloss",
                           decreasing = F)
rf_grid_perf
best_rf_fcuv <- h2o.getModel(rf_grid_perf@model_ids[[1]])
best_rf_perf <- h2o.performance(model= best_rf_fisica,
                                newdata = as.h2o(test_fcuv))
best_rf_perf
predict_class_fisica<-h2o.predict(best_rf_fisica,as.h2o(test_fcuv)) 
predict_class_fisica<-as.data.frame(predict_class_fisica)
confusionMatrix(predict_class_fisica$predict, test_fcuv$retiro)


model_path <- h2o.saveModel(object = best_rf_fcuv, path = getwd(), force = TRUE)
model_path

