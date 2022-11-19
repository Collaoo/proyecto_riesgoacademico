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
library(xtable)
pacman::p_load(tidymodels,themis,discrim,tidyposterior,
               corrr,readr,magrittr,stringr,forcats,
               skimr,patchwork,GGally)
# LEER DATOS ----
datos_variables_nuevas <- read_csv("Base_final_arreglada.csv")
datos1<-datos_variables_nuevas



cols=c("Genero","Carrera","rama_educacional","Dependencia","Gratuidad",
       "misma_region","nacionalidad","Etnia","Discapacidad","ingreso1","pandemia","retiro")
datos1[cols] <- lapply(datos1[cols], factor)


#vis_miss(datos1)



variables <- datos1[c(2,4,5:8,10,11,15,16,22:27,29,20,21)]
variables$promedio <- rowMeans (variables [, c (18,19)], na.rm = T )

años <- c("2020,2021")
pandemia <- filter(variables,año == "2020" | año == "2021")
pandemia$promedio <- rowMeans (pandemia [, c (18,19)], na.rm = TRUE )


drop <- c("Nota_Calculo_I","Notas_Algebra","pandemia")

pandemia = pandemia[,!(names(pandemia) %in% drop)]


pandemia[c("promedio")][is.na(pandemia[c("promedio")])] <- 1

pandemia <- pandemia %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))
pandemia<-pandemia %>% dplyr::select(Carrera,misma_region,edad_est,demora_ingreso,distancia,promedio,retiro)


# train test ----
col = c('lugar')
datos1[cols] <- lapply(datos1[cols], factor)



pandemia<-as.data.frame(pandemia)
set.seed(15)
split<- initial_split(pandemia,prop=0.60)
train_pandemia <- training(split)
test_pandemia <- testing(split)



# Modelo para fisica -----
# 01 Baseline Model -------------------------------------------------------
#Baseline Model with default parameter
library(h2o)
h2o.init(min_mem_size='3g',max_mem_size='10G')





# grid search para fisica ----

independent_var <- names(train_pandemia)[1:6]
dependent_var <- names(train_pandemia)[7]
# Hyper grid search fisica -----
hyper_params <- list(
  activation = c("Rectifier", "Tanh"), 
  hidden = list(c(5), c(15), c(50, 50, 50), c(100, 100, 100)),
  epochs = c(50, 100),
  rate = c(0, 01, 0.005, 0.001),
  rate_annealing = c(1e-8, 1e-6),
  input_dropout_ratio = c(0, 0.1, 0.2)
)

sapply(hyper_params,length) %>% prod()

system.time(grid_cartesian <- h2o.grid(algorithm = "deeplearning",
                                       x=independent_var,
                                       y=dependent_var,
                                       grid_id = "dl_fase3",
                                       seed=1234,
                                       nfolds= 3,
                                       training_frame = as.h2o(train_pandemia),
                                       validation_frame = as.h2o(test_pandemia),
                                       hyper_params = hyper_params))
dl_grid_perf<- h2o.getGrid(grid_id = "dl_fase3",
                           sort_by = "logloss",
                           decreasing = F)
best_rna <- h2o.getModel(dl_grid_perf@model_ids[[1]])
best_rf_perf <- h2o.performance(model= best_rna,
                                newdata = as.h2o(test_pandemia))
predict_class_pandemia<-h2o.predict(best_rna,as.h2o(test_pandemia)) 
predict_class_pandemia<-as.data.frame(predict_class_pandemia)
confusionMatrix(predict_class_pandemia$predict, test_pandemia$retiro)
plot(h2o.performance(best_rna,
                     newdata= as.h2o(test_pandemia)),valid=T,type='roc')

model_path <- h2o.saveModel(object = best_rna, path = getwd(), force = TRUE)
model_path


# guardar modelo ----
saved_model <- h2o.loadModel(model_path)
predict_class_fisica<-h2o.predict(saved_model,as.h2o(test_pandemia)) 
predict_class_fisica<-as.data.frame(predict_class_fisica)
confusionMatrix(predict_class_fisica$predict, test_pandemia$retiro)
