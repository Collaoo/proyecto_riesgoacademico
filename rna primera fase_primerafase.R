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



variables <- datos1[c(2,4,5:8,10,11,15,16,22:27,29)]

fisica <-filter(variables, Carrera == "Física")
fcuv <- filter(variables, Carrera != "Física")
drop <- c("Carrera")
fisica = fisica[,!(names(fisica) %in% drop)]
fcuv = fcuv[,!(names(fcuv) %in% drop)]


# train test ----


fisica<-fisica %>% dplyr::select(Genero,preferencia,edad_est,ingreso1,pandemia,retiro)

fisica <- fisica %>% mutate_if(is.numeric, ~(scale(.) %>% as.vector))

fcuv<-fcuv %>% dplyr::select(Genero,preferencia,misma_region,edad_est,pandemia,Puntaje_Seleccion,retiro)

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


# Probar con tangente hiperbolica, logistica, y no probar con relu. 
# modelo para el resto -----

independent_var <- names(train_fcuv)[1:7]
dependent_var <- names(test_fcuv)[8]



deepmodel <- h2o.deeplearning(x = independent_var      
                              ,y = dependent_var      
                              ,training_frame = as.h2o(train_fcuv)      
                              ,validation_frame = as.h2o(test_fcuv)         
                              ,standardize = T        
                              ,model_id = "deep_model"        
                              ,activation = "Rectifier"       
                              ,epochs = 100       
                              ,seed = 15       
                              ,hidden = 15         
                              ,variable_importances = T)
h2o.varimp_plot(deepmodel)
h2o.confusionMatrix(deepmodel)
h2o.performance(deepmodel)
plot(h2o.performance(deepmodel),valid=T,type='roc')

predict_class<-h2o.predict(deepmodel,as.h2o(test_fcuv)) 
confusionMatrix(predict_class, test_fcuv$retiro)
predict_class<-as.data.frame(predict_class)


k = 0
accuracy = c()
sensitivity = c()
specificity = c()
recall=c()
for(i in seq(from = 0.1 , to = 0.5 , by = 0.01)){
  k = k + 1
  preds_binomial = ifelse(predict_class$p1 > i , 1 , 0)
  confmat = table(test_fcuv$retiro , preds_binomial)
  accuracy[k] = sum(diag(confmat)) / sum(confmat)
  sensitivity[k] = confmat[1 , 1] / sum(confmat[ , 1])
  specificity[k] = confmat[2 , 2] / sum(confmat[ , 2])
  recall[k] = confmat[2 , 2] / sum(confmat[ 2,])
  
}


threshold=seq(from = 0.1 , to = 0.5 , by = 0.01)
data = data.frame(threshold , accuracy , sensitivity , specificity,recall)
ggplot(gather(data , key = 'Metric' , value = 'Value' , 2:5) , 
       aes(x = threshold , y = Value , color = Metric)) + 
  geom_line(size = 1.5)



preds.for.50 = ifelse(predict_class$p1 > 0.38 , 1 , 0)
confusion = table(Predicted = preds.for.50 , Actual = test_fcuv$retiro)
confusion
caret::confusionMatrix(confusion)






# grid search para fisica ----

independent_var_fisica <- names(train_fisica)[1:5]
dependent_var_fisica <- names(train_fisica)[6]
hyper_params <- list(
  activation = c("Rectifier", "Tanh"), 
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
                    grid_id = "dl_grid_fisica",
                    training_frame = as.h2o(train_fisica),
                    validation_frame = as.h2o(test_fisica),
                    nfolds = 0,                    
                    hyper_params = hyper_params,
                    seed = 42
)
gbm_gridperf1 <- h2o.getGrid(grid_id = "dl_grid_fisica",
                             sort_by = "auc",
                             decreasing = TRUE)
print(gbm_gridperf1)
best_rna_fisica <- h2o.getModel(gbm_gridperf1@model_ids[[1]])
best_gbm_perf1 <- h2o.performance(model = best_rna_fisica,
                                  newdata= as.h2o(test_fisica))
h2o.auc(best_gbm_perf1)
best_gbm_perf1
predict_class<-h2o.predict(best_rna_fisica,as.h2o(test_fisica)) 
predict_class<-as.data.frame(predict_class)
confusionMatrix(predict_class$predict, test_fisica$retiro)

h2o.saveModel(object = best_rna_fisica, path = getwd(), force = TRUE)
h2o.saveGrid(grid_directory = getwd(), grid_id = "dl_grid_fisica")


# grid search para el resto de las carreras ----

independent_var <- names(train_fcuv)[1:6]
dependent_var <- names(test_fcuv)[7]


dl_grid_fcuv <- h2o.grid(algorithm = "deeplearning", 
                    x = independent_var,
                    y = dependent_var,
                    grid_id = "dl_fcuv_fase1",
                    training_frame = as.h2o(train_fcuv),
                    validation_frame = as.h2o(test_fcuv),
                    nfolds = 0,                    
                    hyper_params = hyper_params,
                    seed = 42
)
ann_fcuv_grid <- h2o.getGrid(grid_id = "dl_fcuv_fase1",
                             sort_by = "logloss",
                             decreasing = F)
print(ann_fcuv_grid)
best_rna_fcuv <- h2o.getModel(ann_fcuv_grid@model_ids[[1]])
best_rf_fcuv_perf <- h2o.performance(model = best_rna_fcuv,
                                  newdata= as.h2o(test_fcuv))
h2o.auc(best_rf_fcuv_perf)
best_rf_fcuv_perf
predict_class_fcuv<-h2o.predict(best_rna_fcuv,as.h2o(test_fcuv)) 
predict_class_fcuv<-as.data.frame(predict_class_fcuv)
confusionMatrix(predict_class_fcuv$predict, test_fcuv$retiro)

h2o.saveModel(object = best_rna_fcuv, path = getwd(), force = TRUE)
setwd("~/Tesis/datos/base de datos arreglada/grids")
h2o.saveGrid(grid_directory = getwd(), grid_id = "dl_grid_fcuv")
