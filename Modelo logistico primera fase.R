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
library(xtable)
library(plotROC)
library(pROC)
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


#vis_miss(datos1)



variables <- datos1[c(2,4,5:8,10,11,15,16,22:27,29)]
# variables<-variables %>%
#   mutate_if(is.numeric, scale)
fisica <-filter(variables, Carrera == "Física")
fcuv <- filter(variables, Carrera != "Física")
drop <- c("Carrera")
fisica = fisica[,!(names(fisica) %in% drop)]
fcuv = fcuv[,!(names(fcuv) %in% drop)]
fisica<-fisica %>%
    mutate_if(is.numeric, scale)
fcuv<-fcuv %>%
    mutate_if(is.numeric, scale)
# train test ----
set.seed(15)
split<- initial_split(fisica,prop = 4/7)
train_fisica <- training(split)
test_fisica1 <- testing(split)

set.seed(15)
split<- initial_split(fcuv,prop = 4/7)
train_fcuv <- training(split)
test_fcuv <- testing(split)



# modelo logistico fisica ----


f <- formula ( retiro ~. )
# f1 <- formula(retiro ~ distancia +año+Puntaje_Seleccion+edad_est+
#                 Genero	+ingreso1+pandemia)
# 
# log_reg<-glm(f1, data = train_fisica, family = 'binomial')


log_reg<-glm(f, data = train_fisica, family = 'binomial')
summary(log_reg)

step.model <- stepAIC(log_reg, direction = "both", 
                      trace = TRUE)
summary(step.model)


f <- formula ( retiro ~Genero + preferencia+edad_est+ingreso1+pandemia)


log_reg<-glm(f, data = train_fisica, family = 'binomial')
summary_log_fisica<-summary(log_reg)
#xtable(summary_log_fisica)

pred1<- predict.glm(log_reg,newdata = test_fisica1[-14], type="response")


# curva rocc -----



ggthemr('fresh')

k = 0
accuracy = c()
specifity = c()
precision = c()
recall=c()
for(i in seq(from = 0.1 , to = 0.5 , by = 0.01)){
  k = k + 1
  preds_binomial = ifelse(pred1 > i , 1 , 0)
  confmat = table(test_fisica1$retiro , preds_binomial)
  accuracy[k] = sum(diag(confmat)) / sum(confmat)
  specifity[k] = confmat[1 , 1] / sum(confmat[1 , ])
#  precision[k] = confmat[2 , 2] / sum(confmat[ , 2])
  recall[k] = confmat[2 , 2] / sum(confmat[ 2,])
  
}
threshold=seq(from = 0.1 , to = 0.5 , by = 0.01)
data = data.frame(threshold , accuracy  ,recall,specifity)
ggplot(gather(data , key = 'Metric' , value = 'Value' , 2:3) , 
       aes(x = threshold , y = Value , color = Metric)) + 
  geom_line(size = 1.5) +labs(x= "Umbral",
                                y = "Valor")+
   scale_color_discrete(name = "Métrica",
                     labels = c( "Exactitud","Sensibilidad")) +
  guides(color = guide_legend(override.aes = list(size = 10)))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=17))

roc_logistic <- data.frame(modelo=rep("logistic",41),
                           tpr=data$recall,
                           fpr=1-data$specifity)
preds.for.50 = ifelse(pred1 > 0.23 , 1 , 0)
confusion = table(Predicted = preds.for.50 , Actual = test_fisica1$retiro)

accuracyfisica = sum(diag(confusion)) / sum(confusion)
recallfisica = confusion[2 , 2] / sum(confusion[ ,2])
caret::confusionMatrix(confusion)
xtable(fisica_fase1)





# modelo logistico resto ----
f <- formula ( retiro ~. )
log_reg<-glm(f, data = train_fcuv, family = 'binomial')
summary(log_reg)
step.model <- stepAIC(log_reg, direction = "both", 
                      trace = TRUE)
summary(step.model)


# con el puntaje ----

f3 <- formula( retiro ~ Genero+preferencia+lugar+misma_region+edad_est+pandemia+Puntaje_Seleccion)
log_reg2<-glm(f3, data = train_fcuv, family = 'binomial')
log_fcuv<-summary(log_reg2)
#xtable(log_fcuv)
pred1<- predict.glm(log_reg2,newdata = test_fcuv[-14], type="response")
library(ROCR)
pred = ROCR::prediction(pred1,test_fcuv[14])
perf <- performance(pred, "tpr", "fpr")
plot(perf)


# sin el puntaje----
# 
# f4 <- formula( retiro ~ Genero+preferencia+lugar+misma_region+edad_est+pandemia)
# log_reg3<-glm(f4, data = train_fcuv, family = 'binomial')
# summary(log_reg3)
# 
# 
# pred1<- predict.glm(log_reg3,newdata = test_fcuv[-14], type="response")
# library(ROCR)
# pred = ROCR::prediction(pred1,test_fcuv[14])
# perf5 <- performance(pred, "tpr", "fpr")
# plot(perf5)



k = 0
accuracy = c()
specifity = c()
precision = c()
recall=c()
for(i in seq(from = 0.17 , to = 0.5 , by = 0.01)){
  k = k + 1
  preds_binomial = ifelse(pred1 > i , 1 , 0)
  confmat = table(test_fcuv$retiro , preds_binomial)
  accuracy[k] = sum(diag(confmat)) / sum(confmat)
 specifity[k] = confmat[1 , 1] / sum(confmat[ 1, ])
#  precision[k] = confmat[2 , 2] / sum(confmat[ , 2])
  recall[k] = confmat[2 , 2] / sum(confmat[ 2,])
  
}


threshold=seq(from = 0.17 , to = 0.5 , by = 0.01)
data = data.frame(threshold , accuracy  ,recall,specifity)
ggplot(gather(data , key = 'Metric' , value = 'Value' , 2:3) , 
       aes(x = threshold , y = Value , color = Metric)) + 
  geom_line(size = 1.5)+labs(x= "Umbral",
                             y = "Valor")+
  scale_color_discrete(name = "Métrica",
                       labels = c( "Exactitud","Sensibilidad"))+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=17))


preds.for.50 = ifelse(pred1 > 0.4 , 1 , 0)
confusion1 = table(Predicted = preds.for.50 , Actual = test_fcuv$retiro)
caret::confusionMatrix(confusion1)

roc_logistic <- data.frame(modelo=rep("logistic",34),
                           tpr=data$recall,
                           fpr=1-data$specifity)
agregarr <- data.frame(modelo="logistic",
                       tpr=0,
                       fpr=0)
roc_logistic<-rbind(roc_logistic,
                    agregarr)
