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


# train test ----
col = c('lugar')
datos1[cols] <- lapply(datos1[cols], factor)



pandemia<-as.data.frame(pandemia)
set.seed(15)
split<- initial_split(pandemia,prop=0.60)
train_pandemia <- training(split)
test_pandemia <- testing(split)


# modelo para la pandemia ----


f <- formula ( retiro ~. )

log_reg<-glm(f, data = train_pandemia, family = 'binomial')
summary(log_reg)
set.seed(14)
step.model <- stepAIC(log_reg, direction = "both", 
                      trace = TRUE)
summary(step.model)

f <- formula ( retiro ~ Carrera+misma_region+edad_est+demora_ingreso+distancia+promedio)
log_reg<-glm(f, data = train_pandemia, family = 'binomial')
log_sum<-summary(log_reg)
xtable(log_sum)


pred1<- predict.glm(log_reg,newdata = test_pandemia[-14], type="response")
library(ROCR)
pred = ROCR::prediction(pred1,test_pandemia$retiro)
perf <- performance(pred, "tpr", "fpr")
plot(perf)




k = 0
accuracy = c()
specifity = c()
precision = c()
recall=c()
for(i in seq(from = 0.05 , to = 0.75 , by = 0.01)){
  k = k + 1
  preds_binomial = ifelse(pred1 > i , 1 , 0)
  confmat = table(test_pandemia$retiro , preds_binomial)
  accuracy[k] = sum(diag(confmat)) / sum(confmat)
specifity[k] = confmat[1 , 1] / sum(confmat[1 , ])
#  precision[k] = confmat[2 , 2] / sum(confmat[ , 2])
  recall[k] = confmat[2 , 2] / sum(confmat[ 2,])
  
}
threshold=seq(from = 0.05 , to = 0.75 , by = 0.01)
data = data.frame(threshold , accuracy  ,recall,specifity)
library(ggthemr)
ggthemr('fresh')
ggplot(gather(data , key = 'Metric' , value = 'Value' , 2:3) , 
       aes(x = threshold , y = Value , color = Metric)) + 
  geom_line(size = 1.5)+labs(x= "Umbral",
                             y = "Valor")+
  scale_color_discrete(name = "Métrica",
                       labels = c( "Exactitud","Sensibilidad"))+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=17))




preds.for.50 = ifelse(pred1 > 0.23 , 1 , 0)
confusion = table(Predicted = preds.for.50 , Actual = test_pandemia$retiro)
caret::confusionMatrix(confusion)



roc_logistic <- data.frame(modelo=rep("logistic",71),
                           tpr=data$recall,
                           fpr=1-data$specifity)



# leer modelo rf para pandemia -----
library(h2o)
h2o.init(min_mem_size='3g',max_mem_size='6G')
pandemia<-pandemia %>% dplyr::select(Carrera,misma_region,edad_est,demora_ingreso,distancia,promedio,retiro)


dl_pandemia <- h2o.loadModel("C:\\Users\\Usuario\\Documents\\Tesis\\datos\\base de datos arreglada\\dl_fase3_model_244")

predict_class<-h2o.predict(dl_pandemia,as.h2o(test_pandemia)) 
predict_class<-as.data.frame(predict_class)
confusionMatrix(predict_class$predict, test_pandemia$retiro)
plot(h2o.performance(dl_pandemia,
                     newdata= as.h2o(test_pandemia)),valid=T,type='roc')

perf_rf_pandemia <- h2o.performance(dl_pandemia,as.h2o(test_pandemia))
roc_rf_pandemia<-as.data.frame(perf_rf_pandemia@metrics$thresholds_and_metric_scores[c('tpr','fpr')])
roc_rf_pandemia <- data.frame(modelo=rep("random_forest",74),roc_rf_pandemia)







k = 0
accuracy = c()
specifity = c()
precision = c()
recall=c()
for(i in seq(from = 0.2 , to = 0.5 , by = 0.01)){
  k = k + 1
  preds_binomial = ifelse(predict_class$p1 > i , 1 , 0)
  confmat = table(test_pandemia$retiro , preds_binomial)
  accuracy[k] = sum(diag(confmat)) / sum(confmat)
  specifity[k] = confmat[1 , 1] / sum(confmat[ 1, ])
  # precision[k] = confmat[2 , 2] / sum(confmat[ , 2])
  recall[k] = confmat[2 , 2] / sum(confmat[ 2,])
  
}


threshold=seq(from = 0.2 , to = 0.5 , by = 0.01)
data = data.frame(threshold , accuracy  ,recall,specifity)
ggplot(gather(data , key = 'Metric' , value = 'Value' , 2:3) , 
       aes(x = threshold , y = Value , color = Metric)) + 
  geom_line(size = 1.5)+labs(x= "Umbral",
                             y = "Valor")+
  scale_color_discrete(name = "Métrica",
                       labels = c( "Exactitud","Sensibilidad"))+
  guides(color = guide_legend(override.aes = list(size = 10)))+
  theme(legend.text = element_text(size=13))+ theme(legend.title = element_text(size=17))


preds.for.50 = ifelse(predict_class$p1 > 0.22 , 1 , 0)
confusion1 = table(Predicted = preds.for.50 , Actual = test_pandemia$retiro)
caret::confusionMatrix(confusion1)


# curvas roc -----
roc_curves<- rbind(roc_rf_pandemia,roc_logistic)

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



