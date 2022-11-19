setwd("~/Tesis/datos/base de datos arreglada")
#install.packages("tidymodels")

library(pacman)
pacman::p_load(tidymodels,themis,discrim,tidyposterior,
               corrr,readr,magrittr,stringr,forcats,
               skimr,patchwork,GGally,ggthemes,dplyr,
               rlang,gtable,ggplot2,readr,readxl,
               car,MASS,ggcorrplot,reshape2,data.table,
               tidyverse,caret,rpart)
library(reticulate)
# LEER DATOS ----
datos_variables_nuevas <- read_csv("Base_final_arreglada.csv")



# library(visdat)
# vis_miss(datos_variables_nuevas)
datos1<-datos_variables_nuevas

cols=c("Genero","Carrera","rama_educacional","Dependencia","Gratuidad",
       "misma_region","nacionalidad","Etnia","Discapacidad","ingreso1","pandemia","retiro")
datos1[cols] <- lapply(datos1[cols], factor)

# Limpieza ----
datos1 <- datos1 %>% mutate (cociente= Puntaje_Seleccion/puntaje,
                             diferencia= Puntaje_Seleccion / puntaje)




# edad<-filter(datos1,edad_est <= 18)
#   View(edad)

datos1$edad_est<-ifelse(datos1$edad_est <18,18+ datos1$demora_ingreso,datos1$edad_est)




require(clustMixType)

# 
# num=c("año","Puntaje_Seleccion","preferencia","lugar","Año_egreso",
#      "Nota_Calculo_I","Notas_Algebra","edad_est","demora_ingreso",
#      "distancia","puntaje","cociente","diferencia")

# genero. carrera , puntaje ,edad estimada ,demora ingreso ,distancia ,cociente
variables_clust<- datos1[c(4,6,8,22,24,27,29)]



standarize = variables_clust %>% mutate_if(is.numeric,scale)
standarize

cluster<-kproto(standarize,k = 2,
                lambda = 5)

clustering <- data.frame(standarize,cluster$cluster)
table(cluster$cluster)
table(clustering$Carrera, clustering$cluster.cluster)


# dt <- data.frame()
# cluster1<-c()
# cluster2<-c()
# id<-c()
# cluster1n<-c()
# cluster2n<-c()
# for (i in 1:250){
#   set.seed(i)
#   cluster<-kproto(standarize,k = 2,
#                   lambda = 5)
#   clustering <- data.frame(standarize,cluster$cluster)
#   id[i]= i 
#   cluster1[i] =table(clustering$Carrera,clustering$cluster.cluster)[3,1]
#   cluster2[i]= table(clustering$Carrera,clustering$cluster.cluster)[3,2]
#   cluster1n[i]<-table(cluster$cluster)[1]
#   cluster2n[i]<-table(cluster$cluster)[2]
# }
# clus<-cbind(id,cluster1,cluster2,cluster1n,cluster2n)
set.seed(241)
clustering <- kproto(standarize,k=2,
                     lambda=5)
clustering_data <- data.frame(standarize,clustering$cluster)

clusters<- clustering_data %>% dplyr::select(Carrera,clustering.cluster)

clusters %>% ggplot(aes(x=Carrera))+ geom_bar(aes(fill=as.factor(clustering.cluster)),
                                              position = "fill")


#devtools::install_github('Mikata-Project/ggthemr')

# grafico clusters----
library("ggthemr")
ggthemr('fresh')
darken_swatch(amount = 0.1)


clusters %>% ggplot( aes(x=as.factor(Carrera), fill=as.factor(clustering.cluster)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..]) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  ylab('Percent of Cylinder Group, %') +
  scale_y_continuous(labels = scales::percent)+
  #  scale_fill_manual(name = "Cluster",
  #                    values = c('steelblue', 'tomato3'),
  #                    labels = c("1", "2"))+
  labs(x = "Carrera",
       y = "Porcentaje", title = "Gráfico de porcentaje de Carrera por Cluster",
       fill= "Cluster") + theme(axis.text=element_text(size=12),
                               axis.title=element_text(size=14,face="bold"))+ 
  theme(legend.text = element_text(size=15))+ theme(legend.title = element_text(size=20))+  theme(plot.title = element_text(size=25))




clustering_data %>% ggplot(aes(x=Puntaje_Seleccion, fill=as.factor(clustering.cluster)))+
  geom_density(alpha = 0.6)+
  labs(x = "Puntaje de selección",
       y = "Densidad", title = "Gráfico de densidad del puntaje de selección estandarizado por cluster",
       fill= "Cluster") + theme(axis.text=element_text(size=12),
                                axis.title=element_text(size=14,face="bold"))+ 
  theme(legend.text = element_text(size=15))+ theme(legend.title = element_text(size=20))+
  theme(plot.title = element_text(size=25))


