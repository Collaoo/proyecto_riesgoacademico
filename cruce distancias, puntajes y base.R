setwd("~/Tesis/datos/base de datos arreglada")

datos1 <- read_csv("base_retiro_arreglado.csv")


datos1$retiro<-factor(datos1$retiro)

cols=c("Genero","Carrera","rama_educacional","Dependencia","Gratuidad",
       "misma_region","nacionalidad","Etnia","Discapacidad","ingreso1","pandemia","retiro")
datos1[cols] <- lapply(datos1[cols], factor) 



library(readr)
distancia_ciencias <- read_csv("distancia_ciencias1.csv")


library(readxl)
puntajes_porcomuna <- read_excel("puntajes_porcomuna.xlsx", 
                                 col_types = c("text", "numeric"))
distancia_ciencias<-distancia_ciencias[2:3]
colnames(puntajes_porcomuna)<-c("comuna","puntaje")
puntajes_porcomuna$comuna[puntajes_porcomuna$comuna=="Macul (33)"]<-"Macul"

aysen <- data.frame(comuna="Aysén",
                    distancia= 1483)
distancia_ciencias<-rbind(distancia_ciencias,aysen)



completo<-merge(x=distancia_ciencias,y=puntajes_porcomuna)


datos_id<-dplyr::mutate(datos1, ID = row_number())

datos_id$comuna[datos_id$comuna=="Coihaique"]<-"Coyhaique"
datos_id$comuna[datos_id$comuna=="Llaillay"]<-"Llay Llay"
datos_id$comuna[datos_id$comuna=="Aisén"]<-"Aysén"


datos_id$comuna[datos_id$comuna == 'EXTRANJERO'] <- "Valparaíso"


test <- merge(x=datos_id,y=completo,by="comuna")



setdiff(1:588,test$ID)
View(datos_id)

write.csv(test, file= "datos_full_arreglado.csv")


