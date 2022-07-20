# Integrantes: Juan Spadaro; Nicolas Viñolo y Victoria Sosa

rm(list = ls()) # Borra todo el ambiente de trabajo.
gc()
#cambio el directorio sobre el que voy a trabajar
setwd('C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/')
#setwd('C:/AAMIM/Machine learning/TP FINAL')
#setwd('/Users/victoriasosa/Documents/MiM/2. Machine Learning/tp/Horse race data TP2022')

getwd()

#Importamos las funciones que tenemos en un script aparte
source("C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/TP_ML_2022_FUNCIONES.R")
#source("C:/AAMIM/Machine learning/TP FINAL/functions.R")
#source('/Users/victoriasosa/Documents/MiM/2. Machine Learning/tp/functions.R')

# Librerias ####

if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, caret, Rmisc, boot, stargazer, scales, glmnet, rpart, rpart.plot,
               ggthemes, dplyr, randomForest, xgboost, pROC, e1071, caret)


# Para visualizar graficos posteriormente
theme_set(theme_bw()) # fondo blanco
options(scipen=999) # sin notacion cientifica

# 1) Cargamos la data ####
runs <- read.table('runs.csv', sep =',', dec = '.', header = T)
head(runs,8); tail(runs,2); dim(runs) # 79447 x 37
races <- read.table('races.csv', sep =',', dec = '.', header = T)
head(races,2); tail(races,2); dim(races) # 6349 x 37

# Vamos a cruzar las tablas para tener toda la info en un solo data-frame:
df_list <- list(runs, races)
raceruns <- df_list %>% reduce(full_join, by='race_id')

dim(raceruns) # 79448 x 73 (una fila de mas?)

raceruns[79448, 1:6]
raceruns <- raceruns[-79448,]
rm(list = c('races','runs','df_list')) # Borramos información duplicada en la memoria.

head(raceruns,2)
raceruns$date <- as.Date(raceruns$date) # Las fechas de las carreras fueron modificadas para evitar problemas de copyright,
# pero el tiempo entre carrera y carrera esta correctamente cuantificado.


##~~~~~~~~~~~~~ Identificamos training, validation y testing ~~~~~~~~~~~~~~~~~~~

# En esta etapa solo identificamos, luego más adelante realizamos la separación correspondiente.
# De esta manera realizamos todas las modificaciones a un solo dataset y con dicha descripción
# tomamos recaudos para evitar cometer data leakage.

raceruns$train_val_test <- ifelse(raceruns$date >= strptime("2004-01-05", format = "%Y-%m-%d", tz = "UTC"), "test", "train")

raceruns[(which(raceruns$train_val_test == "train" & raceruns$date >= strptime("2002-08-05", format = "%Y-%m-%d", tz = "UTC"))), "train_val_test"] <- "valid"

prop.table(table(raceruns$train_val_test))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Fin del punto 1)

# 2) Pre-procesamiento de la data y exploración de datos (adicionalmente ver script TP_ML_2022_GRAFICOS.R) ####
colnames(raceruns)
summary(raceruns)
str(raceruns)

# Algunas tablitas orientativas
table(raceruns$result); prop.table(table(raceruns$result))
table(raceruns$horse_age); prop.table(table(raceruns$horse_age))
table(raceruns$horse_country); prop.table(table(raceruns$horse_country))
table(raceruns$horse_type); prop.table(table(raceruns$horse_type))
table(raceruns$horse_rating); prop.table(table(raceruns$horse_rating))
table(raceruns$horse_gear); prop.table(table(raceruns$horse_gear))
table(raceruns$venue); prop.table(table(raceruns$venue))
table(raceruns$race_no); prop.table(table(raceruns$race_no))
table(raceruns$config); prop.table(table(raceruns$config))
table(raceruns$surface); prop.table(table(raceruns$surface)) # el 89% corre en turf(césped) y el 11% en dirt (tierra)
table(raceruns$distance); prop.table(table(raceruns$distance))
table(raceruns$going); prop.table(table(raceruns$going))
table(raceruns$horse_ratings); prop.table(table(raceruns$horse_ratings))
table(raceruns$race_class); prop.table(table(raceruns$race_class))
table(raceruns$place_combination1); prop.table(table(raceruns$place_combination1))
table(raceruns$place_combination2); prop.table(table(raceruns$place_combination2))
table(raceruns$place_combination3); prop.table(table(raceruns$place_combination3))
table(raceruns$place_combination4); prop.table(table(raceruns$place_combination4))
table(raceruns$won); prop.table(table(raceruns$won))

caballos_ganadores_segun_venue <- raceruns %>%
  filter(won == 1) %>%
  group_by(venue) %>%
  summarise(Cant_carreras_ganadas = sum(won)) #vemos que hay más caballos ganadores
                                              #en la categoría ST --> 4013/(4013+2347)=63%
rm(caballos_ganadores_segun_venue)

# ~~ 2.1) Tratamiento de outliers ####

#Weight
par(mfrow=c(1,2))
weight_boxplot <- boxplot(raceruns$declared_weight, main = "Declared Weight") 
boxplot(raceruns$actual_weight, main = "Actual Weigth")

head(sort(raceruns$declared_weight))
boxplot.stats(sort(raceruns$declared_weight))$out

raceruns<-raceruns[!(raceruns$declared_weight %in% weight_boxplot$out),]
rm(weight_boxplot)

#Distance
distance_boxplot <- boxplot(raceruns$distance, main = "Distance")
max(raceruns$distance)
boxplot.stats(sort(raceruns$distance))$out

raceruns<-raceruns[!(raceruns$distance %in% distance_boxplot$out),]
rm(distance_boxplot)

#Prize
table(raceruns$prize)
prize_boxplot <- boxplot(raceruns$prize, main= "Prize")
boxplot.stats(sort(raceruns$prize))$out

raceruns<-raceruns[!(raceruns$prize %in% prize_boxplot$out),]
rm(prize_boxplot)

#Otras
boxplot.stats(sort(raceruns$age))$out
boxplot.stats(sort(raceruns$rating))$out

# No se encontraron outliers adicionales

dev.off() # Para cerrar la ventana de imagenes 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Fin del punto 2.1)

# ~~ 2.2) Tratamiento de missings (valores faltantes) ####
# % de obs con valores faltantes
sapply(raceruns, function(x) prop.table(table(missing=is.na(x))))
# Se complementó este análisis con GRÁFICO_1. Ver en script "TP_ML_2022_GRAFICOS.R" 

# Variable Prize
# Imputamos la media en valores faltantes (sólo necesario en Prize)

raceruns <- raceruns %>% mutate(prize = impute_mean(prize))

#Resto de variables con missings 
prop.table(table(is.na(raceruns$place_odds)))
table(is.na(raceruns$place_dividend3))
table(is.na(raceruns$place_combination3))

raceruns <- raceruns %>% drop_na(place_odds, place_dividend3, place_combination3)

# Variables que no tomamos en cuenta (ver justificación en el informe)  
raceruns <- raceruns %>% select(-win_dividend2, -win_combination2, -time7, 
                                -time6.y, -time6.x, -time5.y, -time5.x, -time4.y,
                                -time4.x, -sec_time7, -sec_time6, -sec_time5, -sec_time4,
                                -position_sec6, -position_sec5, -position_sec4, 
                                -place_dividend4, -place_combination4, -behind_sec6,
                                -behind_sec5,-behind_sec4)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Fin del punto 2.2)

# ~~ 2.3) Tratamiento de clases desbalanceadas ####
table(raceruns$won) #ver justificaciones en el informe .pdf

# ~~ 2.4) Feature engineering ####

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creamos variables a partir de la fecha para poder captar comportamientos estilo estacionalidades
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

raceruns$day <- as.integer(strftime(raceruns$date, format = "%d", tz = "UTC")) #Dia
raceruns$month <- as.integer(strftime(raceruns$date, format = "%m", tz = "UTC")) #Mes
raceruns$year <- as.integer(strftime(raceruns$date, format = "%Y", tz = "UTC")) #Anio
raceruns$week_day <- as.integer(strftime(raceruns$date, format = "%w", tz = "UTC")) #Dia de la semana
raceruns$year_week <- as.integer(strftime(raceruns$date, format = "%W", tz = "UTC")) #Semana del anio


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Creamos nuevas variables relativas y ratios combinando info histórica 
# (aquí tenemos un importante cuidado de no cometer data leakage
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

table(raceruns$horse_age)

race_id = unique(raceruns$race_id) # id único de cada carrera.

raceruns$max.age  <- rep(NA,dim(raceruns)[1]) # Distancia respecto del caballo más antiguos 
raceruns$min.age  <- rep(NA,dim(raceruns)[1]) # Distancia respecto del caballo más joven
raceruns$n_compet <- rep(NA,dim(raceruns)[1]) # Cuanto caballos compiten
#raceruns$relative_position <- rep(NA,dim(raceruns)[1]) # Posicion relativa del caballo en la carrera
#raceruns$horsepower <- rep(NA,dim(raceruns)[1]) # Potencia del caballo medida como el peso total sobre el tiempo en segundos

for(i in race_id){
  sel = which(raceruns$race_id == i)
  raceruns$n_compet[sel]  <- length(sel)#-1?
  raceruns$max.age[sel]   <- max(raceruns$horse_age[sel],na.rm = T) - raceruns$horse_age[sel]
  raceruns$min.age[sel]   <- raceruns$horse_age[sel] - min(raceruns$horse_age[sel],na.rm = T)
  #raceruns$relative_position[sel] <- raceruns$result[sel]/raceruns$n_compet[sel]
  #raceruns$horsepower[sel] <- raceruns$declared_weight[sel]/raceruns$finish_time[sel]
}

head(raceruns, 2)


length(unique(raceruns$horse_id)) # 4043 horses
length(unique(raceruns$jockey_id)) # 137 jockeys
length(unique(raceruns$trainer_id)) # 54 trainers


# horse_id = unique(raceruns$horse_id) # id único de cada caballo
# jockey_id = unique(raceruns$jockey_id) # id único de cada jockey
# trainer_id = unique(raceruns$trainer_id) # id único de cada trainer

# raceruns$horse_performance   <- rep(NA,dim(raceruns)[1])
# raceruns$jockey_performance  <- rep(NA,dim(raceruns)[1])
# raceruns$trainer_performance <- rep(NA,dim(raceruns)[1])
#   
# 
# for(i in horse_id){
#   sel = which(raceruns$horse_id == i)
#   raceruns$horse_performance[sel]  <- (raceruns$won == 1)[sel]/raceruns$n_compet[sel] # % de carreras ganadas por el caballo de las últimas 10 participaciones 
# }
# 
# # Tabla para horse_performance
# raceruns$horse_performance <- ifelse(raceruns$horse_performance >= 0.5 , "Descataca", "No destacada")
# table(raceruns$horse_performance); prop.table(table(raceruns$horse_performance)) 
# 
# 
# for(i in jockey_id){
#   sel = which(raceruns$jockey_id == i)
#   raceruns$jockey_performance[sel]  <- (raceruns$won == 1)[sel]/raceruns$n_compet[sel] # % de carreras ganadas por el jockey de las últimas 10 participaciones 
# }
# 
# # Tabla para jockey_performance
# raceruns$jockey_performance <- ifelse(raceruns$jockey_performance >= 0.5 , "Descataca", "No destacada")
# table(raceruns$jockey_performance); prop.table(table(raceruns$jockey_performance)) 
# 
# 
# for(i in trainer_id){
#   sel = which(raceruns$trainer_id == i)
#   raceruns$jockey_performance[sel]  <- (raceruns$won == 1)[sel]/raceruns$n_compet[sel] # % de carreras ganadas por el trainer de las últimas 10 participaciones 
# }
# 
# # Tabla para trainer_performance
# raceruns$trainer_performance <- ifelse(raceruns$trainer_performance >= 0.5 , "Descataca", "No destacada")
# table(raceruns$trainer_performance); prop.table(table(raceruns$trainer_performance))


# peso de carga sobre peso total con carga. Cuanto más alto es el ratio indica que la persona es 
# muy pesada para el caballo
raceruns$weight_ratio <- raceruns$actual_weight/raceruns$declared_weight


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Escalamos las variables
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Agregamos el feature tiempo promedio [(time1+time2+time3)/3]
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#raceruns <- raceruns%>%
# mutate(mean_time=(time1.x+time2.x+time3.x)/3)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Agrupamos algunas variables: 3 niveles según geografía y 3 según horse_ratings
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Agrupamos la variable horse_country
table(raceruns$horse_country); prop.table(table(raceruns$horse_country))

raceruns <- raceruns%>%
  mutate(horse_geography = case_when(horse_country == "AUS" |
                                      horse_country == "NZ" |
                                      horse_country == "JPN" ~'Asia y Oceanía',
                                    horse_country == "FR" |
                                      horse_country == "GB" |
                                      horse_country == "GER"|
                                      horse_country == "GR" |
                                      horse_country == "IRE" |
                                      horse_country == "ITY" |
                                      horse_country == "SAF" ~'Europa',
                                    horse_country == "BRZ" |
                                      horse_country == "ZIM" |
                                      horse_country == "ARG" ~'Otros'))

table(raceruns$horse_geography); prop.table(table(raceruns$horse_geography))

sum(is.na(raceruns$horse_geography)) #Check de que no nos falte agrupar nada

raceruns <- raceruns %>% select(-horse_country) #eliminamos la columna que ya agrupamos


# Agrupamos la variable horse_rating
table(raceruns$horse_ratings)

raceruns <- raceruns%>%
  mutate(ratings_group=case_when(  horse_ratings == "100-75" |
                                     horse_ratings == "100-80" |
                                     horse_ratings == "105-80" |
                                     horse_ratings == "105-85" |
                                     horse_ratings == "110-85" |
                                     horse_ratings == "110-90" |
                                     horse_ratings == "115-90" |
                                     horse_ratings == "115-95" |
                                     horse_ratings == "120-100" |
                                     horse_ratings == "120-95" ~'Rating alto',
                                   horse_ratings == "80-55" |
                                     horse_ratings == "80-60" |
                                     horse_ratings == "85-60" |
                                     horse_ratings == "90-70" |
                                     horse_ratings == "95-75" ~'Rating medio',
                                   horse_ratings == "G"~ 'Rating G',
                                   horse_ratings == "40-0"|
                                     horse_ratings == "40-10"|
                                     horse_ratings == "40-15"|
                                     horse_ratings == "40-20"|
                                     horse_ratings == "60-35"|
                                     horse_ratings == "60-40"|
                                     horse_ratings == "65-40"|
                                     horse_ratings == "75-55" ~ 'Rating bajo'))

sum(is.na(raceruns$ratings_group)) #Check de que no nos falte agrupar nada

raceruns <- raceruns %>% select(-horse_ratings) #eliminamos la columna que ya agrupamos


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# variacion en el peso del caballo
# tiempo de descanso entre carreras

#days_since_last_run

# El porcentaje de carreras ganadas por el caballo en su carrera.
#win_run 

# El número de victorias del jockey en su carrera hasta la fecha de la carrera.
#jnowin

# El porcentaje de victorias del jockey en su carrera hasta la fecha de la carrera.
#jwinper 

# si cambio o no de jockey entre carreras

# velocidad
# fuerza
# Cantidad de carreras que tiene el caballo, jockey y trainer (ver la experiencia)
# encode ordinal columns: config, going, horse_ratings
# win_odds es una característica muy importante


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Binarizamos 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# tiene equipo o no ? 1 si tiene o 0 si no en horse_gear
table(raceruns$horse_gear); prop.table(table(raceruns$horse_gear))
raceruns$horse_gear <- ifelse(raceruns$horse_gear == "--", 0, 1)
table(raceruns$horse_gear); prop.table(table(raceruns$horse_gear)) # el 74% no tiene equipo

# es top 3? 1 si es top3 o 0 si no
#raceruns$is_top3 <- ifelse(
#  raceruns$result == 1 | raceruns$result == 2 | raceruns$result == 3, 1, 0)
#table(raceruns$is_top3); prop.table(table(raceruns$is_top3)) # el 76% no es top3

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Pasamos a factor las variables categóricas
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

str(raceruns)

variables_factor <- c("won", "venue", "config", "going","race_class",
                      "surface", "horse_geography", "horse_type", 
                      "horse_gear","draw", "max.age", "min.age", "n_compet", 
                      "ratings_group")

raceruns[,variables_factor] <- lapply(raceruns[,variables_factor], as.factor) 

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Obtenemos el dataset definitivo 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Eliminamos las siguientes variables (ver justificación en el informe)
str(raceruns)
cols_to_delete <- c("result", "race_id", "lengths_behind" ,"horse_no", "horse_id", "position_sec3",  
                    "position_sec1", "position_sec2","behind_sec1","behind_sec2","behind_sec3",
                    "time1.x","time2.x", "time3.x","time1.y","time2.y","time3.y","finish_time",
                    "sec_time1","sec_time2", "sec_time3","place_combination1","place_combination2",
                    "place_combination3", "place_dividend1","place_dividend2","place_dividend3",
                    "win_combination1","win_dividend1", "jockey_id", "trainer_id","race_id", 
                    "horse_no", "horse_id", "date", "day", "month", "year_week", "week_day", "year",
                    "declared_weight", "actual_weight", "race_no")

raceruns <- raceruns %>% select(-cols_to_delete)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Fin de Feature engineering.


# 3) Entrenamiento de modelos ####

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Separamos en training, validation y testing 
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Ahora si realizamos la separación correspondiente identificada al inicio.

train_set <- raceruns %>% filter(train_val_test== "train") %>% select(-train_val_test)
val_set <- raceruns %>% filter(train_val_test== "valid") %>% select(-train_val_test)
test_set <- raceruns %>% filter(train_val_test== "test") %>% select(-train_val_test)

# Separamos matriz X de vector y
X_train <- train_set %>% select(-won)
X_val <- val_set %>% select(-won)
X_test <- test_set %>% select(-won)

y_train <- train_set %>% select(won) %>% a_vector()
y_val <- val_set %>% select(won)%>% a_vector()
y_test <- test_set %>% select(won) %>% a_vector()

# Pasamos a matrix para correr el modelo posteriormente
# X_train <- model.matrix( ~ .-1, train_set %>% select(-won)) 
# y_train <- as.matrix(train_set %>% select(won))
# 
# X_val <- model.matrix( ~ .-1, val_set %>% select(-won)) 
# y_val <- as.matrix(val_set %>% select(won))
# 
# X_test <- model.matrix( ~ .-1, test_set %>% select(-won)) 
# y_test <- as.matrix(test_set %>% select(won))
# 
# train_set <- as.matrix(train_set)
# val_set <- as.matrix(val_set)
# test_set <- as.matrix(test_set)


# ~~ 3.1) Modelo Baseline (Árbol de decisión) ####

tree <- rpart(as.factor(won) ~ .,
              method="class",
              data=train_set,
              control=rpart.control(minsplit=300,
                                    minbucket =5,
                                    maxdepth = 5,
                                    xval=5 ,
                                    cp=0.001))

#ploteamos el arbol
rpart.plot(tree)

# Matriz de confusión y accuracy
y_pred <- predict(tree, val_set %>% select(-won))[,2]
conf_matrix <- table(y_val, y_pred = round(y_pred,0))
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_val ~ y_pred, plot = TRUE, print.auc = TRUE)


valores.minsplit = c(100,500)      # valores de "m"
valores.minbucket = c(20,100)
valores.maxdepth = c(5,20)
valores.cp = c (0.001, 0.0001)

# Complejidad de Árboles en el bosque.
parametros = expand.grid(valores.minsplit = valores.minsplit,valores.minbucket = valores.minbucket,valores.maxdepth=valores.maxdepth,valores.cp=valores.cp ) 
# En la práctica exploramos una grilla mucho mÃ¡s grande.
head(parametros,3) # Matriz de 12 filas x 2 columnas.

te = c() # Tasa de error estimada por arboles
set.seed(1)
for(i in 1:dim(parametros)[1]){ # i recorre la grilla de parÃ¡metros.
  tree <- rpart(as.factor(won) ~ .,
                method="class",
                data=train_set,
                control=rpart.control(minsplit=parametros[i,1],
                                      minbucket =parametros[i,2],
                                      maxdepth = parametros[i,3],
                                      xval=5 ,
                                      cp=parametros[i,4]))
  y_pred <- predict(tree, val_set %>% select(-won))[,2]
  y_val <- val_set$won
  conf_matrix <- table(y_val, y_pred = round(y_pred,0))
  te[i]  <- (conf_matrix[1,2]+conf_matrix[2,1])/(sum(diag(conf_matrix))+conf_matrix[1,2]+conf_matrix[2,1])
  print(i)
}

# print(te)
which(min(te)==te)

#los mejores parametros son de la combinacion 1 y 9. 
#vamos a probar con la combinacion 1, ahora hacemos las predicciones en el set de validacion.

train_set_new <- rbind(train_set,val_set)

tree <- rpart(as.factor(won) ~ .,
              method="class",
              data=train_set_new,
              control=rpart.control(minsplit=100,
                                    minbucket =20,
                                    maxdepth = 5,
                                    xval=5 ,
                                    cp=0.001))

#ploteamos el arbol
rpart.plot(tree)


# Matriz de confusión y accuracy
y_pred <- predict(tree, test_set %>% select(-won))[,2]
conf_matrix <- table(y_test, y_pred = round(y_pred,0))
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)

#HASTA ACA CORRE EL MODELO
#Limpiamos un poco la memoria

rm(valores.cp, valores.maxdepth, valores.minbucket, valores.minsplit, parametros, tree,i,te)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FIN Modelo Baseline (Árbol de decisión).

# ~~ 3.2) Regresión logística ####

train_set_log <- train_set %>%select(-horse_type,-max.age,-min.age)
val_set_log <- val_set %>%select(-horse_type,-max.age,-min.age)
test_set_log <- test_set %>%select(-horse_type,-max.age,-min.age)

sum(is.na(train_set_log)) # tiene NA

train_set_log = na.omit(train_set_log) # borramos NA porque no son representativos


logit_reg <- glm(won ~ ., 
                 data = train_set_log, 
                 family = "binomial")

summary(logit_reg)

# Matriz de confusión y accuracy
y_pred <- predict(logit_reg, val_set_log %>% select(-won), type = "response")
y_val <- val_set_log$won
y_pred <- ifelse(y_pred>0.08,1,0) ##tomando 0.08 como benchmark

conf_matrix <- table(y_val, y_pred = round(y_pred,0))
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_val ~ y_pred, plot = TRUE, print.auc = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FIN Regresión logística.

# ~~ 3.3) Random Forest #### 

valores.minsplit = c(100,500)      # valores de "m"
valores.minbucket = c(20,100)
valores.maxdepth = c(5,20)
valores.cp = c(0.001, 0.0001)

# Complejidad de Árboles en el bosque.
parametros = expand.grid(valores.minsplit = valores.minsplit,valores.minbucket = valores.minbucket,valores.maxdepth=valores.maxdepth,valores.cp=valores.cp )


############### Sintonía fina de hiper-parámetros (OOB):
valores.m = c(20,50,100)      # valores de "m"
valores.maxnode = c(20,50,100)  # Complejidad de árboles en el bosque.
parametros = expand.grid(valores.m = valores.m,valores.maxnode = valores.maxnode)

te = c() # Tasa de error estimada por OOB.
set.seed(1)
for(i in 1:dim(parametros)[1]){ # i recorre la grilla de parámetros.
  forest.oob  = randomForest(won~.,
                             data=train_set,
                             mtry = 6, # m --> partimos de m=sqrt(21)
                             ntree=500,              # Mientras más grande, mejor.
                             sample = 44000, 
                             maxnodes = parametros[i,2], # complejidad 
                             nodesize = 150, 
                             proximity =F)  
  te[i] = 1 - sum(diag(forest.oob$confusion[,-3]))/nrow(train_set)
  print(i)
}

# print(te)
parametros[which(min(te)==te),]

# Re-entrenamaos con optimización de hiperparámetros m*, maxnodes*, minsplit, minbucket, maxdepth y cp:
modelo.final = randomForest(won~.,
                            data=train_set,
                            mtry   = 6,     # m* =sqrt(21)
                            ntree  = 500,              
                            sample = 44000, 
                            maxnodes = 100, # complejidad*
                            nodesize = 150,
                            importance = T, 
                            proximity  = F
)  
##---- FIN de selección de modelo.

varImp(modelo.final) # Importancia de cada variable en el ensamble.

# Graficamos a continuación
dotPlot(varImp(modelo.final)) 
dev.off() # Para cerrar la ventana de imagenes 

#### Extrapolamos como funcionaría el modelo seleccionado con el conjunto de test:
pred.rfor.test = predict(modelo.final,newdata=val_set)
matriz.conf = table(pred.rfor.test,y_val)
matriz.conf
1-sum(diag(matriz.conf))/13422 # 8.00% :) 

# Matriz de confusión y accuracy
metricas(matriz.conf)

# Área bajo la curva de ROC
pred.rfor.test <- as.numeric(pred.rfor.test)
roc(y_val ~ pred.rfor.test, plot = TRUE, print.auc = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FIN Random Forest.

# ~~ 3.5) SVM #### 

train_set <- as.matrix(train_set)

train_set_sparse <- one_hot_sparse(train_set)
val_set_sparse <- one_hot_sparse(val_set)
train_set_sparse <- one_hot_sparse(test_set)
gc()

#saveRDS(ads_data_sparse,"C:/Others/Master in Management + Analytics/MATERIAS/Módulo 01/Data Mining/TP-Competencia/SecondBest/ads_data_sparse_hyp_100pct.RDS")
#ads_data_sparse <- readRDS("C:/Others/Master in Management + Analytics/MATERIAS/Módulo 01/Data Mining/TP-Competencia/SecondBest/ads_data_sparse_hyp_100pct.RDS")

################### Fitting SVM con Kernel Gaussiano:

svmfit <- svm(y_train~., 
              data = train_set_sparse, 
              kernel = "radial", 
              gamma = 1,  #sigma en las slides.       
              cost = 0.000000001)   # C en las slides.        
svmfit
x11()
plot(svmfit, data) # Pifiamos un poco en el centro (x1 = 0, x2 = 0).
# El modelo (2 hiperparámetros) logra divir en dos el espacio  
# de features a través de una función no lineal en (X1,X2).

### VC de los dos hiper-parámetros sensibles del modelo:
tune.out <- tune.svm(y_train~., 
                     data = train_set, 
                     kernel = "radial",   # Kernel Gausiano. 
                     gamma = c(0.1,1,10), # valores para 'sigma'        
                     cost=2^(-1:2) ,      # Valores para 'C'
                     scale = TRUE) 

# Notar que computacionalmente al modelo le cuesta escalar en "n".
summary(tune.out)

tune.out$best.parameters 

x11()
plot(tune.out)

# Una vez estimados gamma* y C*
svmfit <- svm(y~., 
              data = data, 
              kernel = "radial", 
              gamma = 10,  #gamma*       
              cost = 1)    #C*        

x11()
plot(svmfit, data)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FIN SVM.

# ~~ 3.5) XGBoost #### 

cv <- trainControl(method = "cv",
                   number = 5,
                   classProbs = TRUE,
                   verboseIter = TRUE,
                   summaryFunction = twoClassSummary)

tune_grid <- expand.grid(nrounds = seq(from = 1, to = 5, by = 1),
                         eta = c(0.01, 0.025, 0.05, 0.1, 0.3, 0.4),
                         max_depth = 4:8,
                         gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
                         colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
                         min_child_weight = 1:6,
                         subsample = c(0.5, 0.75, 1.0)) %>% sample_n(500)

sum(is.na(train_set))
sum(is.na(test_set))
# eliminamos los datos faltantes debido a que no son representativos según la exploración de datos realizada
train_set <- na.omit(train_set) 
test_set <- na.omit(test_set)

xgb <- train(won ~ ., 
             data = train_set %>% mutate(won = ifelse(won == 0, "No", "Yes")), 
             method = "xgbTree", 
             trControl = cv,
             tuneGrid = tune_grid,
             metric = "ROC")

#saveRDS(xgb, "C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/xgb.RDS")
#xgb <- readRDS("C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/xgb.RDS")

# Matriz de confusión y accuracy
y_pred <- predict(xgb, test_set %>% select(-won), type = "prob")[, 2]
y_test <- test_set$won
conf_matrix <- table(y_test, y_pred = round(y_pred,0))
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)

plot_classes(y_test, y_pred)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ FIN XGBoost#
