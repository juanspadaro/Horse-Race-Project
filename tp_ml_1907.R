# Integrantes: Juan Spadaro; Nicolas Viñolo y Victoria Sosa

rm(list = ls()) # Borra todo el ambiente de trabajo.
gc()
#cambio el directorio sobre el que voy a trabajar
#setwd('C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/')
setwd('C:/AAMIM/Machine learning/TP FINAL')
#setwd('/Users/victoriasosa/Documents/MiM/2. Machine Learning/tp/Horse race data TP2022')

getwd()

#Importamos las funciones que tenemos en un script aparte
#source("C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/TP_ML_2022_FUNCIONES.R")
source("C:/AAMIM/Machine learning/TP FINAL/functions.R")
#source('/Users/victoriasosa/Documents/MiM/2. Machine Learning/tp/functions.R')


# Librerias ####

if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, caret, Rmisc, boot, stargazer, scales, glmnet, rpart, rpart.plot,
               ggthemes, dplyr, randomForest, xgboost, pROC, e1071, caret)
library(Metrics)

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


# 2) Pre-procesamiento de la data y exploración de datos ####
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
table(raceruns$surface); prop.table(table(raceruns$surface)) # el 89% corre en turf(césped)
# y el 11% en dirt (tierra)
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

#Prize
table(raceruns$prize)
prize_boxplot <- boxplot(raceruns$prize, main= "Prize")
boxplot.stats(sort(raceruns$prize))$out

raceruns<-raceruns[!(raceruns$prize %in% prize_boxplot$out),]
rm(prize_boxplot)
#Otras
boxplot.stats(sort(raceruns$age))$out
boxplot.stats(sort(raceruns$rating))$out

#No se encontraron outliers

dev.off() # Para cerrar la ventana de imagenes 

# ~~ 2.2) Tratamiento de missings (valores faltantes) ####
# % de obs con valores faltantes

sapply(raceruns, function(x) prop.table(table(missing=is.na(x))))
#Se complementó este análisis con gráfico n° XX en script "TP_ML_2022_GRAFICOS" anexo

# Variable Prize
# Imputamos la media en valores faltantes (sólo necesario en Prize)

raceruns <- raceruns %>% mutate(prize = impute_mean(prize))

#Resto de variables con missings 
table(is.na(raceruns$place_odds))
table(is.na(raceruns$place_dividend3))
table(is.na(raceruns$place_combination3))

raceruns <- raceruns %>% drop_na(place_odds, place_dividend3, place_combination3)

# Variables que no tomamos en cuenta  
raceruns <- raceruns %>% select(-win_dividend2, -win_combination2, -time7, 
                                -time6.y, -time6.x, -time5.y, -time5.x, -time4.y,
                                -time4.x, -sec_time7, -sec_time6, -sec_time5, -sec_time4,
                                -position_sec6, -position_sec5, -position_sec4, 
                                -place_dividend4, -place_combination4, -behind_sec6,
                                -behind_sec5,-behind_sec4)

# ~~ 2.3) Tratamiento de clases desbalanceadas ####
table(raceruns$won)

# ~~ 2.4) Feature engineering ####

# Ejemplo: Para cada caballo/ginete/entrenador, puedes calcular el % de carreras ganadas 
#          entre las últimas 5/10/20 participaciones (pensá vos que atributos podrían ser relevantes) 


# Notar que "result", "won" y "finish_time" contienen información sobre el resultado
# de la carrera (ojo con el data leakage).

# Notar que las variables "win_odds" y "place_odds" reflejan las expectativas "de mercado"
# respecto de la posibilidad de que el caballo en cuestión gane la carrera.

raceruns$weight_ratio <- raceruns$actual_weight/raceruns$declared_weight # peso de carga sobre peso total con carga

# cuanto más alto es el ratio indica que la persona es muy pesada para el caballo
# Notar que no estamos incluyendo información dinámica de cada caballo/jinete.

#Escalamos las variables


# Creamos variables a partir de la fecha para poder captar comportamientos estilo estacionalidades
raceruns$day <- as.integer(strftime(raceruns$date, format = "%d", tz = "UTC")) #Dia
raceruns$month <- as.integer(strftime(raceruns$date, format = "%m", tz = "UTC")) #Mes
raceruns$year <- as.integer(strftime(raceruns$date, format = "%Y", tz = "UTC")) #Anio
raceruns$week_day <- as.integer(strftime(raceruns$date, format = "%w", tz = "UTC")) #Dia de la semana
raceruns$year_week <- as.integer(strftime(raceruns$date, format = "%W", tz = "UTC")) #Semana del anio

# Agregamos el feature tiempo promedio [(time1+time2+time3)/3]
#raceruns <- raceruns%>%
# mutate(mean_time=(time1.x+time2.x+time3.x)/3)

# Agregamos algunas comparaciones relativas
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

# Agregamos 3 niveles según horse_ratings
raceruns <- raceruns%>%
  mutate(ratings_group=case_when(  horse_ratings == "100-75" |
                                     horse_ratings == "100-80" |
                                     horse_ratings == " 100+" |
                                     horse_ratings == "105-80" |
                                     horse_ratings == "105-85" |
                                     horse_ratings == "110-80" |
                                     horse_ratings == "100-85" |
                                     horse_ratings == "110-90" |
                                     horse_ratings == "115-90" |
                                     horse_ratings == "115-95" |
                                     horse_ratings == "120-100" |
                                     horse_ratings == "120-95" ~'Rating alto',
                                   horse_ratings == "80-55" |
                                     horse_ratings == "80-60" |
                                     horse_ratings == "80+"   |
                                     horse_ratings == "85-60" |
                                     horse_ratings == "85+"   |
                                     horse_ratings == "90-70" |
                                     horse_ratings == "90+"   |
                                     horse_ratings == "95-70" |
                                     horse_ratings == "95-75" |
                                     horse_ratings == "95+"~'Rating medio',
                                   horse_ratings == "G"~ 'Rating G',
                                   horse_ratings == "40-0"|
                                     horse_ratings == "40-10"|
                                     horse_ratings == "40-15"|
                                     horse_ratings == "40-20"|
                                     horse_ratings == "60-35"|
                                     horse_ratings == "60-40"|
                                     horse_ratings == "75-55" ~ 'Rating bajo'))

raceruns <- raceruns %>% select(-horse_ratings)

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
#~~~~~~~~~~~~~~~~~~~~~ Binarizamos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# tiene equipo o no ? 1 si tiene o 0 si no en horse_gear
table(raceruns$horse_gear); prop.table(table(raceruns$horse_gear))
raceruns$horse_gear <- ifelse(raceruns$horse_gear == "--", 0, 1)
table(raceruns$horse_gear); prop.table(table(raceruns$horse_gear)) # el 72% no tiene equipo

# es top 3? 1 si es top3 o 0 si no
#raceruns$is_top3 <- ifelse(
#  raceruns$result == 1 | raceruns$result == 2 | raceruns$result == 3, 1, 0)
#table(raceruns$is_top3); prop.table(table(raceruns$is_top3)) # el 76% no es top3



# Pasamos a factor las variables categóricas

variables_factor <- c("venue","race_no", "config","going","race_class", "horse_no",
                      "surface", "won", "horse_country", "horse_type", 
                      "horse_gear","draw", "max.age", "min.age", "n_compet", 
                      "ratings_group", "trainer_id", "jockey_id")

raceruns[,variables_factor] <- lapply(raceruns[,variables_factor], as.factor) 


#raceruns_nb <- raceruns %>% select(variables_factor)
#raceruns_nb[,variables_factor] <- lapply(raceruns_nb[,variables_factor], as.factor)

# 3) Entrenamiento de modelos ####


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~ Dividimos los datos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Solo tomo algunas de las variables del data set.
#X = raceruns

#race_id = unique(X[,1])

#X = model.matrix(~.,data = X)[ , -1]

##~~~~~~~~~~~~~ Identificamos training, validation y testing ~~~~~~~~~~~~~
raceruns$train_val_test <- ifelse(raceruns$date >= strptime("2004-01-05", format = "%Y-%m-%d", tz = "UTC"), "test", "train")

raceruns[(which(raceruns$train_val_test == "train" & raceruns$date >= strptime("2002-08-05", format = "%Y-%m-%d", tz = "UTC"))), "train_val_test"] <- "valid"

prop.table(table(raceruns$train_val_test))


variables_spoiler <- c("result", "race_id", "lengths_behind" ,"horse_no", "horse_id", "position_sec3",  "position_sec1", 
                       "position_sec2","behind_sec1","behind_sec2","behind_sec3","time1.x","time2.x",
                       "time3.x","time1.y","time2.y","time3.y","finish_time","sec_time1","sec_time2",
                       "sec_time3","place_combination1","place_combination2","place_combination3",
                       "place_dividend1","place_dividend2","place_dividend3",
                       "win_combination1","win_dividend1")

raceruns <- raceruns %>% select(-variables_spoiler)

variables_problematicas <- c("ratings_group", "jockey_id", "trainer_id","date")
raceruns <- raceruns %>% select(-variables_problematicas)

train_set <- raceruns %>% filter(train_val_test== "train") %>% select(-train_val_test)
val_set <- raceruns %>% filter(train_val_test== "valid") %>% select(-train_val_test)
test_set <- raceruns %>% filter(train_val_test== "test") %>% select(-train_val_test)

#----------------------------------------------------#

############################
###   Ãrbol de clasif.   ###
############################

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


# Matriz de confusiÃ³n y accuracy
y_pred <- predict(tree, val_set %>% select(-won))[,2]
y_val <- val_set$won
conf_matrix <- table(y_val, y_pred = round(y_pred,0))
metricas(conf_matrix)

# Ãrea bajo la curva de ROC
roc(y_val ~ y_pred, plot = TRUE, print.auc = TRUE)

valores.minsplit = c(100,500)      # valores de "m"
valores.minbucket = c(20,100)
valores.maxdepth = c(5,20)
valores.cp = c (0.001, 0.0001)
# Complejidad de Ã¡rboles en el bosque.
parametros = expand.grid(valores.minsplit = valores.minsplit,valores.minbucket = valores.minbucket,valores.maxdepth=valores.maxdepth,valores.cp=valores.cp ) 
# En la prÃ¡ctica exploramos una grilla mucho mÃ¡s grande.
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
  y_val = as.numeric(y_val)
  y_val=as.vector(y_val)
  y_pred = round(y_pred,0)
  y_pred=as.vector(y_pred)
  conf_matrix <- table(y_val, y_pred)
  accuracy <- sum(diag(prop.table(conf_matrix)))
  precision <- prop.table(conf_matrix, margin = 2)[2,2]
  recall <- prop.table(conf_matrix, margin = 1)[2,2]
  fb_score <- fbeta_score(y_val,y_pred, beta=0.3)
  te[i]  <- fb_score
  print(i)
}

# print(te)
which(max(te)==te)


#Reentrenamos con los mejores hiperparametros 
#en el set de validacion

train_set_new <- rbind(train_set,val_set)

tree <- rpart(as.factor(won) ~ .,
              
              method="class",
              
              data=train_set_new,
              
              control=rpart.control(minsplit=100,
                                    
                                    minbucket =20,
                                    
                                    maxdepth = 20,
                                    
                                    xval=5 ,
                                    
                                    cp=0.0001))

#ploteamos el arbol
rpart.plot(tree)


# Matriz de confusiÃ³n y accuracy
y_pred <- predict(tree, test_set %>% select(-won))[,2]
y_test <- test_set$won
conf_matrix <- table(y_test, y_pred = round(y_pred,0))
metricas(conf_matrix)
y_test <- as.numeric(y_test)
y_test <- as.vector(y_test)
y_pred = round(y_pred,0)
y_pred=as.vector(y_pred)
fb_score <- fbeta_score(y_test,y_pred, beta=0.3)
print(paste("Fb score:", round(fb_score, 3)))

# Ãrea bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)

#HASTA ACA CORRE EL MODELO
#Limpiamos un poco la memoria

rm(valores.cp, valores.maxdepth, valores.minbucket, valores.minsplit, parametros, tree,i,te)


############################
###    Reg. LogÃ­stica    ###
############################

#separamos la data
train_set_log <- train_set %>%select(-horse_country,-max.age,-min.age,-draw)
val_set_log <- val_set %>%select(-horse_country,-max.age,-min.age,-draw)
test_set_log <- test_set %>%select(-horse_country,-max.age,-min.age,-draw)

train_set_log = na.omit(train_set_log)

#entrenamos modelo benchmark
logit_reg <- glm(won ~ ., data = train_set_log, family = "binomial")

y_pred <- predict(logit_reg, val_set_log %>% select(-won), type = "response")
y_val <- val_set_log$won
y_pred <- ifelse(y_pred>0.08,1,0) ##tomando 0.08 como benchmark

# Matriz de confusiÃ³n y accuracy
conf_matrix <- table(y_val, y_pred = round(y_pred,0))
metricas(conf_matrix)

# Ãrea bajo la curva de ROC
roc(y_val ~ y_pred, plot = TRUE, print.auc = TRUE)


### AJUSTAMOS BENCHMARK

umbral = c (0.05,0.06,0.07, 0.08,0.12,0.18,0.2,0.25)

te = c() #tasa de error estimada en logit


for(i in 1:length(umbral)){ # recorre todos los valores del umbral
  logit_reg <- glm(won ~ ., data = train_set_log, family = "binomial")
  
  # Matriz de confusiÃ³n y accuracy
  y_pred <- predict(logit_reg, val_set_log %>% select(-won), type = "response")
  y_val <- val_set_log$won
  y_val <- as.numeric(y_val)
  y_val<-as.vector(y_val)
  y_pred <- ifelse(y_pred>umbral[i],1,0)##probamos el umbral
  y_pred <- as.vector(y_pred)
  conf_matrix <- table(y_val, y_pred)
  accuracy <- sum(diag(prop.table(conf_matrix)))
  precision <- prop.table(conf_matrix, margin = 2)[2,2]
  recall <- prop.table(conf_matrix, margin = 1)[2,2]
  fb_score <- fbeta_score(y_val,y_pred, beta=0.3)
  te[i]  <- fb_score
  print(i)
}

# print(te)
which.max(te)

#Reentrenamos modelo con el umbral correcto: 
#sobre los datos de validacion

train_set_log_new <- rbind(train_set_log,val_set_log)

logit_reg <- glm(won ~ ., data = train_set_log_new, family = "binomial")

y_pred <- predict(logit_reg, test_set_log %>% select(-won), type = "response")
y_test <- test_set_log$won
y_pred <- ifelse(y_pred>0.05,1,0) ##tomando 0.05

# Matriz de confusiÃ³n y accuracy
conf_matrix <- table(y_test, y_pred)
metricas(conf_matrix)
y_test <- as.numeric(y_test)
y_test <- as.vector(y_test)
y_pred=as.vector(y_pred)
fb_score <- fbeta_score(y_test,y_pred, beta=0.3)
print(paste("Fb score:", round(fb_score, 3)))


# Ãrea bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)




#RANDOM FOREST ----------------------------
# ?randomForest
forest.bench = randomForest(won~.,
                            data=train_set,
                            mtry =  7,# m = 10 (si m = cantidad features entonces estamos haciendo Bagging)
                            ntree=500,                  # B
                            sample = 50000, # tamaÃ±o de cada re-muestra bootstrap.
                            maxnodes = 20,  # cantidad mÃ¡xima de nodos terminales en c/ Ã¡rbol.
                            nodesize = 5, # cantidad mÃ­nima de datos en nodo terminal.
                            importance=T,   # Computar importancia de c/ covariable.
                            # proximity =F    #computa la matriz de proximidad entre observaciones.
                             na.action =  na.roughfix # imputa perdidos con mediana / moda. 
                            # na.action = rfImpute     # imputa perdidos con datos prÃ³ximos.
                            # na.action = na.omit      # descarta valores perdidos. 
                            )

#@ Respecto del Fitting del modelo:
head(forest.bench$oob.times,3) # nro de veces q c/obs quedÃ³ out-of-bag. 
head(forest.bench$votes,3)     # (OOB)

head(forest.bench$predicted,3) # (Ã­dem pero catagorÃ­as).
forest.bench$confusion         # Matriz de confusiÃ³n (OOB)

# Performance: (estimaciones OOB)
forest.bench # 

X11()
varImpPlot(forest.bench, main ='Importancia de cada feature')



#------------------------------------------------------------------------- End#

# Etapa perscriptiva: Invirtiendo con el modelo.
# Estrategia naive 2: SÃ³lo invierto en el caballo cuya prob. estimada de 
#                     ganar es la mÃ¡ixma y siempre que Ã©sta sea mayor a 1/4.

flag.logit     <- rep(NA,length(race_id)) # 1 si acierto ganador y 0 en otro caso.
return.logit   <- rep(NA,length(race_id)) # Resultado neto de apostar en cada carrera.

nbets.logit = naivebet1 =  0
# Nota deberÃ­amos hacer esta cuenta sobre un conjunto de validaciÃ³n.
for(i in 1:length(race_id)){
  sel = which(X[,1] == race_id[i])
  flag.logit[i]   <- as.integer(which.max(pred[sel])==which.max(X[sel,2])) # = 1 if max prob == real winer.
  if( max(pred[sel]) > 0.25 ){ # Solo apuesto si estoy relativamente seguro de ganar
    return.logit[i]  <- flag.logit[i]*X[sel[which.max(pred[sel])],45] - 1;
    nbets.logit = nbets.logit + 1
  } else {return.logit[i] = 0}
  naivebet1 = naivebet1 + X[sel[which.max(X[sel,2])],45] - length(sel) # Apuesto 1 USD a cada caballo. 
}


# Estrategia de invertir 1 dÃ³lar por caballo en todas las carreras:
naivebet1 / 75712 # Retorno sobre el total invertido
naivebet1 # De los 75712 invertidos terminÃ© perdiendo 19400 (apporx)

# AnÃ¡lisis de desempeÃ±o del modelo y de la estrategia de inversiÃ³n:
mean(flag.logit) # Tasa de acierto del modelo (train + test)
nbets.logit ; nbets.logit/length(race_id) # Apuesto solo el 1.2% de las carreras. 
sum(return.logit)/nbets.logit  # Retorno sobre el total invertido (7.7%)





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#     
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~# Estos modelos corren pero hay que mejorarlos  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#      
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#      

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ 3.1) Random Forest (---------ok---------)#### 
############### Sintonía fina de hiper-parámetros (OOB):
valores.m = c(20,50,100)      # valores de "m"
valores.maxnode = c(20,50,100)  # Complejidad de árboles en el bosque.
parametros = expand.grid(valores.m = valores.m,valores.maxnode = valores.maxnode) 
# En la práctica exploramos una grilla mucho más grande.
head(parametros,3) # Matriz de 9 filas x 2 columnas.

te = c() # Tasa de error estimada por OOB.
set.seed(1)
for(i in 1:dim(parametros)[1]){ # i recorre la grilla de parámetros.
  forest.oob  = randomForest(won~.,
                             data=train_set,
                             mtry = 26, # m
                             ntree=10,              # Mientras más grande, mejor.
                             sample = 3000, 
                             maxnodes = parametros[i,2], # complejidad 
                             nodesize = 150, 
                             proximity =F)  
  te[i] = 1 - sum(diag(forest.oob$confusion[,-3]))/nrow(train_set)
  print(i)
}

# print(te)
which(min(te)==te) #es 2 por eso va debajo debajo
parametros[2,] # m* = 50, maxnode* = 50 (estimaciones de mala calidad porque B es relativamente pequeño)

# Re-entrenamaos con m* y maxnodes*:
modelo.final = randomForest(won~.,
                            data=train_set,
                            mtry   = 26,     # m*
                            ntree  = 6,              
                            sample = 3000, 
                            maxnodes = 50, # complejidad*
                            nodesize = 150,
                            importance = F, 
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
1-sum(diag(matriz.conf))/13422 # 8.00% :) 12305+1037+49+31

# Matriz de confusión y accuracy
metricas(matriz.conf)

# Área bajo la curva de ROC
pred.rfor.test <- as.numeric(pred.rfor.test)
roc(y_val ~ pred.rfor.test, plot = TRUE, print.auc = TRUE)

#------------------------------------------------------------ END.



# ~~ 3.5) XGBoost (---------ok---------)#### 

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
                         subsample = c(0.5, 0.75, 1.0)) %>% sample_n(30)

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










