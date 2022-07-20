# Principio ----
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


# Librerias

if(!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, caret, Rmisc, boot, stargazer, scales, glmnet, rpart, rpart.plot,
               ggthemes, dplyr, randomForest, xgboost, pROC, e1071, caret)
library(Metrics)

####cargamos funcion auxiliar

random_grid <- function(size,
                        min_nrounds, max_nrounds,
                        min_max_depth, max_max_depth,
                        min_eta, max_eta,
                        min_gamma, max_gamma,
                        min_colsample_bytree, max_colsample_bytree,
                        min_min_child_weight, max_min_child_weight,
                        min_subsample, max_subsample) {
  
  rgrid <- data.frame(nrounds = if (min_nrounds == max_nrounds) {
    rep(min_nrounds, size)
  } else {
    sample(c(min_nrounds:max_nrounds),
           size = size, replace = TRUE)
  },
  max_depth = if (min_max_depth == max_max_depth) {
    rep(min_max_depth, size)
  } else {
    sample(c(min_max_depth:max_max_depth),
           size = size, replace = TRUE)
  },
  eta = if (min_eta == max_eta) {
    rep(min_eta, size)
  } else {
    round(runif(size, min_eta, max_eta), 7)
  },
  gamma = if (min_gamma == max_gamma) {
    rep(min_gamma, size)
  } else {
    round(runif(size, min_gamma, max_gamma), 7)
  },
  colsample_bytree = if (min_colsample_bytree == max_colsample_bytree) {
    rep(min_colsample_bytree, size)
  } else {
    round(runif(size, min_colsample_bytree, max_colsample_bytree), 7)
  },
  min_child_weight = if (min_min_child_weight == max_min_child_weight) {
    rep(min_min_child_weight, size)
  } else {
    round(runif(size, min_min_child_weight, max_min_child_weight), 7)
  },
  subsample = if (min_subsample == max_subsample) {
    rep(min_subsample, size)
  } else {
    round(runif(size, min_subsample, max_subsample), 7)
  })
  
  return(rgrid)
}



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

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~~~~~~~~~~~~~~~~~~~~~~~ Dividimos los datos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#por las dudas limpiamos na

train_set = na.omit(train_set)
val_set = na.omit(val_set)
test_set = na.omit(test_set)

rm(caballos_ganadores_segun_venue)

#3)Seleccion de modelos----------------------------

X=train_set[,c(2,5,7,8,10,11,16,18,20)]

y = train_set[,1]

pca <- prcomp(X, center = T, scale = T) # Escalamos las variables.

### Visualización:
library(factoextra)
fviz_eig(pca)

# Scree plot y Gráfico de varianza acumulada:
x11()
par(mfrow = c(1,2))
plot(pca$sdev^2, type = 'b', main ='Scree plot', ylab = '', xlab = 'm')
tm = cumsum(pca$sdev^2) / sum(pca$sdev^2)  #con 6 explicamos mas del 80% de la varianza
plot(tm, type = 'b', main ='Varianza acumulada', ylab = '', xlab = 'm')

# PCR:
datos <- data.frame(y,pca$x)
datos$y <-as.numeric(datos$y)
head(datos,3)

pcr <- lm(y~PC1+PC2+PC3+PC4+PC5+PC6, data = datos)
summary(pcr) #R2adj = 0.03205 muy bajo

X_val<-val_set[,c(2,5,7,8,10,11,16,18,20)]

y_val <- val_set[,1]

pca_val <- prcomp(X_val, center = T, scale = T)

datos_val <- data.frame(y_val,pca_val$x)
datos_val$y_val <-as.numeric(datos_val$y_val)

pcr_val <- lm(y~PC1+PC2+PC3+PC4+PC5+PC6, data = datos)
y_pred <- predict(pcr_val, newdata = datos_val)
y_pred <- ifelse(y_pred>1.05,1,0) 

# Matriz de confusiÃ³n y accuracy

y_val = as.numeric(y_val)
y_val=as.vector(y_val)
y_pred=as.vector(y_pred)

conf_matrix <- table(y_val, y_pred)
metricas(conf_matrix)
accuracy <- sum(diag(prop.table(conf_matrix)))
precision <- prop.table(conf_matrix, margin = 2)[2,2]
recall <- prop.table(conf_matrix, margin = 1)[2,2]
fb_score <- fbeta_score(y_val,y_pred, beta=0.05)
fb_score
#~~3.1) Optimizamos cuantas variables usar----

fb <- c()
fb[1]  <- fb_score #con 6

#probamos con 5

pcr_val <- lm(y~PC1+PC2+PC3+PC4+PC5, data = datos)
y_pred <- predict(pcr_val, newdata = datos_val)
y_pred <- ifelse(y_pred>1.05,1,0) 

# Matriz de confusiÃ³n y accuracy

y_val = as.numeric(y_val)
y_val=as.vector(y_val)
y_pred=as.vector(y_pred)

conf_matrix <- table(y_val, y_pred)
accuracy <- sum(diag(prop.table(conf_matrix)))
precision <- prop.table(conf_matrix, margin = 2)[2,2]
recall <- prop.table(conf_matrix, margin = 1)[2,2]
fb_score <- fbeta_score(y_val,y_pred, beta=0.05)
fb_score
fb[2]  <- fb_score #con 5

#probamos con 7

pcr_val <- lm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7, data = datos)
y_pred <- predict(pcr_val, newdata = datos_val)
y_pred <- ifelse(y_pred>1.05,1,0) 

# Matriz de confusiÃ³n y accuracy

y_val = as.numeric(y_val)
y_val=as.vector(y_val)
y_pred=as.vector(y_pred)

conf_matrix <- table(y_val, y_pred)
accuracy <- sum(diag(prop.table(conf_matrix)))
precision <- prop.table(conf_matrix, margin = 2)[2,2]
recall <- prop.table(conf_matrix, margin = 1)[2,2]
fb_score <- fbeta_score(y_val,y_pred, beta=0.05)
fb_score
fb[3]  <- fb_score #con 7
which(max(fb)==fb)

#el mejor es con 7 variables.

### AJUSTAMOS UMBRAL

umbral = c (1.02, 1.05, 1.1)

fb = c() #fb score para PCA


for(i in 1:length(umbral)){ # recorre todos los valores del umbral
  pcr_val <- lm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7, data = datos)
  y_pred <- predict(pcr_val, newdata = datos_val)
  y_pred <- ifelse(y_pred>umbral[i],1,0) 
  
  y_val <- val_set$won
  conf_matrix <- table(y_val, y_pred)
  y_val <- as.numeric(y_val)
  y_val<-as.vector(y_val)
  y_pred <- as.vector(y_pred)
  
  accuracy <- sum(diag(prop.table(conf_matrix)))
  precision <- prop.table(conf_matrix, margin = 2)[2,2]
  recall <- prop.table(conf_matrix, margin = 1)[2,2]
  fb_score <- fbeta_score(y_val,y_pred, beta=0.05)
  fb[i]  <- fb_score
  print(i)
}

# print(fb)
max <-which.max(fb)
max #UMBRAL 2

#~~3.2) Reentrenamos modelo ------
#Reentrenamos modelo con el umbral correcto: 
#sobre los datos de test

train_set_new <- rbind(train_set,val_set)

X<-train_set_new[,c(2,5,7,8,10,11,16,18,20)]
X_test <- test_set[,c(2,5,7,8,10,11,16,18,20)]
y <- train_set_new[,1]
y_test <- test_set[,1]

pca <- prcomp(X, center = T, scale = T)
pca_test <- prcomp(X_test, center = T, scale = T)

datos <- data.frame(y,pca$x)
datos$y <-as.numeric(datos$y)
datos_test <- data.frame(y_test,pca_test$x)
datos_test$y_test <-as.numeric(datos_test$y_test)



pcr <- lm(y~PC1+PC2+PC3+PC4+PC5+PC6+PC7, data = datos)
y_pred <- predict(pcr, newdata = datos_test)
y_pred <- ifelse(y_pred>umbral[max],1,0) 
y_test <- test_set$won

#Matriz de confusion

conf_matrix <- table(y_test, y_pred)
metricas(conf_matrix)
y_test <- as.numeric(y_test)
y_test <- as.vector(y_test)
y_pred=as.vector(y_pred)
fb_score <- fbeta_score(y_test,y_pred, beta=0.05)
print(paste("Fb score:", round(fb_score, 3)))

# Área bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)


#3)Arbol de clasificacion----------------------------

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
valores.cp = c (0.001, 0.0001) #complejidad arbol

parametros = expand.grid(valores.minsplit = valores.minsplit,valores.minbucket = valores.minbucket,valores.maxdepth=valores.maxdepth,valores.cp=valores.cp ) 
head(parametros,3) # 

#~~3.1) Ajustamos hiperparametros ------

fb = c() #fb score para logit
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
  fb_score <- fbeta_score(y_val,y_pred, beta=0.05)
  fb[i]  <- fb_score
  print(i)
}

# print(fb)
which(max(fb)==fb)

#~~3.2) Reentrenamos modelo ------
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
fb_score <- fbeta_score(y_test,y_pred, beta=0.05)
print(paste("Fb score:", round(fb_score, 3)))

# Ãrea bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)



#4) Regresion logistica ------

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


#~~4.1) Ajustamos umbral ------

### AJUSTAMOS BENCHMARK

umbral = c (0.05,0.06,0.07, 0.08,0.085, 0.09,0.12,0.18,0.2,0.25)

fb = c() #fb score para logit


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
  fb_score <- fbeta_score(y_val,y_pred, beta=0.05)
  fb[i]  <- fb_score
  print(i)
}

# print(fb)
max <- which.max(fb)

#~~4.2) Reentrenamos modelo ------
#Reentrenamos modelo con el umbral correcto: 
#sobre los datos de test

train_set_log_new <- rbind(train_set_log,val_set_log)

logit_reg <- glm(won ~ ., data = train_set_log_new, family = "binomial")

y_pred <- predict(logit_reg, test_set_log %>% select(-won), type = "response")
y_test <- test_set_log$won
y_pred <- ifelse(y_pred>umbral[max],1,0) ##tomando 0.05

# Matriz de confusiÃ³n y accuracy
conf_matrix <- table(y_test, y_pred)
metricas(conf_matrix)
y_test <- as.numeric(y_test)
y_test <- as.vector(y_test)
y_pred=as.vector(y_pred)
fb_score <- fbeta_score(y_test,y_pred, beta=0.05)
print(paste("Fb score:", round(fb_score, 3)))


# Ãrea bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)

#limpiamos

rm(train_set_log, val_set_log, test_set_log, train_set_log_new,logit_reg)

#----------------------------------------------------#

#5) Random forest ------


oob <- trainControl(method = "oob",
                    classProbs = TRUE,
                    verboseIter = TRUE)
grid <- data.frame(mtry = seq(2,16, 2))

train_set = na.omit(train_set)
sum(is.na(train_set))


# rf <- train(won ~ ., 
#             data = train_set %>% mutate(won = ifelse(won == 0, "No", "Yes")), 
#             method = "rf", 
#             trControl = oob,
#             tuneGrid = grid,
#             metric = "ROC")

#saveRDS(rf,"C:/AAMIM/Machine learning/TP FINAL/rf.RDS")

rf <- readRDS("rf.RDS")


# Armo primer modelo RF
y_pred <- predict(rf, val_set %>% select(-won), type = "prob")[, 2]

#Matriz de confusion
y_pred <- ifelse(y_pred>0.02,1,0)
y_val <- val_set$won
conf_matrix <- table(y_val, y_pred)
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_val ~ y_pred, plot = TRUE, print.auc = TRUE)

#~~5.1) Ajustamos umbral ------

### AJUSTAMOS UMBRAL

umbral = c (0.001, 0.01, 0.02,0.025,0.035,0.07)

fb = c() #fb score para random forest


for(i in 1:length(umbral)){ # recorre todos los valores del umbral
  y_pred <- predict(rf, val_set %>% select(-won), type = "prob")[, 2]
  y_pred <- ifelse(y_pred>umbral[i],1,0)
  
  y_val <- val_set$won
  conf_matrix <- table(y_val, y_pred)
  y_val <- as.numeric(y_val)
  y_val<-as.vector(y_val)
  y_pred <- as.vector(y_pred)
  
  accuracy <- sum(diag(prop.table(conf_matrix)))
  precision <- prop.table(conf_matrix, margin = 2)[2,2]
  recall <- prop.table(conf_matrix, margin = 1)[2,2]
  fb_score <- fbeta_score(y_val,y_pred, beta=0.05)
  fb[i]  <- fb_score
  print(i)
}

# print(fb)
max <-which.max(fb)


#~~5.2) Reentrenamos modelo ------
#Reentrenamos modelo con el umbral correcto: 
#sobre los datos de test

train_set_new <- rbind(train_set,val_set)
train_set_new = na.omit(train_set_new)
sum(is.na(train_set_new))

# rf2 <- train(won ~ .,
#              data = train_set_new %>% mutate(won = ifelse(won == 0, "No", "Yes")),
#              method = "rf",
#              trControl = oob,
#              tuneGrid = grid,
#              metric = "ROC")

#saveRDS(rf2,"C:/AAMIM/Machine learning/TP FINAL/rf2.RDS")

rf2 <- readRDS("rf2.RDS")


y_pred <- predict(rf2, test_set %>% select(-won), type = "prob")[, 2]

#Matriz de confusion
y_pred <- ifelse(y_pred>umbral[max],1,0)
y_test <- test_set$won
conf_matrix <- table(y_test, y_pred)
metricas(conf_matrix)
y_test <- as.numeric(y_test)
y_test <- as.vector(y_test)
y_pred=as.vector(y_pred)
fb_score <- fbeta_score(y_test,y_pred, beta=0.05)
print(paste("Fb score:", round(fb_score, 3)))

# Área bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)

varImp(rf2) # Importancia de variable

# Graficamos a continuación
dotPlot(varImp(rf2)) 
dev.off() # cierra ventana 

#6) XGBoost ------

cv <- trainControl(method = "cv",
                   number = 5,
                   classProbs = TRUE,
                   verboseIter = TRUE,
                   summaryFunction = twoClassSummary)


rgrid <- random_grid(size = 10,
                     min_nrounds = 50, max_nrounds =200,
                     min_max_depth = 5, max_max_depth = 15,
                     min_eta = 0.001, max_eta = 0.1,
                     min_gamma = 0.1, max_gamma = 1,
                     min_colsample_bytree = 0.6, max_colsample_bytree = 1,
                     min_min_child_weight = 1, max_min_child_weight = 10,
                     min_subsample = 0.75, max_subsample = 1)




sum(is.na(train_set))
sum(is.na(test_set))
# eliminamos los datos faltantes debido a que no son representativos según la exploración de datos realizada
train_set <- na.omit(train_set) 
test_set <- na.omit(test_set)

# xgb <- train(won ~ .,
#              data = train_set %>% mutate(won = ifelse(won == 0, "No", "Yes")),
#              method = "xgbTree",
#              trControl = cv,
#              tuneGrid = rgrid,
#              metric = "ROC")

#saveRDS(xgb, "C:/AAMIM/Machine learning/TP FINAL/xgb.RDS")
xgb <- readRDS("C:/AAMIM/Machine learning/TP FINAL/xgb.RDS")

#Entrenamos primer modelo
y_pred <- predict(xgb, val_set %>% select(-won), type = "prob")[, 2]

#metricas
y_val <- val_set$won
y_pred <- ifelse(y_pred>0.5,1,0)
conf_matrix <- table(y_val, y_pred)
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_val ~ y_pred, plot = TRUE, print.auc = TRUE)


#~~6.1) Ajustamos umbral ------

umbral = c (0.05,0.1, 0.2,0.22,0.23,0.25,0.26,0.27, 0.28,0.29,0.3,0.33,0.4,0.5)

fb = c() #fb score para XGBOOST


for(i in 1:length(umbral)){ # recorre todos los valores del umbral
  y_pred <- predict(xgb, val_set %>% select(-won), type = "prob")[, 2]
  y_pred <- ifelse(y_pred>umbral[i],1,0)
  
  y_val <- val_set$won
  conf_matrix <- table(y_val, y_pred)
  y_val <- as.numeric(y_val)
  y_val<-as.vector(y_val)
  y_pred <- as.vector(y_pred)
  
  accuracy <- sum(diag(prop.table(conf_matrix)))
  precision <- prop.table(conf_matrix, margin = 2)[2,2]
  recall <- prop.table(conf_matrix, margin = 1)[2,2]
  fb_score <- fbeta_score(y_val,y_pred, beta=0.05)
  fb[i]  <- fb_score
  print(i)
}

# print(fb)
max <-which.max(fb)
umbral[max]

#~~6.2) Reentrenamos modelo ------
#Reentrenamos modelo con el umbral correcto: 
#sobre los datos de test

train_set_new <- rbind(train_set,val_set)
train_set_new = na.omit(train_set_new)
sum(is.na(train_set_new))

# xgb2 <- train(won ~ .,
#              data = train_set_new %>% mutate(won = ifelse(won == 0, "No", "Yes")),
#              method = "xgbTree",
#              trControl = cv,
#              tuneGrid = rgrid[max,],
#              metric = "ROC")

#saveRDS(xgb2, "C:/AAMIM/Machine learning/TP FINAL/xgb2.RDS")
xgb2 <- readRDS("C:/AAMIM/Machine learning/TP FINAL/xgb2.RDS")



y_pred <- predict(xgb2, test_set %>% select(-won), type = "prob")[, 2]

#Metricas
y_pred <- ifelse(y_pred>umbral[max],1,0)
y_test <- test_set$won
conf_matrix <- table(y_test, y_pred)
metricas(conf_matrix)
y_test <- as.numeric(y_test)
y_test <- as.vector(y_test)
y_pred=as.vector(y_pred)
fb_score <- fbeta_score(y_test,y_pred, beta=0.05)
print(paste("Fb score:", round(fb_score, 3)))

# Área bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)


