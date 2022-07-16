# Integrantes: Juan Spadaro; Nicolas Viñolo y Victoria Sosa

rm(list = ls()) # Borra todo el ambiente de trabajo.

#cambio el directorio sobre el que voy a trabajar
#setwd('C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/')
#setwd('C:/AAMIM/Machine learning/TP FINAL')
setwd('/Users/victoriasosa/Documents/MiM/2. Machine Learning/tp/Horse race data TP2022')

getwd()

#Importamos las funciones que tenemos en un script aparte
#source("C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/TP_ML_2022_FUNCIONES.R")
#source("C:/AAMIM/Machine learning/TP FINAL/functions.R")
source('/Users/victoriasosa/Documents/MiM/2. Machine Learning/tp/functions.R')


# Librerias ####
library(tidyverse) # tiene ggplot, dplyr, tidyr, y otros
library(Rmisc)
library(boot)
library(ggplot2)
library(stargazer)
library(scales)
library(glmnet)
library(rpart)
library(ggthemes)
library(dplyr)
library(rpart.plot)
library(pROC)

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
X = raceruns

race_id = unique(X[,1])

X = model.matrix(~.,data = X)[ , -1]

##~~~~~~~~~~~~~ Identificamos training, validation y testing ~~~~~~~~~~~~~
raceruns$train_val_test <- ifelse(raceruns$date >= strptime("2004-01-05", format = "%Y-%m-%d", tz = "UTC"), "test", "train")

raceruns[(which(raceruns$train_val_test == "train" & raceruns$date >= strptime("2002-08-05", format = "%Y-%m-%d", tz = "UTC"))), "train_val_test"] <- "valid"

prop.table(table(raceruns$train_val_test))


train_set <- raceruns %>% filter(train_val_test== "train") %>% select(-train_val_test)
val_set <- raceruns %>% filter(train_val_test== "valid") %>% select(-train_val_test)
test_set <- raceruns %>% filter(train_val_test== "test") %>% select(-train_val_test)

# Separamos matriz X de vector y
X_train <- train_set %>% select(-won)
X_val <- val_set %>% select(-won)
X_test <- test_set %>% select(-won)

prueba <- function(vect) {
  sapply(vect, function(x) ifelse(x == 1, 1 ,0))
}

y_train <- train_set %>% select(won)
y_val <- val_set %>% select(won)%>% prueba()
y_test <- test_set %>% select(won) %>% prueba()


#train.id = which(X[,1]<3001) # primeras 3000 carreras de train y Ãºtimas 3128 de test.

####Modelo Baselin
tree <- rpart(won ~ ., data = train_set)
rpart.plot(tree)

# Matriz de confusión y accuracy
y_pred <- predict(tree, X_val)[,2]
conf_matrix <- table(y_val, y_pred = round(y_pred,0))
metricas(conf_matrix)

table(y_pred, y_val)

# Área bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ 3.2) Regresión logística ####

# Vamos a fitear un modelo logÃ­stico basico.
#

######## RegresiÃ³n logÃ­stica SIN regularizaciÃ³n:

logit = glmnet( x=data.matrix(X[,-2]), #attributes(X) # Columna 2 es variable "won"
                y = as.matrix(X[, 2]), 
                family  = 'binomial',
                subset  = train.id,
                lambda = 0,  
                alpha = 1)

pred <- predict(logit, s = 0 , newx = X[,-2], type = 'response')
head(pred,3) # Probabilidades estimadas de ganar la carrera.

x11() ; par(mfrow = c(1,2)) # El modelo tiene sentido.
plot(X[train.id,45], pred[train.id], type = 'p', pch = 20,
     xlab = 'win odds', ylab = 'Prob. Est. Ganar', main = 'Train')
plot(X[-train.id,45],  pred[-train.id], type = 'p', pch = 20,
     xlab = 'win odds', ylab = 'Prob. Est. Ganar', main = 'Test')


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
# ~~~~~~~~~~~~~~~~~~~~~~~~ Dividimos los datos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Apartamos un conjunto de testeo (15%)
set.seed(123)

train_cutoff <- as.integer(nrow(raceruns)*0.85)
train_set <- raceruns[1:train_cutoff,]
test_set <- raceruns[(train_cutoff+1):nrow(raceruns),]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ 3.4) Random Forest ####

oob <- trainControl(method = "oob",
                    classProbs = TRUE,
                    verboseIter = TRUE)

grid <- data.frame(mtry = seq(1, 3, 1))

rf <- train(won ~ ., 
            data = train_set %>% mutate(won = ifelse(won == 0, "No", "Yes")), 
            method = "rf", 
            trControl = oob,
            tuneGrid = grid,
            metric = "Accuracy")

#saveRDS(rf, "C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/rf.RDS")
#rf <- readRDS("C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/rf.RDS")


# Matriz de confusión y accuracy
y_pred <- predict(rf, test_set %>% select(-won), type = "prob")[, 2]
y_test <- test_set$won
conf_matrix <- table(y_test, y_pred = round(y_pred,0))
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
                         subsample = c(0.5, 0.75, 1.0)) %>% sample_n(30)

#Solo tomo algunas de las variables del data set.
train_set <- train_set[,c(1,5,7:10,12:14,34,35,41:45,74:77)]
test_set  <- test_set[,c(1,5,7:10,12:14,34,35,41:45,74:77)]
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












