# Integrantes: Juan Spadaro; Nicolas Viñolo y Victoria Sosa

rm(list = ls()) # Borra todo el ambiente de trabajo.

#cambio el directorio sobre el que voy a trabajar
setwd('C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/')
getwd()

#Importamos las funciones que tenemos en un script aparte
source("C:/Others/Master in Management + Analytics/MATERIAS/Módulo 02/Machine Learning/TP/Horse race data TP2022/TP_ML_2022_FUNCIONES.R")


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

# Pasamos a factor las variables categóricas

raceruns$surface <- as.factor(raceruns$surface) # Factor (idem para todos los caballos en cada carrera).
raceruns$race_class <- as.factor(raceruns$race_class) # idem.
raceruns$draw <- as.factor(raceruns$draw) # Factor? (determina la posición de cada caballo en la salida).
raceruns$horse_type <- as.factor(raceruns$horse_type)

levels(raceruns$race_class) # para saber las categorias

# ~~ 2.1) Tratamiento de outliers ####

# ~~ 2.2) Tratamiento de missings (valores faltantes) ####
# % de obs con valores faltantes
cat(
  round((1-nrow(train %>% na.omit())/nrow(train))*100, 3),
  "% de las obs con valores faltantes en algún feature"
)
sapply(train, function(x) prop.table(table(missing=is.na(x))))

# Notar que muchas columnas no tienen registros y/o 
# contienen información que NO tendrías disponible a la hora de
# hacer predicciones ern timepo real con los modelos que entrenes.
# Ejemplos:
# 1) Tiene info pero no vas a poder disponer: sec_time1 -> time taken by the leader of the race to reach the end of the end of the 1st sectional point (sec)
# 2) No tienes información : position_sec1 -> position of this horse (ranking) in section 1 of the race (quizá se puede calcular con los datos)
# Tendras que descartar alguna de estas variables del análisis.


# ~~ 2.3) Tratamiento de clases desbalanceadas ####

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
raceruns <- raceruns%>%
  mutate(mean_time=(time1.x+time2.x+time3.x)/3)

# Agregamos algunas comparaciones relativas
table(raceruns$horse_age)

race_id = unique(raceruns$race_id) # id único de cada carrera.

raceruns$max.age  <- rep(NA,dim(raceruns)[1]) # Distancia respecto del caballo más antiguos 
raceruns$min.age  <- rep(NA,dim(raceruns)[1]) # Distancia respecto del caballo más joven
raceruns$n_compet <- rep(NA,dim(raceruns)[1]) # Cuanto caballos compiten
raceruns$relative_position <- rep(NA,dim(raceruns)[1]) # Posicion relativa del caballo en la carrera
raceruns$horsepower <- rep(NA,dim(raceruns)[1]) # Potencia del caballo medida como el peso total sobre el tiempo en segundos


for(i in race_id){
  sel = which(raceruns$race_id == i)
  raceruns$n_compet[sel]  <- length(sel)#-1?
  raceruns$max.age[sel]   <- max(raceruns$horse_age[sel],na.rm = T) - raceruns$horse_age[sel]
  raceruns$min.age[sel]   <- raceruns$horse_age[sel] - min(raceruns$horse_age[sel],na.rm = T)
  raceruns$relative_position[sel] <- raceruns$result[sel]/raceruns$n_compet[sel]
  raceruns$horsepower[sel] <- raceruns$declared_weight[sel]/raceruns$finish_time[sel]
  }

head(raceruns, 2)

# Agregamos 3 niveles según horse_rating
raceruns <- raceruns%>%
  mutate(rating_group=case_when(horse_rating<45~'Bajo',
                                horse_rating>=45 & horse_rating<90~'Medio',
                                horse_rating>=90 ~'Alto'))


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
raceruns$is_top3 <- ifelse(
  raceruns$result == 1 | raceruns$result == 2 | raceruns$result == 3, 1, 0)
table(raceruns$is_top3); prop.table(table(raceruns$is_top3)) # el 76% no es top3

# 3) Entrenamiento de modelos ####

# ~~~~~~~~~~~~~~~~~~~~~~~~ Dividimos los datos ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Apartamos un conjunto de testeo (20%)
set.seed(123)

train_cutoff <- as.integer(nrow(raceruns)*0.85)
train <- raceruns[1:train_cutoff,]
test <- raceruns[(train_cutoff+1):nrow(raceruns),]

# Eliminamos columnas que no queremos para el caso de estudio (spoilers)
#train <- train %>% select(-)

# Separamos matriz X de vector y
#X_train <- train %>% select(-won)
#X_test <- test %>% select(-won)

#y_train <- train %>% pull(won) %>% to_binary()
#y_test <- test %>% pull(won) %>% to_binary()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ 3.1) Modelo Baseline ####

tree <- rpart(won ~ ., data = train %>% mutate(won = factor(won)))
rpart.plot(tree)

# Matriz de confusión y accuracy
y_pred <- predict(tree, X_test)[2]
conf_matrix <- table(y_test, y_pred = round(y_pred,0))
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)


# Matriz de confusión y accuracy
y_pred <- predict(tree, test_set %>% select(-is_canceled))[,2]
y_test <- test_set$is_canceled
conf_matrix <- table(y_test, y_pred = round(y_pred,0))
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ 3.2) Regresión logística ####

logit_reg <- glm(won ~ ., data = train, family = binomial)
summary(logit_reg)

y_pred <- predict(logit_reg, X_test, type = "response")

# Matriz de confusión y accuracy
conf_matrix <- table(y_test, y_pred = round(y_pred, 0))
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)


logit_reg <- glm(is_canceled ~ ., data = train_set, family = "binomial")

# Matriz de confusión y accuracy
y_pred <- predict(logit_reg, test_set %>% select(-is_canceled), type = "response")
y_test <- test_set$is_canceled





#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ 3.3) Regresión logística ####

add_poly <- function(X, degrees) {
  lapply(degrees, function(degree) {
    X <- X[, sapply(X, is.numeric)]
    X <- apply(X, 2, function(x) x^degree) %>% as.data.frame()
    return(X)
  }) %>% bind_cols()
}

train_poly <- cbind(train, add_poly(train, 2:6))
test_poly <- cbind(test, add_poly(test, 2:6))

X_train_mat <- model.matrix(~.-1, train_poly %>% select(-won))
X_test_mat <- model.matrix(~.-1, test_poly %>% select(-won))

lasso_cv <- cv.glmnet(X_train_mat, y_train, family = binomial, alpha = 1)
best_lambda <- lasso_cv$lambda.min
best_lambda
plot(lasso_cv)

lasso_reg <- glmnet(X_train_mat,
                    y_train,
                    alpha = 1,
                    lambda = best_lambda,
                    family = "binomial")
coef(lasso_reg)

## Performance
preds <- predict(lasso_reg,
                 s = best_lambda,
                 newx = X_test_mat,
                 type = "response")[,1]

# Matriz de confusión y accuracy
conf_matrix <- table(y_test, y_pred = round(y_pred,0))
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_test ~ preds, plot = TRUE, print.auc = TRUE)

ggplot(data = data.frame(Churn = ifelse(y_test == 1, "Sí", "No"), y_hat = preds)) +
  geom_density(aes(x = y_hat, fill = Churn), alpha =  0.8) +
  theme_minimal() + 
  scale_fill_manual(values = c("#73777B", "#EC994B")) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold")) +
  labs(x = "Probabilidad predicha",
       y = "Densidad",
       title = "Distribución de las predicciones") +
  xlim(0,1)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ Umbral Óptimo ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

costs = matrix(c(0, 4400, 510, 3435), nrow = 2)

cutoffs <- seq(0, 1, 0.01)
total_costs <- c()
for(cutoff in cutoffs) {
  y_pred <- factor(ifelse(preds < cutoff, 0, 1), levels = c(0, 1))
  conf_matrix <- prop.table(table(y_test, y_pred))
  total_costs <- c(total_costs, sum(costs * conf_matrix))
}

plot(cutoffs, total_costs, type = 'l',
     xlab = "Umbral de corte",
     ylab = "Costo total",
     main = "Costo total según umbral de corte")
optimal_cutoff <- cutoffs[which.min(total_costs)]
abline(v = optimal_cutoff, col = "red", lty = 2)

y_pred <- ifelse(preds < optimal_cutoff, 0, 1)
conf_matrix <- table(y_test, y_pred)
metricas(conf_matrix)

min(total_costs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ 3.4) Random Forest ####

oob <- trainControl(method = "oob",
                    classProbs = TRUE,
                    verboseIter = TRUE)
grid <- data.frame(mtry = seq(2, 16, 2))
rf <- train(won ~ ., 
            data = train_set %>% mutate(won = ifelse(won == 0, "No", "Yes")), 
            method = "rf", 
            trControl = oob,
            tuneGrid = grid,
            metric = "Accuracy")

# rf <- readRDS("rf.RDS")

# Matriz de confusión y accuracy
y_pred <- predict(rf, test_set %>% select(-won), type = "prob")[, 2]
y_test <- test_set$won
conf_matrix <- table(y_test, y_pred = round(y_pred,0))
metricas(conf_matrix)

# Área bajo la curva de ROC
roc(y_test ~ y_pred, plot = TRUE, print.auc = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ~~ 3.5) XGBoost ####













