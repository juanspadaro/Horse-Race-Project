library(ggplot2)
library(tidyverse)
library(dplyr)
options(scipen=999) # sin notacion cientifica
# GRÁFICO_1 ####
##Vemos los valores NA

position_sec4<-mean(is.na(raceruns$position_sec4))
position_sec4<-as.data.frame(position_sec4) 

position_sec5<-mean(is.na(raceruns$position_sec5))
position_sec5<-as.data.frame(position_sec5)

position_sec6<-mean(is.na(raceruns$position_sec6))
position_sec6<-as.data.frame(position_sec6)

behind_sec4<-mean(is.na(raceruns$behind_sec4))
behind_sec4<-as.data.frame(behind_sec4)

behind_sec5<-mean(is.na(raceruns$behind_sec5))
behind_sec5<-as.data.frame(behind_sec5)

behind_sec6<-mean(is.na(raceruns$behind_sec6))
behind_sec6<-as.data.frame(behind_sec6)

time4.x<-mean(is.na(raceruns$time4.x))
time4.x<-as.data.frame(time4.x)

time5.x<-mean(is.na(raceruns$time5.x))
time5.x<-as.data.frame(time5.x)

time6.x<-mean(is.na(raceruns$time6.x))
time6.x<-as.data.frame(time6.x)

place_odds<-mean(is.na(raceruns$place_odds))
place_odds<-as.data.frame(place_odds)

prize<-mean(is.na(raceruns$prize))
prize<-as.data.frame(prize)

sec_time4<-mean(is.na(raceruns$sec_time4))
sec_time4<-as.data.frame(sec_time4)

sec_time5<-mean(is.na(raceruns$sec_time5))
sec_time5<-as.data.frame(sec_time5)

sec_time6<-mean(is.na(raceruns$sec_time6))
sec_time6<-as.data.frame(sec_time6)

sec_time7<-mean(is.na(raceruns$sec_time7))
sec_time7<-as.data.frame(sec_time7)

time4.y<-mean(is.na(raceruns$time4.y))
time4.y<-as.data.frame(time4.y)

time5.y<-mean(is.na(raceruns$time5.y))
time5.y<-as.data.frame(time5.y)

time6.y<-mean(is.na(raceruns$time6.y))
time6.y<-as.data.frame(time6.y)

time7<-mean(is.na(raceruns$time7))
time7<-as.data.frame(time7)

place_combination3<-mean(is.na(raceruns$place_combination3))
place_combination3<-as.data.frame(place_combination3)

place_combination4<-mean(is.na(raceruns$place_combination4))
place_combination4<-as.data.frame(place_combination4)

place_dividend3<-mean(is.na(raceruns$place_dividend3))
place_dividend3<-as.data.frame(place_dividend3)

place_dividend4<-mean(is.na(raceruns$place_dividend4))
place_dividend4<-as.data.frame(place_dividend4)

win_combination2<-mean(is.na(raceruns$win_combination2))
win_combination2<-as.data.frame(win_combination2)

win_dividend2<-mean(is.na(raceruns$win_dividend2))
win_dividend2<-as.data.frame(win_dividend2)


#Consolidamos la data
data_graf1 <- cbind(position_sec4,position_sec5,position_sec6,behind_sec4,behind_sec5,
              behind_sec6,time4.x,time5.x,time6.x,place_odds,prize,sec_time4,
              sec_time5,sec_time6,sec_time7,time4.y,time5.y,time6.y,time7,
              place_combination3,place_combination4,place_dividend3,place_dividend4,
              win_combination2,win_dividend2)

rm(position_sec4,position_sec5,position_sec6,behind_sec4,behind_sec5,
   behind_sec6,time4.x,time5.x,time6.x,place_odds,prize,sec_time4,
   sec_time5,sec_time6,sec_time7,time4.y,time5.y,time6.y,time7,
   place_combination3,place_combination4,place_dividend3,place_dividend4,
   win_combination2,win_dividend2)

gc()

#Realizamos un dataframe de los valores NA y graficamos

data_graf1 <- data_graf1 %>%
  pivot_longer(cols = 1:25,names_to = "Variable",values_to = "Porcentaje de valores NA")

data_graf1 <- data_graf1[with(data_graf1, order(data_graf1$`Porcentaje de valores NA`)), ]

#Graficamos
x11(); data_graf1 %>%
  ggplot(aes(x=Variable, y= `Porcentaje de valores NA`)) +
  geom_bar(stat="identity", fill="red", alpha=.6, width=.4) +
  geom_text(position = position_stack(vjust = 0.8), size = 2.8, 
            label= sprintf("%2.2f%%", 100 *data_graf1$`Porcentaje de valores NA`)) +
  ggtitle("Porcentaje de valores NA según cada variable") +
  coord_flip() +
  xlab("") +
  theme_bw()

# GRÁFICO_2 ####
jinetes_ganadores <- raceruns %>%
  filter(won == 1) %>%
  group_by(jockey_id) %>%
  summarise(cant_carreras_ganadas = sum(won)) %>%
  select(jockey_id, cant_carreras_ganadas)

x11(); jinetes_ganadores %>%
  ggplot(aes(x=cant_carreras_ganadas)) +
  geom_histogram(binwidth=20, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Distribución de cantidad de carreras ganadas") +
  labs(x='Cantidad de carreras ganadas', y='Cantidad de Jinetes') +
  theme_bw()

# GRÁFICO_3 ####
caballos_ganadores <- raceruns %>%
  filter(won == 1) %>%
  group_by(horse_id) %>%
  summarise(Cant_carreras_ganadas = sum(won)) %>%
  select(horse_id, Cant_carreras_ganadas)

x11(); caballos_ganadores %>%
  ggplot(aes(x=Cant_carreras_ganadas)) +
  geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Distribución de cantidad de carreras ganadas") +
  labs(x='Cantidad de carreras ganadas', y='Cantidad de caballos') +
  scale_x_continuous(breaks = seq(0, 13)) +
  theme_bw()

caballos_perdedores <- raceruns %>%
  filter(won == 0) %>% mutate(won = 1) %>%
  group_by(horse_id) %>%
  summarise(Cant_carreras_perdidas = sum(won)) %>%
  select(horse_id, Cant_carreras_perdidas)

x11(); caballos_perdedores %>%
  ggplot(aes(x=Cant_carreras_perdidas)) +
  geom_histogram(binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Distribución de cantidad de carreras perdidas") +
  labs(x='Cantidad de carreras perdidas', y='Cantidad de caballos') +
  scale_x_continuous(limits = c(0, 75)) +
  theme_bw()

# GRÁFICO_4 ####

x11(); raceruns%>%ggplot( aes(x=prize)) +
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribución del premio") +
  labs(x='Prize', y='Density') +
  scale_x_continuous(limits = c(400000, 2000000)) +
  theme_bw()

# GRÁFICO_5 ####
caballos_ganadoresII <- raceruns %>%
  filter(won == 1) %>%
  group_by(horse_id, horse_age, weight_ratio) %>%
  summarise(Cant_carreras_ganadas = sum(won)) 

x11(); ggplot(caballos_ganadoresII, aes(x=weight_ratio)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribución del Weight Ratio") +
  labs(x='Weight Ratio de los caballos ganadores', y='Density') +
  scale_x_continuous(limits = c(0.075,0.15))


caballos_perdedoresII <- raceruns %>%
  filter(won == 0) %>%
  group_by(horse_id, horse_age, weight_ratio) %>%
  summarise(Cant_carreras_perdidas = sum(won)) 

x11(); ggplot(caballos_perdedoresII, aes(x=weight_ratio)) + 
  geom_density(fill="#69b3a2", color="#e9ecef", alpha=0.8) +
  ggtitle("Distribución del Weight Ratio") +
  labs(x='Weight Ratio de los caballos perdedores', y='Density') +
  scale_x_continuous(limits = c(0.075,0.15))

# GRÁFICO_6 #### 

table(raceruns$draw); prop.table(table(raceruns$draw)) # descartamos el 15 ya que solo tiene una sola observación

#Proporcion de caballos ganadores para cada draw
prop_caballos_ganadores_draw1 <- nrow(raceruns %>% filter(won == 1 & draw == 1))/nrow(raceruns %>% filter(draw == 1))
prop_caballos_ganadores_draw2 <- nrow(raceruns %>% filter(won == 1 & draw == 2))/nrow(raceruns %>% filter(draw == 2))
prop_caballos_ganadores_draw3 <- nrow(raceruns %>% filter(won == 1 & draw == 3))/nrow(raceruns %>% filter(draw == 3))
prop_caballos_ganadores_draw4 <- nrow(raceruns %>% filter(won == 1 & draw == 4))/nrow(raceruns %>% filter(draw == 4))
prop_caballos_ganadores_draw5 <- nrow(raceruns %>% filter(won == 1 & draw == 5))/nrow(raceruns %>% filter(draw == 5))
prop_caballos_ganadores_draw6 <- nrow(raceruns %>% filter(won == 1 & draw == 6))/nrow(raceruns %>% filter(draw == 6))
prop_caballos_ganadores_draw7 <- nrow(raceruns %>% filter(won == 1 & draw == 7))/nrow(raceruns %>% filter(draw == 7))
prop_caballos_ganadores_draw8 <- nrow(raceruns %>% filter(won == 1 & draw == 8))/nrow(raceruns %>% filter(draw == 8))
prop_caballos_ganadores_draw9 <- nrow(raceruns %>% filter(won == 1 & draw == 9))/nrow(raceruns %>% filter(draw == 9))
prop_caballos_ganadores_draw10 <- nrow(raceruns %>% filter(won == 1 & draw == 10))/nrow(raceruns %>% filter(draw == 10))
prop_caballos_ganadores_draw11 <- nrow(raceruns %>% filter(won == 1 & draw == 11))/nrow(raceruns %>% filter(draw == 11))
prop_caballos_ganadores_draw12 <- nrow(raceruns %>% filter(won == 1 & draw == 12))/nrow(raceruns %>% filter(draw == 12))
prop_caballos_ganadores_draw13 <- nrow(raceruns %>% filter(won == 1 & draw == 13))/nrow(raceruns %>% filter(draw == 13))
prop_caballos_ganadores_draw14 <- nrow(raceruns %>% filter(won == 1 & draw == 14))/nrow(raceruns %>% filter(draw == 14))

#Proporcion de caballos perdedores para cada draw
prop_caballos_perdedores_draw1 <- nrow(raceruns %>% filter(won == 0 & draw == 1))/nrow(raceruns %>% filter(draw == 1))
prop_caballos_perdedores_draw2 <- nrow(raceruns %>% filter(won == 0 & draw == 2))/nrow(raceruns %>% filter(draw == 2))
prop_caballos_perdedores_draw3 <- nrow(raceruns %>% filter(won == 0 & draw == 3))/nrow(raceruns %>% filter(draw == 3))
prop_caballos_perdedores_draw4 <- nrow(raceruns %>% filter(won == 0 & draw == 4))/nrow(raceruns %>% filter(draw == 4))
prop_caballos_perdedores_draw5 <- nrow(raceruns %>% filter(won == 0 & draw == 5))/nrow(raceruns %>% filter(draw == 5))
prop_caballos_perdedores_draw6 <- nrow(raceruns %>% filter(won == 0 & draw == 6))/nrow(raceruns %>% filter(draw == 6))
prop_caballos_perdedores_draw7 <- nrow(raceruns %>% filter(won == 0 & draw == 7))/nrow(raceruns %>% filter(draw == 7))
prop_caballos_perdedores_draw8 <- nrow(raceruns %>% filter(won == 0 & draw == 8))/nrow(raceruns %>% filter(draw == 8))
prop_caballos_perdedores_draw9 <- nrow(raceruns %>% filter(won == 0 & draw == 9))/nrow(raceruns %>% filter(draw == 9))
prop_caballos_perdedores_draw10 <- nrow(raceruns %>% filter(won == 0 & draw == 10))/nrow(raceruns %>% filter(draw == 10))
prop_caballos_perdedores_draw11 <- nrow(raceruns %>% filter(won == 0 & draw == 11))/nrow(raceruns %>% filter(draw == 11))
prop_caballos_perdedores_draw12 <- nrow(raceruns %>% filter(won == 0 & draw == 12))/nrow(raceruns %>% filter(draw == 12))
prop_caballos_perdedores_draw13 <- nrow(raceruns %>% filter(won == 0 & draw == 13))/nrow(raceruns %>% filter(draw == 13))
prop_caballos_perdedores_draw14 <- nrow(raceruns %>% filter(won == 0 & draw == 14))/nrow(raceruns %>% filter(draw == 14))

proporciones <- data.frame(
  Draw = factor(c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)),
  "Prop_caballos_ganadores" = c(prop_caballos_ganadores_draw1[1],
                                prop_caballos_ganadores_draw2[1],
                                prop_caballos_ganadores_draw3[1],
                                prop_caballos_ganadores_draw4[1],
                                prop_caballos_ganadores_draw5[1],
                                prop_caballos_ganadores_draw6[1],
                                prop_caballos_ganadores_draw7[1],
                                prop_caballos_ganadores_draw8[1],
                                prop_caballos_ganadores_draw9[1],
                                prop_caballos_ganadores_draw10[1],
                                prop_caballos_ganadores_draw11[1],
                                prop_caballos_ganadores_draw12[1],
                                prop_caballos_ganadores_draw13[1],
                                prop_caballos_ganadores_draw14[1]),
  "Prop_caballos_perdedores" = c(prop_caballos_perdedores_draw1[1],
                                 prop_caballos_perdedores_draw2[1],
                                 prop_caballos_perdedores_draw3[1],
                                 prop_caballos_perdedores_draw4[1],
                                 prop_caballos_perdedores_draw5[1],
                                 prop_caballos_perdedores_draw6[1],
                                 prop_caballos_perdedores_draw7[1],
                                 prop_caballos_perdedores_draw8[1],
                                 prop_caballos_perdedores_draw9[1],
                                 prop_caballos_perdedores_draw10[1],
                                 prop_caballos_perdedores_draw11[1],
                                 prop_caballos_perdedores_draw12[1],
                                 prop_caballos_perdedores_draw13[1],
                                 prop_caballos_perdedores_draw14[1]))

proporciones <- proporciones %>%
  rename('Ganadores'     = Prop_caballos_ganadores,
         'Perdedores'   = Prop_caballos_perdedores) %>%
  pivot_longer(cols = 2:3,names_to = "Tipo",values_to = "Proporcion")

#limpiamos el ambiente
rm(prop_caballos_ganadores_draw1,prop_caballos_ganadores_draw2,prop_caballos_ganadores_draw3,prop_caballos_ganadores_draw4,
   prop_caballos_ganadores_draw5,prop_caballos_ganadores_draw6,prop_caballos_ganadores_draw7,prop_caballos_ganadores_draw8,
   prop_caballos_ganadores_draw9,prop_caballos_ganadores_draw10,prop_caballos_ganadores_draw11,prop_caballos_ganadores_draw12,
   prop_caballos_ganadores_draw13,prop_caballos_ganadores_draw14,prop_caballos_perdedores_draw1,prop_caballos_perdedores_draw2,
   prop_caballos_perdedores_draw3,prop_caballos_perdedores_draw4,prop_caballos_perdedores_draw5,prop_caballos_perdedores_draw6,
   prop_caballos_perdedores_draw7,prop_caballos_perdedores_draw8,prop_caballos_perdedores_draw9,prop_caballos_perdedores_draw10,
   prop_caballos_perdedores_draw11,prop_caballos_perdedores_draw12,prop_caballos_perdedores_draw13,prop_caballos_perdedores_draw14)

gc()

# Graficamos
x11(); ggplot(proporciones, aes(x=Draw, y= Proporcion, fill = Tipo,
                                label = sprintf("%1.2f%%", 100 *Proporcion))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#4D7CA8", "#9CB8D3")) +
  geom_text(position = position_stack(vjust = 0.4), size = 2.8) +
  labs(title = "Proporción de caballos ganadores y perdedores según draw",
       x='Draw', y='Proporción') +
  theme(legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill =  "white")) +
  coord_flip()


# GRÁFICO_7 ####
x11(); raceruns%>% 
  ggplot() +  
  geom_smooth(aes(x = day, y = prize/1000), show.legend = FALSE) + 
  scale_x_continuous(limits = c(1,31))  +
  labs(title = "Evolución promedio del premio por día",
       x='Days', y='Premio/1000')

# GRÁFICO_8 ####
x11(); raceruns%>% 
  ggplot() +  
  geom_smooth(aes(x = month, y = prize/1000), show.legend = FALSE) + 
  scale_x_continuous(breaks = seq(0, 12)) +
  labs(title = "Evolución promedio del premio por mes",
       x='Mes', y='Premio/1000')

# GRÁFICO_9 ####

x11(); ggplot(raceruns, aes(x=year, y=prize/1000)) + 
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(1997, 2005)) +
  labs(title = "Evolución promedio del premio por año",
       x='Año', y='Premio/1000')

# GRÁFICO_10 ####
table(raceruns$horse_age); prop.table(table(raceruns$horse_age))

#Proporcion de caballos ganadores para cada edad
prop_caballos_ganadores_2anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 2))/nrow(raceruns %>% filter(horse_age == 2))
prop_caballos_ganadores_3anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 3))/nrow(raceruns %>% filter(horse_age == 3))
prop_caballos_ganadores_4anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 4))/nrow(raceruns %>% filter(horse_age == 4))
prop_caballos_ganadores_5anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 5))/nrow(raceruns %>% filter(horse_age == 5))
prop_caballos_ganadores_6anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 6))/nrow(raceruns %>% filter(horse_age == 6))
prop_caballos_ganadores_7anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 7))/nrow(raceruns %>% filter(horse_age == 7))
prop_caballos_ganadores_8anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 8))/nrow(raceruns %>% filter(horse_age == 8))
prop_caballos_ganadores_9anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 9))/nrow(raceruns %>% filter(horse_age == 9))
prop_caballos_ganadores_10anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 10))/nrow(raceruns %>% filter(horse_age == 10))

#Proporcion de caballos perdedores para cada edad
prop_caballos_perdedores_2anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 2))/nrow(raceruns %>% filter(horse_age == 2))
prop_caballos_perdedores_3anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 3))/nrow(raceruns %>% filter(horse_age == 3))
prop_caballos_perdedores_4anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 4))/nrow(raceruns %>% filter(horse_age == 4))
prop_caballos_perdedores_5anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 5))/nrow(raceruns %>% filter(horse_age == 5))
prop_caballos_perdedores_6anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 6))/nrow(raceruns %>% filter(horse_age == 6))
prop_caballos_perdedores_7anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 7))/nrow(raceruns %>% filter(horse_age == 7))
prop_caballos_perdedores_8anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 8))/nrow(raceruns %>% filter(horse_age == 8))
prop_caballos_perdedores_9anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 9))/nrow(raceruns %>% filter(horse_age == 9))
prop_caballos_perdedores_10anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 10))/nrow(raceruns %>% filter(horse_age == 10))


proporciones <- data.frame(
  Edad = factor(c(2,3,4,5,6,7,8,9,10)),
  "Prop_caballos_ganadores" = c(prop_caballos_ganadores_2anios[1],
                                        prop_caballos_ganadores_3anios[1],
                                        prop_caballos_ganadores_4anios[1],
                                        prop_caballos_ganadores_5anios[1],
                                        prop_caballos_ganadores_6anios[1],
                                        prop_caballos_ganadores_7anios[1],
                                        prop_caballos_ganadores_8anios[1],
                                        prop_caballos_ganadores_9anios[1],
                                        prop_caballos_ganadores_10anios[1]),
  "Prop_caballos_perdedores" = c(prop_caballos_perdedores_2anios[1],
                                        prop_caballos_perdedores_3anios[1],
                                        prop_caballos_perdedores_4anios[1],
                                        prop_caballos_perdedores_5anios[1],
                                        prop_caballos_perdedores_6anios[1],
                                        prop_caballos_perdedores_7anios[1],
                                        prop_caballos_perdedores_8anios[1],
                                        prop_caballos_perdedores_9anios[1],
                                        prop_caballos_perdedores_10anios[1]))


#limpiamos el ambiente
rm(prop_caballos_ganadores_2anios,prop_caballos_ganadores_3anios,prop_caballos_ganadores_4anios,
   prop_caballos_ganadores_5anios,prop_caballos_ganadores_6anios,prop_caballos_ganadores_7anios,
   prop_caballos_ganadores_8anios,prop_caballos_ganadores_9anios,prop_caballos_ganadores_10anios,
   prop_caballos_perdedores_2anios,prop_caballos_perdedores_3anios,prop_caballos_perdedores_4anios,
   prop_caballos_perdedores_5anios,prop_caballos_perdedores_6anios,prop_caballos_perdedores_7anios,
   prop_caballos_perdedores_8anios,prop_caballos_perdedores_9anios,prop_caballos_perdedores_10anios)

gc()

# Graficamos
x11(); ggplot(proporciones, aes(x=Edad, y=Prop_caballos_ganadores)) +
  geom_segment( aes(x=Edad, y=Prop_caballos_ganadores, xend=Edad, yend=0), color="grey") +
  geom_point( color="orange", size=4) +
  geom_text(position = position_stack(vjust = 0.8), size = 2.8, 
            label= sprintf("%2.2f%%", 100 *proporciones$Prop_caballos_ganadores)) +
  labs(title = "Proporción de caballos ganadores según la edad",
       x='Edad', y='Proporción')


x11(); ggplot(proporciones, aes(x=Edad, y=Prop_caballos_perdedores)) +
  geom_segment( aes(x=Edad, y=Prop_caballos_perdedores, xend=Edad, yend=0), color="grey") +
  geom_point( color="orange", size=4) +
  geom_text(position = position_stack(vjust = 0.8), size = 2.8, 
            label= sprintf("%2.2f%%", 100 *proporciones$Prop_caballos_perdedores)) +
  labs(title = "Proporción de caballos perdedores según la edad",
       x='Edad', y='Proporción')


#OJO con los que tienen 10 anios, son muy pocos!
table(raceruns$horse_age); prop.table(table(raceruns$horse_age))




#  otra opcion (no me gusta como queda, la dejo por las dudas)
  # proporciones %>%  
  # ggplot() +  
  # aes(x = Edad,  
  #     y = Proporcion,
  #     group = Tipo,
  #     color = Tipo) +  
  # geom_line() +
  # geom_point() +
  # labs(title = "Proporción de caballos ganadores y perdedores según la edad",
  #      x='Edad', y='Proporción') + 
  # theme(legend.title = element_blank(),
  #       legend.position = "bottom",
  #       legend.background = element_rect(fill =  "white")) 
  
# GRÁFICO_11 (misma info que el 10, diferente visualización) ####
table(raceruns$horse_age); prop.table(table(raceruns$horse_age))

#Proporcion de caballos ganadores para cada edad
prop_caballos_ganadores_2anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 2))/nrow(raceruns %>% filter(horse_age == 2))
prop_caballos_ganadores_3anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 3))/nrow(raceruns %>% filter(horse_age == 3))
prop_caballos_ganadores_4anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 4))/nrow(raceruns %>% filter(horse_age == 4))
prop_caballos_ganadores_5anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 5))/nrow(raceruns %>% filter(horse_age == 5))
prop_caballos_ganadores_6anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 6))/nrow(raceruns %>% filter(horse_age == 6))
prop_caballos_ganadores_7anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 7))/nrow(raceruns %>% filter(horse_age == 7))
prop_caballos_ganadores_8anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 8))/nrow(raceruns %>% filter(horse_age == 8))
prop_caballos_ganadores_9anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 9))/nrow(raceruns %>% filter(horse_age == 9))
prop_caballos_ganadores_10anios <- nrow(raceruns %>% filter(won == 1 & horse_age == 10))/nrow(raceruns %>% filter(horse_age == 10))

#Proporcion de caballos perdedores para cada edad
prop_caballos_perdedores_2anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 2))/nrow(raceruns %>% filter(horse_age == 2))
prop_caballos_perdedores_3anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 3))/nrow(raceruns %>% filter(horse_age == 3))
prop_caballos_perdedores_4anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 4))/nrow(raceruns %>% filter(horse_age == 4))
prop_caballos_perdedores_5anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 5))/nrow(raceruns %>% filter(horse_age == 5))
prop_caballos_perdedores_6anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 6))/nrow(raceruns %>% filter(horse_age == 6))
prop_caballos_perdedores_7anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 7))/nrow(raceruns %>% filter(horse_age == 7))
prop_caballos_perdedores_8anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 8))/nrow(raceruns %>% filter(horse_age == 8))
prop_caballos_perdedores_9anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 9))/nrow(raceruns %>% filter(horse_age == 9))
prop_caballos_perdedores_10anios <- nrow(raceruns %>% filter(won == 0 & horse_age == 10))/nrow(raceruns %>% filter(horse_age == 10))


proporciones <- data.frame(
  Edad = factor(c(2,3,4,5,6,7,8,9,10)),
  "Prop_caballos_ganadores" = c(prop_caballos_ganadores_2anios[1],
                                prop_caballos_ganadores_3anios[1],
                                prop_caballos_ganadores_4anios[1],
                                prop_caballos_ganadores_5anios[1],
                                prop_caballos_ganadores_6anios[1],
                                prop_caballos_ganadores_7anios[1],
                                prop_caballos_ganadores_8anios[1],
                                prop_caballos_ganadores_9anios[1],
                                prop_caballos_ganadores_10anios[1]),
  "Prop_caballos_perdedores" = c(prop_caballos_perdedores_2anios[1],
                                 prop_caballos_perdedores_3anios[1],
                                 prop_caballos_perdedores_4anios[1],
                                 prop_caballos_perdedores_5anios[1],
                                 prop_caballos_perdedores_6anios[1],
                                 prop_caballos_perdedores_7anios[1],
                                 prop_caballos_perdedores_8anios[1],
                                 prop_caballos_perdedores_9anios[1],
                                 prop_caballos_perdedores_10anios[1]))

proporciones <- proporciones %>%
  rename('Ganadores'     = Prop_caballos_ganadores,
         'Perdedores'   = Prop_caballos_perdedores) %>%
  pivot_longer(cols = 2:3,names_to = "Tipo",values_to = "Proporcion")

#limpiamos el ambiente
rm(prop_caballos_ganadores_2anios,prop_caballos_ganadores_3anios,prop_caballos_ganadores_4anios,
   prop_caballos_ganadores_5anios,prop_caballos_ganadores_6anios,prop_caballos_ganadores_7anios,
   prop_caballos_ganadores_8anios,prop_caballos_ganadores_9anios,prop_caballos_ganadores_10anios,
   prop_caballos_perdedores_2anios,prop_caballos_perdedores_3anios,prop_caballos_perdedores_4anios,
   prop_caballos_perdedores_5anios,prop_caballos_perdedores_6anios,prop_caballos_perdedores_7anios,
   prop_caballos_perdedores_8anios,prop_caballos_perdedores_9anios,prop_caballos_perdedores_10anios)

gc()

# Graficamos
x11(); ggplot(proporciones, aes(x=Edad, y= Proporcion, fill = Tipo,
                                 label = sprintf("%1.2f%%", 100 *Proporcion))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("#6EB05A", "#DBE8B4")) +
  geom_text(position = position_stack(vjust = 0.4), size = 2.8) +
  labs(title = "Proporción de caballos ganadores y perdedores según la edad",
       x='Edad', y='Proporción') +
  theme(legend.text = element_text(size = 10),
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = element_rect(fill =  "white")) +
  coord_flip()


#OJO con los que tienen 10 anios, son muy pocos!
table(raceruns$horse_age); prop.table(table(raceruns$horse_age))


# GRÁFICO_12 ####

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~ con la variable actual_weight ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
resultado_vs_peso <- raceruns %>%
  select(result, actual_weight) %>%
  group_by(result) %>%
  summarise(Media = mean(actual_weight),
            Mediana = median(actual_weight))

#con la media
x11(); ggplot(resultado_vs_peso, aes(x=result, y=Media)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la media de actual_weight",
       x='Resultado', y='Media de actual_weight') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#con la mediana
x11(); ggplot(resultado_vs_peso, aes(x=result, y=Mediana)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la mediana de actual_weight",
       x='Resultado', y='Mediana de actual_weight') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~ con la variable declared_weight ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
resultado_vs_peso <- raceruns %>%
  select(result, declared_weight) %>%
  group_by(result) %>%
  summarise(Media = mean(declared_weight),
            Mediana = median(declared_weight))

#con la media
x11(); ggplot(resultado_vs_peso, aes(x=result, y=Media)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la media de declared_weight",
       x='Resultado', y='Media de declared_weight') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#con la mediana
x11(); ggplot(resultado_vs_peso, aes(x=result, y=Mediana)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la mediana de declared_weight",
       x='Resultado', y='Mediana de declared_weight') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~ con la variable horse_age ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
resultado_vs_edad <- raceruns %>%
  select(result, horse_age) %>%
  group_by(result) %>%
  summarise(Media = mean(horse_age),
            Mediana = median(horse_age))

#con la media
x11(); ggplot(resultado_vs_edad, aes(x=result, y=Media)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la media de horse_age",
       x='Resultado', y='Media de horse_age') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#con la mediana
x11(); ggplot(resultado_vs_edad, aes(x=result, y=Mediana)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la mediana de horse_age",
       x='Resultado', y='Mediana de horse_age') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~ con la variable weight_ratio ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
resultado_vs_weight_ratio <- raceruns %>%
  select(result, weight_ratio) %>%
  group_by(result) %>%
  summarise(Media = mean(weight_ratio),
            Mediana = median(weight_ratio))

#con la media
x11(); ggplot(resultado_vs_weight_ratio, aes(x=result, y=Media)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la media de weight_ratio",
       x='Resultado', y='Media de weight_ratio') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#con la mediana
x11(); ggplot(resultado_vs_weight_ratio, aes(x=result, y=Mediana)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la mediana de weight_ratio",
       x='Resultado', y='Mediana de weight_ratio') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~ con la variable mean_time ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
resultado_vs_mean_time <- raceruns %>%
  select(result, mean_time) %>%
  group_by(result) %>%
  summarise(Media = mean(mean_time),
            Mediana = median(mean_time))

#con la media
x11(); ggplot(resultado_vs_mean_time, aes(x=result, y=Media)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la media de mean_time",
       x='Resultado', y='Media de mean_time') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#con la mediana
x11(); ggplot(resultado_vs_mean_time, aes(x=result, y=Mediana)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la mediana de mean_time",
       x='Resultado', y='Mediana de mean_time') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~ con la variable win_odds ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
resultado_vs_win_odds <- raceruns %>%
  select(result, win_odds) %>%
  group_by(result) %>%
  summarise(Media = mean(win_odds),
            Mediana = median(win_odds),
            Maximo = max(win_odds),
            Min = min(win_odds))

#con la media
x11(); ggplot(resultado_vs_win_odds, aes(x=result, y=Media)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la media de win_odds",
       x='Resultado', y='Media de win_odds') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#con la mediana
x11(); ggplot(resultado_vs_win_odds, aes(x=result, y=Mediana)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la mediana de win_odds",
       x='Resultado', y='Mediana de win_odds') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#con el minimo
x11(); ggplot(resultado_vs_win_odds, aes(x=result, y=Min)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y el mínimo de win_odds",
       x='Resultado', y='Mínimo de win_odds') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~ con la variable horsepower ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
data_horsepower <- raceruns %>%
  select(result, horsepower) %>%
  group_by(result) %>%
  summarise(Media = mean(horsepower),
            Mediana = median(horsepower))

#con la media
x11(); ggplot(data_horsepower, aes(x=result, y=Media)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la media de horsepower",
       x='Resultado', y='Media de horsepower') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

#con la mediana
x11(); ggplot(data_horsepower, aes(x=result, y=Mediana)) +
  geom_point() +
  stat_smooth(color="red", fill="#69b3a2", se=TRUE) +
  labs(title = "Relación entre el resultado en la carrera y la mediana de horsepower",
       x='Resultado', y='Mediana de horsepower') +
  scale_x_continuous(breaks = seq(1, 14)) +
  theme_bw()

# GRÁFICO_13 ####

library(ggridges)
library(ggplot2)
library(viridis)
library(hrbrthemes)
raceruns$draw <- as.factor(raceruns$draw)
x11(); ggplot(raceruns, aes(x = result, y = draw, fill = draw)) +
  geom_density_ridges() +
  theme_ridges() + 
  theme(legend.position = "none")
