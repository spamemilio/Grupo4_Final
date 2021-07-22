library(tidyverse)
library(viridis)
library(packcircles)
library(ggplot2)



#Proceso datasets de ejecucion presupuestaria 

lista_de_ejercicios <- c(2013:2020)
cant_ejercicios = length(lista_de_ejercicios) 
nombre_arch = "credito-historico-anual-"
todos_los_ejercicios <- list()
ej_con_bapin = 0

for (i in (1:cant_ejercicios)) {
  dataset_ejercicio <- paste0("data/credito-historico-anual-",lista_de_ejercicios[i],".csv")
  todos_los_ejercicios [[i]] <- read.csv(dataset_ejercicio)
  if (lista_de_ejercicios[i]>=2020) {
    ej_con_bapin = ej_con_bapin+1
  }
}

#A partir de 2020 los datasets tienen 4 columnas mas (pex+descripcion, bapin+descripcion)
#Las voy a quitar para poder hacer rbind

ultimo_ej = cant_ejercicios + 1
for (j in (1:ej_con_bapin)) {
   dataset_a_corregir <- todos_los_ejercicios[[ultimo_ej-j]]
   dataset_a_corregir$prestamo_externo_id <- NULL
   dataset_a_corregir$prestamo_externo_desc <- NULL
   dataset_a_corregir$codigo_bapin_id <- NULL
   dataset_a_corregir$codigo_bapin_desc <- NULL
   todos_los_ejercicios[[ultimo_ej-j]] <- dataset_a_corregir 
}  

ejercicios <- do.call("rbind",todos_los_ejercicios)


#Tratamiento dataset de gasto total
#el tipo de columnas lo pongho para poder poner los numeros como caracteres y despues tyratar el , que seapar decimales
dataset_gasto_total <- read_csv("data/serie_pib_anual.csv", col_types = "dcccc")

#Tratamiento de dataset de gasto total discriminado por finalidad funcion
dataset_gasto_por_fin_fun <- read_csv("data/serie_finfun_anual.csv", col_types = "dccccccc")


#DATA CLEANING SESSION 

#Limpio y acomodo dataset de ejercicios

#EDA para ver si hay valores raros
summary(ejercicios)

## Los importes vienen como caracteres y con, como separador, los parseo como numero y ademas vienen en millones
es_MX <- locale("es", decimal_mark = ",")

ejercicios <- read_csv("data/Credito_PG19_SPG3_2009_2021.txt") %>% 
  filter(ejercicio_presupuestario %in% c(2013:2020) )

unidad_millon = 1000000

ejercicios <- ejercicios %>% 
  mutate(credito_presupuestado = credito_presupuestado*unidad_millon,
         credito_vigente = credito_vigente*unidad_millon,
         credito_comprometido = credito_comprometido*unidad_millon,
         credito_devengado =  credito_devengado*unidad_millon,
         credito_pagado = credito_pagado*unidad_millon)

ejercicios$credito_presupuestado <- parse_number(ejercicios$credito_presupuestado, locale = es_MX)*unidad_millon
ejercicios$credito_vigente <- parse_number(ejercicios$credito_vigente, locale = es_MX)*unidad_millon
ejercicios$credito_comprometido <- parse_number(ejercicios$credito_comprometido, locale = es_MX)*unidad_millon
ejercicios$credito_devengado <- parse_number(ejercicios$credito_devengado, locale = es_MX)*unidad_millon
ejercicios$credito_pagado <- parse_number(ejercicios$credito_pagado, locale = es_MX)*unidad_millon

#Saco esta columna porque no aporta a este analisis
ejercicios$ultima_actualizacion_fecha <- NULL


#la actividad dentro del programa de auh cambio en 2018 pero no esta incluido en la 
#tabla de mapeos por lo que tuvimos que desehcar su uso y filtrar a mano
#primero unifico su descripcion

ejercicios$actividad_desc = str_replace(ejercicios$actividad_desc, "Asignacion Universal para Proteccion Social[:blank:][:graph:]PPG[:graph:]","AUH")
ejercicios$actividad_desc = str_replace(ejercicios$actividad_desc, "Asignacion Universal para Proteccion Social","AUH")

#DATA CLEANSING DE LOS DATASET TOTALES

#EDA de los dataset de gasto total para ver valores raros
summary(dataset_gasto_total)
summary(dataset_gasto_por_fin_fun)


# paso los importes a number
dataset_gasto_total$ingreso <- parse_number(dataset_gasto_total$ingreso, locale = es_MX)
dataset_gasto_total$gasto <- parse_number(dataset_gasto_total$gasto, locale = es_MX)
dataset_gasto_total$PIB <- parse_number(dataset_gasto_total$PIB, locale = es_MX)
dataset_gasto_total$ultima_actualizacion_fecha <- NULL

# paso los importes a number
dataset_gasto_por_fin_fun$gasto <- parse_number(dataset_gasto_por_fin_fun$gasto, locale = es_MX)
dataset_gasto_por_fin_fun$PIB <- parse_number(dataset_gasto_por_fin_fun$PIB, locale = es_MX)

dataset_gasto_por_fin_fun$ultima_actualizacion_fecha <- NULL


#str_replace_all(ejercicios$actividad_desc, "Asignacion Universal para Proteccion Social (PPG)", "Asignacion Universal para Proteccion Social")

#HECHA LA LIMPIEZA comienza el tratamiento
#Por un lado generamos info especifica de AUH, recordar que cambio de actividad a partir de 2019

auh1 <- ejercicios %>% 
     filter(servicio_id == 850) %>%
     filter(programa_id == 19) %>%
     filter(subprograma_id == 3) %>%
     filter(actividad_id == 1)

auh2 <- ejercicios %>% 
  filter(servicio_id == 850) %>%
  filter(programa_id == 19) %>%
  filter(subprograma_id == 3) %>%
  filter(actividad_id == 41)

auh_unificada <- rbind(auh1, auh2)
summary(auh_unificada)

#agrupo gasto en auh por anio

auh_por_anio <- auh_unificada %>% 
  group_by(ejercicio_presupuestario) %>% summarise(total_auh = sum(credito_devengado))


#Por otro lado armo un dataset con todas las asignaciones familiares incluida la AUH

asign_fliares <- ejercicios %>% 
  filter(servicio_id == 850) %>%
  filter(programa_id == 19) %>%
  filter(subprograma_id %in% c(1,2,3,4))

#agrupo gasto por tipo de asignacion familiar y por año

familiares_por_anio <- asign_fliares %>% 
  group_by (ejercicio_presupuestario, actividad_desc) %>% 
  summarise(total_gasto_familiar = sum(credito_devengado))

#familiares_por_anio$actividad_desc = str_replace(familiares_por_anio$actividad_desc, "Asignacion Universal para Proteccion Social[:blank:][:graph:]PPG[:graph:]","AUH")
#familiares_por_anio$actividad_desc = str_replace(familiares_por_anio$actividad_desc, "Asignacion Universal para Proteccion Social","AUH")

#str_detect(familiares_por_anio$actividad_desc, "Asignacion Universal para Proteccion Social[:blank:][:graph:]PPG[:graph:]")

#Genero analisis de servicios sociales por anio

sociales_por_anio <- dataset_gasto_por_fin_fun %>% 
  filter (finalidad_id == 3) %>% 
  filter (ejercicio_presupuestario %in% c(2013:2020)) %>% 
  group_by(ejercicio_presupuestario) %>% summarise(total_gasto_social = sum(gasto))

#Agrupo con auh y con el gasto total en un dataset de a pasos
sociales_y_auh_por_anio <- left_join(sociales_por_anio, auh_por_anio)
gasto_unificado_por_anio <- left_join(sociales_y_auh_por_anio, dataset_gasto_total)

#Agrego columnas de porcentaje de cada total respecto a otros
gasto_unificado_por_anio$auh_sobre_gasto_social <- gasto_unificado_por_anio$total_auh/gasto_unificado_por_anio$total_gasto_social
gasto_unificado_por_anio$auh_sobre_gasto_total <- gasto_unificado_por_anio$total_auh/gasto_unificado_por_anio$gasto
gasto_unificado_por_anio$auh_sobre_PIB <- gasto_unificado_por_anio$total_auh/gasto_unificado_por_anio$PIB
gasto_unificado_por_anio$gasto_social_sobre_gasto_total <- gasto_unificado_por_anio$total_gasto_social/gasto_unificado_por_anio$gasto
gasto_unificado_por_anio$gasto_social_sobre_PIB <- gasto_unificado_por_anio$total_gasto_social/gasto_unificado_por_anio$PIB
gasto_unificado_por_anio$gasto_total_sobre_PIB <- gasto_unificado_por_anio$gasto/gasto_unificado_por_anio$PIB


#Agrupo con asignaciones en general y luego con el dataset total todo en un dataset de a pasos
#familiares_y_auh_por_anio <- left_join(familiares_por_anio, auh_por_anio)
sociales_y_familiares_por_anio <- left_join(sociales_por_anio, familiares_por_anio)
gasto_unif_familiares_por_anio <- left_join(sociales_y_familiares_por_anio, dataset_gasto_total)

#Agrego columnas de porcentaje de cada total respecto a otros
gasto_unif_familiares_por_anio$porc_sobre_gasto_social <- gasto_unif_familiares_por_anio$total_gasto_familiar/gasto_unif_familiares_por_anio$total_gasto_social
gasto_unif_familiares_por_anio$porc_sobre_gasto_total <- gasto_unif_familiares_por_anio$total_gasto_familiar/gasto_unif_familiares_por_anio$gasto
gasto_unif_familiares_por_anio$porc_sobre_PIB <- gasto_unif_familiares_por_anio$total_gasto_familiar/gasto_unif_familiares_por_anio$PIB

print ("Graficos de AUH")

auh1 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_PIB))+
  geom_line ()+
  labs(title="Evolucion anual de AUH como % del PIB",
       y="% Auh sobre PBI",
       x = "Anio")+
  theme_classic()

print (auh1)

auh2 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_gasto_total))+
  geom_line (color="Red")+
  labs(title="Evolucion anual de AUH como % del Gasto Total",
       y="% Auh sobre Gasto",
       x = "Año")+
  theme_classic()

print (auh2) 

auh3 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_gasto_social))+
  geom_line ()+
  labs(title="Evolucion anual de AUH como % del Gasto Social",
       y="% Auh sobre Gasto Social",
       x = "Año")+
  theme_classic()

print (auh3)

summary (gasto_unificado_por_anio)

gasto_unificado_porcentajes <- 
  gasto_unificado_por_anio %>% 
  select (c(ejercicio_presupuestario, auh_sobre_PIB, auh_sobre_gasto_total, auh_sobre_gasto_social, gasto_social_sobre_PIB, gasto_social_sobre_gasto_total, gasto_total_sobre_PIB)) 

gasto_unificado_tidy <- gasto_unificado_porcentajes %>% 
    pivot_longer(cols = -ejercicio_presupuestario, names_to = "Tipo", values_to = "Porcentaje")

gasto1 <- gasto_unificado_tidy %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = Porcentaje)) +
  geom_line(aes(color=Tipo))+
  guides(color=guide_legend(ncol=1))

print (gasto1)

#Grafico de asignaciones familiares

flia1 <- gasto_unif_familiares_por_anio %>% 
  ggplot(aes(actividad_desc, porc_sobre_gasto_social)) +
  geom_col(aes(color=actividad_desc, fill=actividad_desc), position=position_dodge(width = 10))+
  facet_wrap(~ ejercicio_presupuestario)+
  guides(color=guide_legend(ncol=1))+
  theme(legend.position = "bottom")

print (flia1)

flia2 <- gasto_unif_familiares_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = porc_sobre_gasto_social)) +
  geom_line(aes(color=actividad_desc))+
  guides(color=guide_legend(ncol=1))+
  theme(legend.position = "bottom")

print (flia2)

#Mas analisis, ahora comparando la finalidad funcion Seguridad Social con las otras pertenecientes a Gasto Social
#Genero analisis de servicios sociales por anio

funcion_por_anio <- dataset_gasto_por_fin_fun %>% 
  filter (finalidad_id == 3) %>% 
  filter (ejercicio_presupuestario %in% c(2013:2020)) %>% 
  group_by(ejercicio_presupuestario, funcion_desc) %>% summarise(total_gasto_social = sum(gasto))

funcion_unificado_por_anio <- left_join(funcion_por_anio, dataset_gasto_total)

#Agrego columnas de porcentaje de cada total respecto a otros
funcion_unificado_por_anio$porc_sobre_gasto_total <- funcion_unificado_por_anio$total_gasto_social/funcion_unificado_por_anio$gasto
funcion_unificado_por_anio$porc_sobre_PIB <- funcion_unificado_por_anio$total_gasto_social/funcion_unificado_por_anio$PIB

leyenda = "Mas analisis, ahora comparando la finalidad funcion Seguridad Social con las otras pertenecientes a Gasto Social"
print(leyenda)

leyenda = "Como vemos en este grafico el gasto en seguridad social explica gran parte del gasto social. Sin embargo, la gran mayoria son jubilaciones por lo que vamos a sacar la finalidad seguridad social y dejar solo la parte de asignaciones familiares"
print (leyenda)

unif1 <- funcion_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = porc_sobre_gasto_total)) +
  geom_line(aes(color=funcion_desc))+
  guides(color=guide_legend(ncol=1))+
  theme(legend.position = "bottom")

print(unif1)

#Ahora dejamos solo la parte de asignaciones familiares en la fin fun 3.3
leyenda = "Ahora dejamos solo la parte de asignaciones familiares"
print (leyenda)

  asignaciones_familiares_por_anio <- 
    gasto_unif_familiares_por_anio %>% 
    group_by(ejercicio_presupuestario) %>% 
     summarise(porc_sobre_gasto_total = sum(porc_sobre_gasto_social))

  #Agrego una columna para tener la descripcion
  asignaciones_familiares_por_anio$funcion_desc = "Asignaciones Familiares"

  
  #ahora saco la funcion seguridad social  
funcion_unificado_sin_seguridad <-  
  funcion_unificado_por_anio %>% 
  filter (funcion_desc != "Seguridad Social") %>% 
  select (c(ejercicio_presupuestario, porc_sobre_gasto_total, funcion_desc))

funcion_unificado_sin_seguridad_con_familiares <- rbind(funcion_unificado_sin_seguridad,asignaciones_familiares_por_anio)

sinsegu1 <- funcion_unificado_sin_seguridad_con_familiares %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = porc_sobre_gasto_total)) +
  geom_line(aes(color=funcion_desc))+
  guides(color=guide_legend(ncol=1))+
  theme(legend.position = "bottom")

print(sinsegu1)

#Hacemos el mismo analisis pero solo la AUH en lugar de todas las asignaciones
leyenda = "Ahora dejamos solo AUH en lugar de todas las asignaciones faamiliares"
print (leyenda)



auh_por_anio_sobre_gasto_total <- 
  gasto_unif_familiares_por_anio %>% 
  filter (actividad_desc == "AUH") %>% 
  group_by(ejercicio_presupuestario) %>% 
  summarise(porc_sobre_gasto_total = sum(porc_sobre_gasto_social)) 

#Agrego una columna para tener la descripcion
auh_por_anio_sobre_gasto_total$funcion_desc = "AUH"

funcion_unificado_sin_seguridad_solo_auh <- rbind(funcion_unificado_sin_seguridad,auh_por_anio_sobre_gasto_total)

soloauh <- funcion_unificado_sin_seguridad_solo_auh %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = porc_sobre_gasto_total)) +
  geom_line(aes(color=funcion_desc))+
  guides(color=guide_legend(ncol=1))+
  theme(legend.position = "bottom")

print(soloauh)


#GRAFICO DE BURBUJAS

leyenda = "Grafico de burbujas para graficar mejor la relcion entre asignaciones familiares y otros gastos sociales (sin Seguridad Social)"
print (leyenda)

burbuja_asig_promedio <- 
  funcion_unificado_sin_seguridad_con_familiares %>% 
  group_by (funcion_desc)  %>% 
  summarise (porc_sobre_gasto_total = mean(porc_sobre_gasto_total))

data = burbuja_asig_promedio

packing <- circleProgressiveLayout(data$porc_sobre_gasto_total, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

burflia <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, funcion_desc = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  scale_fill_manual(values = magma(nrow(data))) +
  geom_text(data = data, aes(x, y, size=porc_sobre_gasto_total, label = funcion_desc)) +
  scale_size_continuous(range = c(1,4)) +
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

print(burflia)

leyenda = "Grafico de burbujas para graficar mejor relacion entre AUH y otros gastos sociales (sin Seguridad Social)"
print (leyenda)


burbuja_auh_promedio <- 
  funcion_unificado_sin_seguridad_solo_auh %>% 
  group_by (funcion_desc)  %>% 
  summarise (porc_sobre_gasto_total = mean(porc_sobre_gasto_total))

data = burbuja_auh_promedio

packing <- circleProgressiveLayout(data$porc_sobre_gasto_total, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

bur_auh <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, funcion_desc = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  scale_fill_manual(values = magma(nrow(data))) +
  geom_text(data = data, aes(x, y, size=porc_sobre_gasto_total, label = funcion_desc)) +
  scale_size_continuous(range = c(1,4)) +
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

print(bur_auh)