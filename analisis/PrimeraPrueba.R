setwd("~/Diplomatura/Trabajo Final/Grupo4_Final")
library(tidyverse)
library(lubridate)
options(scipen = 100)

prestaciones_previsionales_sipa_por_tipo <- read_csv("https://infra.datos.gob.ar/catalog/sspm/dataset/189/distribution/189.1/download/prestaciones-previsionales-sipa-por-tipo.csv")

prestaciones_tidy <- prestaciones_previsionales_sipa_por_tipo %>% 
  pivot_longer(cols = -ends_with("indice_tiempo"), names_to  = "prestacion", values_to ="cantidad" )

glimpse(prestaciones_tidy)  

prestaciones_tidy %>% 
ggplot(aes(x=indice_tiempo, y = cantidad)) +
  geom_line(aes(color=prestacion))+
  guides(color=guide_legend(ncol=1))

totales_por_prestacion <- prestaciones_tidy %>% 
  group_by(prestacion) %>% 
  summarise(total = max(cantidad, na.rm = TRUE))

prestaciones_auh <- prestaciones_tidy %>% 
  filter(str_detect(prestacion, "asignacion_universal"))

prestaciones_auh %>% 
  ggplot(aes(x=indice_tiempo, y = cantidad)) +
  geom_line(aes(color=prestacion))+
  guides(color=guide_legend(ncol=1))

totales_por_tipo_auh <- prestaciones_auh %>% 
  group_by(prestacion) %>% 
  summarise(total = max(cantidad, na.rm = TRUE))


Credito_AUH_Interanual <- read_csv("data/Credito_PG19_SPG3_2009_2021.txt")

Credito_2020_por_Ubicacion <- Credito_AUH_Interanual %>% 
  filter(ejercicio_presupuestario == 2020) %>%
  group_by(impacto_presupuestario_mes, ubicacion_geografica_id,ubicacion_geografica_desc ) %>% 
  summarise(vigente = sum(credito_vigente, na.rm = TRUE ),
            pagado = sum(credito_pagado, na.rm = TRUE),
            devengado = sum(credito_devengado, na.rm = TRUE) )

Credito_2020_por_Ubicacion %>% 
  ggplot(aes(x=impacto_presupuestario_mes, y=devengado))+
  geom_line(aes(color=ubicacion_geografica_desc))+
  guides(color=guide_legend(ncol=1))

  theme( plot.title.position = "plot",
         panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80")
         , panel.grid.minor = element_blank(),  
         legend.direction = "vertical", legend.key.size = unit(1,"mm"), legend.key.width = unit(3,"mm"),
         legend.title = element_text(size = 9, face="bold"),
         axis.text.x = element_text(size = 7),
         strip.text.x = element_text(size = 6,face="bold"),
         strip.background = element_rect(color="black", fill="#FFE599", size=0.5, linetype="solid"))

  prestaciones_totales <- prestaciones_tidy %>% 
    filter(str_detect(prestacion, "total"))
  
  prestaciones_totales %>% 
    ggplot(aes(x=indice_tiempo, y = cantidad)) +
    geom_line(aes(color=prestacion))+
    guides(color=guide_legend(ncol=1))
  
  library(readxl)
  
  poblacion_por_sexo_edad <-  read_excel("data/c1_proyecciones_nac_2010_2040.xls", 
                                              sheet = "cuadro 1", skip = 4) %>% 
    rename(anio = ...1) %>% 
    mutate(anio = as.numeric(anio ) ) %>% 
    filter(!is.na(anio)) %>% 
    select(-c(...5,...6,...7))
  
  poblacion_por_edad_rango <- read_excel("data/c2_proyecciones_nac_2010_2040.xls", 
                                              skip = 5) %>% 
    rename(rangos=...1)
  
  poblacion_por_edad_rango_unisex <- poblacion_por_edad_rango %>% 
    slice(c(5:25)) %>% 
    mutate(`2010`= as.numeric(`2010`))

  poblacion_por_edad_rango_varones <- poblacion_por_edad_rango %>% 
    slice(c(35:65))
  
  poblacion_por_edad_rango_mujeres <- poblacion_por_edad_rango %>% 
    slice(c(65:85))
  
  poblacion_por_edad_rango_unisex_tidy <- poblacion_por_edad_rango_unisex %>% 
    pivot_longer(cols = is_double,names_to  = "anio", values_to = "personas" ) %>% 
    mutate()
  
  poblacion_por_edad_0_19 <- poblacion_por_edad_rango_unisex %>% 
    filter(rangos %in% c("0- 4","5-9", "10-14","15-19")) %>% 
    pivot_longer(cols = is_double,names_to  = "anio", values_to = "personas" ) %>% 
    mutate(anio = as.numeric(anio)) %>% 
    group_by(anio) %>% 
    summarise(total_menores = sum(personas)) 
  
  prestaciones_auh_julio <- prestaciones_auh %>% 
    filter(month(indice_tiempo)==7 ) %>% 
    filter(day(indice_tiempo)==1 ) %>% 
    mutate(anio=year(indice_tiempo)) %>% 
    select(-indice_tiempo)
  
  prestaciones_auh_total_menores <-  prestaciones_auh_julio %>% 
    left_join(poblacion_por_edad_0_19) %>% 
    mutate(proporcion = cantidad/total_menores)
  
  prestaciones_auh_total_menores %>% 
    ggplot(aes(x=anio, y = proporcion)) +
    geom_line(aes(color=prestacion),size=2)+
    guides(color=guide_legend(ncol=1))+
    scale_color_brewer(palette="Dark2")
  
  poblacion_por_edad_0_19 %>% 
    ggplot(aes(x=anio, y=total_menores))+
    geom_line( color = "red")
  
  glimpse(poblacion_por_edad_0_19)
  