setwd("~/Diplomatura/Trabajo Final/Grupo4_Final")
library(tidyverse)
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
  
