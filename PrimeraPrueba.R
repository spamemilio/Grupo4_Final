setwd("~/Diplomatura/Trabajo Final/Grupo4_Final")
library(tidyverse)
options()

prestaciones_previsionales_sipa_por_tipo <- read_csv("https://infra.datos.gob.ar/catalog/sspm/dataset/189/distribution/189.1/download/prestaciones-previsionales-sipa-por-tipo.csv")

prestaciones_tidy <- prestaciones_previsionales_sipa_por_tipo %>% 
  pivot_longer(cols = -ends_with("indice_tiempo"), names_to  = "prestacion", values_to ="cantidad" )

glimpse(prestaciones_tidy)  

prestaciones_tidy %>% 
ggplot(aes(x=indice_tiempo, y = cantidad)) +
  geom_line(aes(color=prestacion))

totales_por_prestacion <- prestaciones_tidy %>% 
  group_by(prestacion) %>% 
  summarise(total = sum(cantidad, na.rm = TRUE))
