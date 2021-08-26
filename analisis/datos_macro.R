library(tidyverse)
library(lubridate)

# Tasa Desempleo

#https://infra.datos.gob.ar/catalog/sspm/dataset/45/distribution/45.2/download/tasa-desempleo-valores-trimestrales.csv

tasa_desempleo_valores_trimestrales <- read_csv("data/tasa-desempleo-valores-trimestrales.csv")


indicador_prueba <- tasa_desempleo_valores_trimestrales[1:5] %>% 
  bind_rows( c( indice_tiempo                              = parse_date("01/02/2003", "%d/%m/%Y") ),
             c( indice_tiempo                              = parse_date("01/03/2003", "%d/%m/%Y") ),
             c( indice_tiempo                              = parse_date("01/05/2003", "%d/%m/%Y") ),
             c( indice_tiempo                              = parse_date("01/06/2003", "%d/%m/%Y") ),
             c( indice_tiempo                              = parse_date("01/08/2003", "%d/%m/%Y") ),
             c( indice_tiempo                              = parse_date("01/09/2003", "%d/%m/%Y") )
             ) %>% 
  arrange(indice_tiempo)


completar_meses_faltantes <- function(lista_meses) {
  
  min_max <-  lista_meses %>% 
    summarise( min = min(pull(lista_meses[1])) ,
               max = max(pull(lista_meses[1]) ))
  
  cantidad_meses <-  interval(min_max$min,  min_max$max) %/% months(1)
  
  Fecha <- seq(from=min_max$min, by="month", length.out=cantidad_meses)
  
  lista_meses_completa <- data.frame(Fecha)
  
  return(lista_meses_completa)
  
   }
  
lista <- completar_meses_faltantes(indicador_prueba[1])  

completar_promedios <- function(indicador){
  
  i = c(2)
  
  indicador_n = indicador 
  
  if(is.na(indicador[1,])) {
    indicador_n[1,] = 0
    }
  
  if(is.na(indicador[unlist(count(indicador)),])) { 
    indicador_n[unlist(count(indicador)),] = 0 
    indicador[unlist(count(indicador)),] = 0 
    }
  
  while(i<=unlist(count(indicador))) {
    
    if(!is.na(indicador[i,])) {  
      
      indicador_n[i,] =  indicador[i,]
      
      i = i+1
      
      }
    
    else {
      
      j = i
      
      while(is.na(indicador[j,])) {
        
        j = j + 1
        
      }
      
      for (k in i:j){
        
        indicador_n[k,] = indicador[i-1,] + (indicador[j,]-indicador[i-1,])/(j-(i-1))*(k-(i-1))
        
      }
      
      i = j
      
    }
    
  }
  
  return(indicador_n)

  }


desempleo_mensual <- tasa_desempleo_valores_trimestrales %>% 
  right_join(lista, by = c("indice_tiempo" = "Fecha") ) %>% 
  select(indice_tiempo,eph_continua_tasa_desempleo_total) %>% 
 arrange(indice_tiempo)

desempleo_mensual$eph_continua_tasa_desempleo_total <- unlist(completar_promedios(desempleo_mensual['eph_continua_tasa_desempleo_total']))

# oponemos auh con tasa de desocupación para buscar correlación

prestaciones_auh_tdes <- prestaciones_tidy %>% 
  filter( prestacion %in% c("total_auh","ninios_adp") ) %>% 
  filter( day(indice_tiempo)==1 ) %>% 
  filter( !is.na(cantidad) ) %>% 
  pivot_wider(names_from = prestacion, values_from = cantidad) %>% 
  mutate( anio = as.factor(year(indice_tiempo) )) %>% 
  mutate( proporcion_auh =  total_auh/ninios_adp) %>% 
  left_join(desempleo_mensual) %>% 
  filter(!indice_tiempo %in% c(parse_date("2020-11-01"), parse_date("2020-12-01"))) %>% 
  mutate( mes = as.factor(month(indice_tiempo)))

grafico_correlacion_auh_tdes <- 
  prestaciones_auh_tdes %>% 
  ggplot(aes(x=proporcion_auh,y=eph_continua_tasa_desempleo_total)) +
  geom_point(aes(color=mes))+
  scale_color_brewer(palette="Dark2")

glimpse(prestaciones_auh_tdes)
