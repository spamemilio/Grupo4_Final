setwd("~/Diplomatura/Trabajo Final/Grupo4_Final")
library(tidyverse)
library(lubridate)
library(readxl)
library(zoo)
options(scipen = 100, Encoding("WINDOWS-1252") )

#prestaciones_previsionales_sipa_por_tipo <- read_csv("https://infra.datos.gob.ar/catalog/sspm/dataset/189/distribution/189.1/download/prestaciones-previsionales-sipa-por-tipo.csv")

prestaciones_previsionales_sipa_por_tipo <- read_csv("data/prestaciones-previsionales-sipa-por-tipo.csv") %>% 
  filter(year(indice_tiempo) %in% c(2013:2020))


prestaciones_tidy <- prestaciones_previsionales_sipa_por_tipo %>% 
  pivot_longer(cols = -ends_with("indice_tiempo"), names_to  = "prestacion", values_to ="cantidad" ) 
  
grafico_prestaciones_desagredadas <- 
prestaciones_tidy %>% 
  filter(prestacion %in% c("jubilaciones_sipa_transparencia",
                           "pensiones_sipa_transparencia ",
                           "asignacion_familiar_hijo_hijo_discapacitado",
                           "asignacion_familiar_prenatal",
                           "asignacion_universal_por_embarazo",
                           "titulares_progresar",
                           "jubilaciones_sipa_datos_abiertos",
                           "pensiones_sipa_datos_abiertos",
                           "pension_universal_adulto_mayor",
                           "asignacion_familiar_hijo_discapacitado",
                           "asignacion_familiar_prenatal_datos_abiertos",
                           "asignacion_universal_embarazo",
                           "prestaciones_seguro_desempleo",
                           "asignacion_universal_hijo_hijo_discapacitado_trasparencia",
                           "asignacion_universal_hijo_hijo_discapacitado_datos_abiertos"
                           # "asignacion_universal_hijo_monotributo_social",
                           # "asignacion_universal_hijo_servicio_domestico",
                           # "asignacion_universal_hijo_otra_condicion",
                           # "asignaciones_familiares_suaf_relacion_dependencia",
                           # "asignaciones_familiares_suaf_monotributo",
                           # "asignaciones_familiares_desempleo",
                           # "asignaciones_familiares_pasivos_sipa"
  )) %>% 
ggplot(aes(x=indice_tiempo, y = cantidad)) +
  geom_line(aes( color=prestacion),size=1)+
  guides(color=guide_legend(ncol=2))+
  theme(legend.position="bottom")+
  labs(title = "Prestaciones ANSES",
       subtitle = paste("Evolucion ", year(min(prestaciones_tidy$indice_tiempo)),
                        " a ", year(max(prestaciones_tidy$indice_tiempo))),
       x = "Anio", y = "Cantidad de beneficiarios", color = "Prestacion")


totales_por_prestacion <- prestaciones_tidy %>% 
  group_by(prestacion) %>% 
  summarise(total = max(cantidad, na.rm = TRUE))

prestaciones_auh <- prestaciones_tidy %>% 
  filter(str_detect(prestacion, "asignacion_universal")) %>% 
  filter(!prestacion %in% c("asignacion_universal_hijo_hijo_discapacitado_datos_abiertos",
                           "asignacion_universal_hijo_hijo_discapacitado_trasparencia"))

grafico_prestaciones_auh <- 
prestaciones_auh %>% 
  ggplot(aes(x=indice_tiempo, y = cantidad)) +
  geom_line(aes(color=prestacion),size=1)+
  guides(color=guide_legend(ncol=2))+
  theme(legend.position="bottom")+
  labs(title = "Asignacion Universal por Hijo/a",
       subtitle = paste("Por categorias Evolucion ", year(min(prestaciones_auh$indice_tiempo)),
                        " a ", year(max(prestaciones_auh$indice_tiempo))),
       x = "Anio", y = "Cantidad de beneficiarios", color = "Categoria")

totales_por_tipo_auh <- prestaciones_auh %>% 
  group_by(prestacion) %>% 
  summarise(total = max(cantidad, na.rm = TRUE))

# Presupuesto AUH

Credito_AUH_Interanual <- read_csv("data/Credito_PG19_SPG3_2009_2021.txt")

poblacion_provincia <- read_excel("data/poblacion_provincia.xls", 
                                  col_types = c("text", "skip", "numeric", 
                                                "skip", "skip"),
                                  skip = 3) %>% 
  rename(cantidad=...2) %>% 
  filter(!is.na(cantidad)) %>% 
  filter(!is.na(Provincia)) %>% 
  filter(!Provincia %in% c("Total del país","24 partidos del Gran Buenos Aires",
                          "Interior de la provincia de Buenos Aires")
         ) 
  # %>% 
  # mutate(ponderacion = cantidad/sum(cantidad) )

Credito_2020_por_Ubicacion <- Credito_AUH_Interanual %>% 
  filter(ejercicio_presupuestario == 2020) %>%
  group_by(impacto_presupuestario_mes, ubicacion_geografica_id,ubicacion_geografica_desc ) %>% 
  summarise(vigente = sum(credito_vigente, na.rm = TRUE ),
            pagado = sum(credito_pagado, na.rm = TRUE),
            devengado = sum(credito_devengado, na.rm = TRUE) ) %>% 
  filter(!ubicacion_geografica_desc %in% c("Interprovincial")) %>% 
  mutate(ubicacion_geografica_desc = str_replace(ubicacion_geografica_desc,"Provincia de ", "")) %>% 
  mutate(ubicacion_geografica_desc = str_replace(ubicacion_geografica_desc,"Provincia del ", "")) %>% 
  rename(Provincia=ubicacion_geografica_desc) %>% 
  left_join(poblacion_provincia) %>% 
  mutate(devengado_proporcional = devengado / cantidad )

grafico_evolucion_presupuesto_AUH <- 
Credito_2020_por_Ubicacion %>% 
  ggplot(aes(x=impacto_presupuestario_mes, y=devengado_proporcional))+
  geom_line(aes(color=Provincia))+
  guides(color=guide_legend(ncol=3))+
  theme( plot.title.position = "plot",
         panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80")
         , panel.grid.minor = element_blank(),  
         legend.direction = "vertical", legend.key.size = unit(1,"mm"), legend.key.width = unit(3,"mm"),
         legend.title = element_text(size = 9, face="bold"),
         legend.position = "bottom",
         axis.text.x = element_text(size = 7),
         strip.text.x = element_text(size = 6,face="bold"),
         strip.background = element_rect(color="black", fill="#FFE599", size=0.5, linetype="solid"))

  prestaciones_totales <- prestaciones_tidy %>% 
    filter(str_detect(prestacion, "total"))
  
  prestaciones_totales %>% 
    ggplot(aes(x=indice_tiempo, y = cantidad)) +
    geom_line(aes(color=prestacion))+
    guides(color=guide_legend(ncol=1))
  
  # incorporamos las proyecciones de población de INDEC
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
  
  grafico_prestaciones_proporcion <- 
    prestaciones_auh_total_menores %>% 
    ggplot(aes(x=anio, y = proporcion)) +
    geom_line(aes(color=prestacion),size=1)+
    guides(color=guide_legend(ncol=2))+
    scale_color_brewer(palette="Dark2")+
    theme(legend.position="bottom")+
    labs(title = "Evolucion categorias AUH",
         subtitle = "Normalizado por poblacion total de 0 a 18",
         x = "Anio", y = "Proporcion de Beneficiarios/as", color = "Categoria")
  
  poblacion_por_edad_0_19 %>% 
    ggplot(aes(x=anio, y=total_menores))+
    geom_line( color = "red")
  
  # Oponemos asignaciones familiares con AUH para determinar proporción
  
  prestaciones_auh_aaff <- prestaciones_tidy %>% 
    filter( prestacion %in% c("total_aaff", "total_auh","ninios_adp") ) %>% 
    filter( day(indice_tiempo)==1 ) %>% 
    filter( !is.na(cantidad) ) %>% 
    pivot_wider(names_from = prestacion, values_from = cantidad) %>% 
    mutate( anio = as.factor(year(indice_tiempo) )) %>% 
    mutate( proporcion_aaff = total_aaff/ninios_adp,
            proporcion_auh =  total_auh/ninios_adp)
    
  grafico_correlacion_auh_aaff <- 
  prestaciones_auh_aaff %>% 
    ggplot(aes(x=total_aaff,y=total_auh)) +
    geom_point(aes(color=anio))+
    scale_color_brewer(palette="Dark2")
  
  grafico_correlacion_auh_aaff_proporcion <- 
    prestaciones_auh_aaff %>% 
    ggplot(aes(x=proporcion_aaff,y=proporcion_auh)) +
    geom_point(aes(color=anio))+
    scale_color_brewer(palette="Dark2")
  
  
  # Prestaciones por titular 
  prestaciones_por_familia <- read_excel("data/H.2.3.Total Pais. Titulares de la AUH. Hijo e Hijo Discapacitado, segun hijos a cargo.xlsx", 
                                         col_types = c("date", "numeric", "numeric", 
                                                       "numeric", "numeric", "numeric", 
                                                       "numeric"), skip = 3) %>% 
    rename( Fecha = '...1',
            Total = '...7') %>% 
    mutate( `1` = `1` / Total,
            `2` = `2` / Total,
            `3` = `3` / Total,
            `4` = `4` / Total,
            `5` = `5` / Total)
  
  glimpse(prestaciones_por_familia)
  
  prestaciones_por_familia_tidy <- prestaciones_por_familia %>% 
    pivot_longer(cols=-c("Fecha"), names_to = "cantidad_hijes", values_to = "cantidad") %>% 
    filter(cantidad_hijes != "Total")
  
  grafico_prestaciones_por_familia <- 
    prestaciones_por_familia_tidy %>% 
    ggplot(aes(x=Fecha, y=cantidad))+
    geom_area(aes(fill=cantidad_hijes))+
    labs(title = "Evolucion proporcion cantidad de hijos/as por titular",
         x = "Anio", y = "Proporcion", color = "Cantidad de hijo/as")
  
  prestaciones_por_familia_tidy_min <- 
    prestaciones_por_familia_tidy %>% 
    filter(Fecha== min(Fecha, na.rm = TRUE ) ) %>% 
    select(cantidad_hijes,cantidad)%>% 
    rename(cantidad_min = cantidad)
  
  prestaciones_por_familia_tidy_dif <- 
    prestaciones_por_familia_tidy %>% 
    filter(Fecha== max(Fecha, na.rm = TRUE ) ) %>% 
    select(cantidad_hijes,cantidad) %>% 
    rename(cantidad_max = cantidad) %>% 
    left_join(prestaciones_por_familia_tidy_min) %>% 
    mutate(cantidad_dif = cantidad_max - cantidad_min)
  
# Prestaciones por rango de edad
  prestaciones_edad <- 
  read_excel("data/H.2.2.Total Pais. Titulares de la AUH. Hijo e Hijo Discapacitado por sexo y grupo de edad.xlsx", 
             skip = 3) %>% 
    rename(Fecha = `...1`) %>%
    filter(!is.na(as.numeric(Fecha))) %>%
    mutate(Fecha=as.double(Fecha)-(70*365)-19) %>%
    mutate(Fecha=as_date(Fecha,origin = lubridate::origin)) %>%
    mutate(`15 - 19...2` = as.double(`15 - 19...2`))
  
  prestaciones_edad_tidy <- 
  prestaciones_edad %>% 
    select(c(1,14:25)) %>% 
    pivot_longer(cols = c(2:13), names_to = "rango_edad", values_to = "cantidad" ) %>% 
    mutate(genero = 'F') %>% 
    mutate(rango_edad =  str_replace_all(rango_edad,"\\.+\\d+", ""))
  
  prestaciones_edad_tidy_m <- 
    prestaciones_edad %>% 
    select(c(1,26:37)) %>% 
    pivot_longer(cols = c(2:13), names_to = "rango_edad", values_to = "cantidad" ) %>% 
    mutate(genero = 'M') %>% 
    mutate(rango_edad =  str_replace_all(rango_edad,"\\.+\\d+", ""))
  
  prestaciones_edad_tidy <- prestaciones_edad_tidy %>% 
    rbind(prestaciones_edad_tidy_m)
  
  grafico_prestaciones_edad <- 
  prestaciones_edad_tidy %>% 
  ggplot(aes(x = rango_edad, y = cantidad))+
    geom_col(aes(fill=genero))+
    facet_wrap(vars(year(Fecha)))+
    labs(title = "Cantidad de prestaciones por edad",
         subtitle = "De 0 a 18 anios dividio en rangos de 5",
         x = "Rando de Edad", y = "Cantidad", fill = "Genero")
  
  #Titulares de derecho de la AUH Hijo e Hijo Discapacitado, por sexo y grupo de edad
  
  prestaciones_sexo_edad_ninies <-
  read_excel("data/H.1.3.Total Pais. Titulares de derecho de la AUH Hijo e Hijo Discapacitado, por sexo y grupo de edad.xlsx", 
             col_types = c("date", "text", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                           "numeric", "numeric", "numeric", 
                         "numeric"), skip = 3) %>% 
    rename(Fecha = `...1`) %>%
    filter(!is.na(Fecha)) %>% 
    mutate(Fecha=as.Date(Fecha))

  prestaciones_sexo_edad_ninies_tidy <-   prestaciones_sexo_edad_ninies %>% 
    select(c(1,9:15)) %>% 
    pivot_longer(cols = c(2:8), names_to = "rango_edad", values_to = "cantidad" ) %>% 
    mutate(genero = 'F') %>% 
    mutate(rango_edad =  str_replace_all(rango_edad,"\\.+\\d+", "")) %>% 
    mutate(rango_edad = factor(rango_edad,levels=c("0 - 2", "3 - 5","6 - 8",
                                                   "9 - 11", "12 - 14","15 - 17"
                                                   , "18 y más") ))
  
  prestaciones_sexo_edad_ninies_tidy_m <-   prestaciones_sexo_edad_ninies %>% 
    select(c(1,16:22)) %>% 
    pivot_longer(cols = c(2:8), names_to = "rango_edad", values_to = "cantidad" ) %>% 
    mutate(genero = 'M') %>% 
    mutate(rango_edad =  str_replace_all(rango_edad,"\\.+\\d+", "")) %>% 
    mutate(rango_edad = factor(rango_edad,levels=c("0 - 2", "3 - 5","6 - 8",
                                                      "9 - 11", "12 - 14","15 - 17"
                                                      , "18 y más") ))
    
  prestaciones_sexo_edad_ninies_tidy <- prestaciones_sexo_edad_ninies_tidy %>% 
    rbind(prestaciones_sexo_edad_ninies_tidy_m)
  
  grafico_prestaciones_sexo_edad_ninies <- 
  prestaciones_sexo_edad_ninies_tidy %>% 
    ggplot(aes(x = rango_edad, y = cantidad))+
    geom_col(aes(fill=genero))+
    facet_wrap(vars(year(Fecha)))
  
# Monto Liquidado por Prestación
  
  montos_liquidados <- read_excel("data/H.1.2.Total Pais. Montos liquidados de la AUH. Hijo e Hijo Discapacitado.xlsx", 
                       col_types = c("date", "numeric", "numeric", 
                                     "numeric"), skip = 3) %>% 
    rename(Fecha = `...1`) %>% 
    filter(!is.na(Fecha)) %>% 
    mutate( ejercicio_presupuestario = year(Fecha),
      impacto_presupuestario_mes = month(Fecha))
  
    credito_mensual <- read_csv("data/credito_mensual_auh_interanual.csv.csv") %>% 
    filter(ejercicio_presupuestario %in% c(2013:2019) ) %>%
    group_by(ejercicio_presupuestario, impacto_presupuestario_mes) %>% 
    summarise(vigente = sum(credito_vigente, na.rm = TRUE ),
              pagado = sum(credito_pagado, na.rm = TRUE),
              devengado = sum(credito_devengado, na.rm = TRUE) )
  
  montos_liquidados_presupuesto <- montos_liquidados %>% 
    left_join(credito_mensual) %>% 
    mutate(Total=Total/1000) %>% 
    mutate(proporcion= Total/devengado) %>% 
    mutate(proporcion = replace(proporcion, is.infinite(proporcion),0)) %>% 
    mutate(media_movil = rollmean(proporcion, k = 5 , fill = NA, align = "right"))
  
  proporcion_montos_presupusto <- montos_liquidados_presupuesto %>% 
    ggplot(aes(x=Fecha,))+
    geom_line( aes(y=proporcion, color = "Diferencia" ), color = "green")+
    geom_line(  aes(y=media_movil, color="Media Móvil"), color = "blue", size = 2)
  
  
  
    
  