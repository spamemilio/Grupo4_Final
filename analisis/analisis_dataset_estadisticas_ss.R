library(tidyverse)
library(lubridate)
library(readxl)
beneficiarios_auh_2013_2019 <-  read_excel("data/Estadistica SS junio 2021_0.xlsx", 
                     sheet = "H.1.1.", skip = 4, n_max=7, col_names = c("Periodo", "Hijo", "Hijo con discapacidad", "Total_AUH"), col_types = c("guess", "numeric", "numeric", "numeric"))
beneficiarios_auh_2020_mensuales <-  read_excel("data/EstadisticaSSjunio2021.xlsx", 
                                sheet = "H.1.1.", skip = 11, n_max=12, col_names = c("Periodo", "Hijo", "Hijo con discapacidad", "Total_AUH"), col_types = c("date", "numeric", "numeric", "numeric"))

beneficiarios_auh_2020 <- tibble (Periodo = "2020",
                                 Hijo = round(mean(beneficiarios_auh_2020_mensuales$Hijo),0),
                                 `Hijo con discapacidad` = round(mean(beneficiarios_auh_2020_mensuales$`Hijo con discapacidad`),0),
                                 Total_AUH = round(mean(beneficiarios_auh_2020_mensuales$Total_AUH),0))

beneficiarios_auh <- rbind(beneficiarios_auh_2013_2019, beneficiarios_auh_2020)


titulares_auh_2013_2019 <-  read_excel("data/Estadistica SS junio 2021_0.xlsx", 
                                          sheet = "H.2.1.", skip = 4, n_max=7, col_names = c("Periodo", "Hijo", "Hijo con discapacidad", "Total_AUH"), col_types = c("guess", "numeric", "numeric", "numeric"))
titulares_auh_2020_mensuales <-  read_excel("data/EstadisticaSSjunio2021.xlsx", 
                                               sheet = "H.2.1.", skip = 11, n_max=12, col_names = c("Periodo", "Hijo", "Hijo con discapacidad", "Total_AUH"), col_types = c("date", "numeric", "numeric", "numeric"))

titulares_auh_2020 <- tibble (Periodo = "2020",
                                 Hijo = round(mean(titulares_auh_2020_mensuales$Hijo),0),
                                 `Hijo con discapacidad` = round(mean(titulares_auh_2020_mensuales$`Hijo con discapacidad`),0),
                                 Total_AUH = round(mean(titulares_auh_2020_mensuales$Total_AUH),0))

titulares_auh <- rbind(titulares_auh_2013_2019, titulares_auh_2020)


hijos_a_cargo_auh_2013_2019 <-  read_excel("data/Estadistica SS junio 2021_0.xlsx", 
                                          sheet = "H.2.3.", skip = 4, n_max=7, col_names = c("Periodo", "1", "2","3","4","5",">5","Total_a_Cargo"), col_types = c("guess", "numeric", "numeric", "numeric","numeric", "numeric", "numeric","numeric"))
hijos_a_cargo_auh_2020_mensuales <-  read_excel("data/EstadisticaSSjunio2021.xlsx", 
                                   sheet = "H.2.3.", skip = 11, n_max=12, col_names = c("Periodo", "1", "2","3","4","5",">5","Total_a_Cargo"), col_types = c("date", "numeric", "numeric", "numeric","numeric", "numeric", "numeric","numeric"))

hijos_a_cargo_auh_2020 <- tibble (Periodo = "2020",
                                 "1" = round(mean(hijos_a_cargo_auh_2020_mensuales$`1`),0),
                                 "2" = round(mean(hijos_a_cargo_auh_2020_mensuales$`2`),0),
                                 "3" = round(mean(hijos_a_cargo_auh_2020_mensuales$`3`),0),
                                 "4" = round(mean(hijos_a_cargo_auh_2020_mensuales$`4`),0),
                                 "5" = round(mean(hijos_a_cargo_auh_2020_mensuales$`5`),0),
                                 ">5" = round(mean(hijos_a_cargo_auh_2020_mensuales$`>5`),0),
                                 Total_a_Cargo = round(mean(hijos_a_cargo_auh_2020_mensuales$Total_a_Cargo),0))

hijos_a_cargo_auh <- rbind(hijos_a_cargo_auh_2013_2019, hijos_a_cargo_auh_2020)


#Asignacion por embarazo

beneficiarios_aue_2013_2019 <-  read_excel("data/Estadistica SS junio 2021_0.xlsx", 
                                          sheet = "E.1.1.", skip = 4, n_max=7, col_names = c("Periodo", "Total_AUE"), col_types = c("guess", "numeric"))
beneficiarios_aue_2020_mensuales <-  read_excel("data/EstadisticaSSjunio2021.xlsx", 
                                               sheet = "E.1.1.", skip = 11, n_max=12, col_names = c("Periodo", "Total_AUE"), col_types = c("date", "numeric"))

beneficiarios_aue_2020 <- tibble (Periodo = "2020",
                                 Total_AUE = round(mean(beneficiarios_aue_2020_mensuales$Total_AUE),0))

beneficiarios_aue <- rbind(beneficiarios_aue_2013_2019, beneficiarios_aue_2020)

#Edad de beneficiarios AUH a marzo 2021

edad_benef_auh <-  read_excel("data/Estadistica SS junio 2021_0.xlsx", 
                                           sheet = "H.1.3.", skip = 4, n_max=7, col_names = c("Rango Edad", "Femenino", "Masculino", "Total"), col_types = c("guess", "numeric", "numeric", "numeric"))


#Edad titulares AUH a marzo 2021

edad_tit_auh <-  read_excel("data/Estadistica SS junio 2021_0.xlsx", 
                        sheet = "H.2.2.", skip = 4, n_max=12, col_names = c("Rango Edad", "Femenino", "Masculino", "Total"), col_types = c("guess", "numeric", "numeric", "numeric"))


#Edad titulares AUE a marzo 2021

edad_tit_aue <-  read_excel("data/Estadistica SS junio 2021_0.xlsx", 
                 sheet = "E.1.3.", skip = 4, n_max=1, col_names = c("Periodo", "15-19", "20-24","25-29","30-34","35-40",">40"), col_types = c("guess", "numeric", "numeric", "numeric","numeric", "numeric", "numeric"))

                                          