library(tidyverse)

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


completar_promedios_for <- function (indicador) {
  
  for(i in 1:unlist(count(indicador))) {
    
    j = i
    
    while(is.na(indicador[j,])) {
      
      j = j + 1
     
    }
    
   # if (j == i ) {
    #  indicador[i,] = indicador[j,]
    # }
    
    if (i==1) {
      if(is.na(indicador[i,])) {indicador[i,] = 0 }
    } 
    
    else {
     # print((unlist(indicador[i-1,]) + unlist(indicador[j,]  ) /j-i) )
     #  print(paste("unlist(indicador[i-1,] ", unlist(indicador[i-1,] )))
     #  print(paste("unlist(indicador[j,] ", unlist(indicador[j,] )))
     #  print (paste("j: ", j, " i ", i))
      # print(as.data.frame(unlist(indicador[i,1])))
      indicador[i,] = as.data.frame((unlist(indicador[i-1,]) + unlist(indicador[j,] )) / (j-i))
      # return(as.data.frame((unlist(indicador[i-1,]) + unlist(indicador[j,] )) / (j-i)))
      
    }
    
            
    }
}

completar_promedios <- function (indicador) {
    
  glimpse(indicador)
  
    j = i
    
  #   while(is.na(indicador[j,])) {
  #     
  #     j = j + 1
  #     
  #   }
  #   
  #   if (i==1) {
  #     if(is.na(indicador[i,])) {indicador[i,] = 0 }
  #   } 
  #   
  #   else {
  #     print((unlist(indicador[i-1,]) + unlist(indicador[j,]  ) /j-i) )
  #     print(paste("unlist(indicador[i-1,] ", unlist(indicador[i-1,] )))
  #     print(paste("unlist(indicador[j,] ", unlist(indicador[j,] )))
  #     print (paste("j: ", j, " i ", i))
  #     print(as.data.frame(unlist(indicador[i,1])))
  #     indicador[i,] = as.data.frame((unlist(indicador[i-1,]) + unlist(indicador[j,] )) / (j-i))
  #     
  #   }
  #   
  #   
  # }
}

# indicador_prueba_completado <- completar_promedios(indicador_prueba[2])


# indicador_prueba %>%  mutate(eph_continua_tasa_desempleo_total=unlist(map(eph_continua_tasa_desempleo_total, completar_promedios_for) ))
# 
# glimpse(indicador_prueba)

 

completar_promedios_for(indicador_prueba[2])
# 
# indicador_prueba[2][2,] = (unlist(indicador_prueba[2][2-1,]) + unlist(indicador_prueba[2][3,] )) / (3-1)

variable <- 3

cambio_prueba(variable)

cambio_prueba <- function(var){
  var= 5
  
}
