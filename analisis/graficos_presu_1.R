# print ("Graficos de AUH")
# los graficos los divido en diferentes scripts para poder ir poniendo comentarios en el Rmarkdown

auh1 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_PIB))+
  geom_line ()+
  labs(title="Evolucion anual de AUH como % del PIB",
       y="% Auh sobre PBI",
       x = "Anio")

print (auh1)

auh2 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_gasto_total))+
  geom_line (color="Red")+
  labs(title="Evolucion anual de AUH como % del Gasto Total",
       y="% Auh sobre Gasto",
       x = "Anio")+
  theme_classic()

print (auh2) 

auh3 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_gasto_social))+
  geom_line ()+
  labs(title="Evolucion anual de AUH como % del Gasto Social",
       y="% Auh sobre Gasto Social",
       x = "Anio")+
  theme_classic()

print (auh3)

