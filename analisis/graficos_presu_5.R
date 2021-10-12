#Ahora dejamos solo la parte de asignaciones familiares en la fin fun 3.3
#Hacemos el mismo analisis pero solo la AUH en lugar de todas las asignaciones

auh_por_anio_sobre_gasto_total <- 
  gasto_unif_familiares_por_anio %>% 
  filter (actividad_desc == "AUH") %>% 
  group_by(ejercicio_presupuestario) %>% 
  summarise(porc_sobre_gasto_total = sum(porc_sobre_gasto_total)) 

#Agrego una columna para tener la descripcion
auh_por_anio_sobre_gasto_total$funcion_desc = "AUH"

funcion_unificado_sin_seguridad_solo_auh <- rbind(funcion_unificado_sin_seguridad,auh_por_anio_sobre_gasto_total)

soloauh <- funcion_unificado_sin_seguridad_solo_auh %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = porc_sobre_gasto_total*100)) +
  geom_line(aes(color=funcion_desc))+
  labs(title = "Evolucion anual de AUH y Otras Funciones Sociales \n (Sin Jubilaciones y Pensiones ni Asignaciones Fliares) \n en % del Gasto Total", x = "Anio", y = "% Gasto Total", color = "Funcion")+
  scale_color_brewer(palette = "Set2")+
  guides(color=guide_legend(nrow=4))+
  theme(axis.text.x = element_text(size = 7), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 12),
        legend.position="bottom")


print(soloauh)

