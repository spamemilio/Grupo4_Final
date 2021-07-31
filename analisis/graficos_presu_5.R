#Ahora dejamos solo la parte de asignaciones familiares en la fin fun 3.3
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

