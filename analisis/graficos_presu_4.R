leyenda = "Ahora dejamos solo la parte de asignaciones familiares"
print (leyenda)

asignaciones_familiares_por_anio <- 
  gasto_unif_familiares_por_anio %>% 
  group_by(ejercicio_presupuestario) %>% 
  summarise(porc_sobre_gasto_total = sum(porc_sobre_gasto_total))

#Agrego una columna para tener la descripcion
asignaciones_familiares_por_anio$funcion_desc = "Asignaciones Familiares"


#ahora saco la funcion seguridad social  
funcion_unificado_sin_seguridad <-  
  funcion_unificado_por_anio %>% 
  filter (funcion_desc != "Seguridad Social") %>% 
  select (c(ejercicio_presupuestario, porc_sobre_gasto_total, funcion_desc))

funcion_unificado_sin_seguridad_con_familiares <- rbind(funcion_unificado_sin_seguridad,asignaciones_familiares_por_anio)

sinsegu1 <- funcion_unificado_sin_seguridad_con_familiares %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = porc_sobre_gasto_total)) +
  geom_line(aes(color=funcion_desc))+
  guides(color=guide_legend(ncol=1))+
  theme(legend.position = "bottom")

print(sinsegu1)

