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


titulo1 = "Evolución anual de Servicios Sociales"
Encoding(titulo1)<-"UTF-8"
anio = "Año"
Encoding(anio)<-"UTF-8"


sinsegu1 <- funcion_unificado_sin_seguridad_con_familiares %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = porc_sobre_gasto_total*100)) +
  geom_line(aes(color=funcion_desc))+
  guides(color=guide_legend(ncol=1))+
  labs(title = paste0(titulo1, "\n (Sin Jubilaciones y Pensiones) \n en % del Gasto Total"), x = anio, y = "% Gasto Total", color = "Funcion")+
  scale_color_brewer(palette = "Set2")+
  guides(color=guide_legend(nrow=4))+
  theme(axis.text.x = element_text(size = 7), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 12),
        legend.position="bottom")

print(sinsegu1)

