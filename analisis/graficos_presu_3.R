anio = "Año"
Encoding(anio)<-"UTF-8"

unif1 <- funcion_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = porc_sobre_gasto_total*100)) +
  geom_line(aes(color=funcion_desc))+
  labs(title = "Evolucion anual de Servicios Sociales \n en % del Gasto Total", x = anio, y = "% Gasto Total", color = "Funcion")+
  scale_color_brewer(palette = "Set2")+
  guides(color=guide_legend(nrow=4))+
  theme(axis.text.x = element_text(size = 7), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 12),
        legend.position="bottom")

print(unif1)

