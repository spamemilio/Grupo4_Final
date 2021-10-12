gasto_unificado_porcentajes <- 
  gasto_unificado_por_anio %>% 
  select (c(ejercicio_presupuestario, auh_sobre_PIB, auh_sobre_gasto_total, auh_sobre_gasto_social, gasto_social_sobre_PIB, gasto_social_sobre_gasto_total, gasto_total_sobre_PIB)) 

gasto_unificado_tidy <- gasto_unificado_porcentajes %>% 
  pivot_longer(cols = -ejercicio_presupuestario, names_to = "Tipo", values_to = "Porcentaje")


gasto1 <- gasto_unificado_tidy %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = Porcentaje*100)) +
  geom_line(aes(color=Tipo))+
  guides(color=guide_legend(ncol=1))+
  labs(title="Evolucion de AUH y Gasto Social  \n en % del Gasto Total y PBI",
       y="% Auh sobre Gasto Social",
       x = "Anio")+
  theme(axis.text.x = element_text(size = 8), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 8), axis.title.y = element_text(size = 8),
        plot.title = element_text(size = 12),
        legend.position="left")+
  scale_color_brewer(palette = "Set2")

print (gasto1)