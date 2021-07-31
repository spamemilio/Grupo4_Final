gasto1 <- gasto_unificado_tidy %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = Porcentaje)) +
  geom_line(aes(color=Tipo))+
  guides(color=guide_legend(ncol=1))

print (gasto1)

flia1 <- gasto_unif_familiares_por_anio %>% 
  ggplot(aes(actividad_desc, porc_sobre_gasto_social)) +
  geom_col(aes(color=actividad_desc, fill=actividad_desc), position=position_dodge(width = 10))+
  facet_wrap(~ ejercicio_presupuestario, nrow=3)+
  guides(color=guide_legend(ncol=1))+
  theme(legend.position = "bottom")

print (flia1)

flia2 <- gasto_unif_familiares_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = porc_sobre_gasto_social)) +
  geom_line(aes(color=actividad_desc))+
  guides(color=guide_legend(ncol=1))+
  theme(legend.position = "bottom")

print (flia2)
