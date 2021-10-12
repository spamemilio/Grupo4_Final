#Graficos de Asignaciones Familiares


flia1 <- gasto_unif_familiares_por_anio %>% 
  ggplot(aes(actividad_desc, porc_sobre_gasto_social*100)) +
  geom_col(aes(color=actividad_desc, fill=actividad_desc), position=position_dodge(width = 10))+
  facet_wrap(~ ejercicio_presupuestario, nrow=2)+
  labs(title = "Asignaciones Familiares y AUH en % \n gasto social por anio", x = "Anio", y = "% Gasto Social")+
  scale_fill_discrete(name="Asig. Fam.",
                      breaks=c("Asignaciones Familiares Activos", 
                               "Asignaciones Familiares Sector Público Nacional", 
                               "Asignaciones Familiares Monotributistas",
                               "Asignaciones Familiares Pasivos",
                               "Asignaciones Familiares Pensión Universal",
                               "AUH"),
                      labels=c("Activos", "Sector Público", "Monotributistas", "Pasivos", "Pension Univ", "AUH"))+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  guides(color=guide_legend(nrow=4))+
  theme(axis.text.x = element_blank(), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 12),
        plot.title = element_text(size = 12, face = "bold", color = "darkgreen"),
        legend.position="bottom")


print (flia1)

flia2 <- gasto_unif_familiares_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = porc_sobre_gasto_social*100)) +
  geom_line(aes(color=actividad_desc))+
  guides(color=guide_legend(ncol=1))+
  scale_color_brewer(palette = "Set2")+
  labs(x = "Anio", y = "% Gasto Social", color = "Asig. Fam.")+
  scale_fill_brewer(palette = "Set2")+
  guides(color=guide_legend(nrow=4))+
  theme(axis.text.x = element_text(size = 7), axis.title.x = element_text(size = 12),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 12),
        legend.position="bottom")


print (flia2)
