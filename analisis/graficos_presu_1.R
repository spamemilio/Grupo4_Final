# print ("Graficos de AUH")
# los graficos los divido en diferentes scripts para poder ir poniendo comentarios en el Rmarkdown

options(Encoding = "UTF-8")

auh1 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_PIB*100))+
  geom_line ()+
  labs(title="Evolución anual de AUH como % del PIB",
       y="% Auh sobre PBI",
       x = "Año")+
  theme(axis.text.x = element_text(size = 7), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 8),
        plot.title = element_blank())+
  scale_color_brewer(palette = "Set2")

print (auh1)

auh2 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_gasto_total*100))+
  geom_line ()+
  labs(title="Evolucion anual de AUH como % del Gasto Total",
       y="% Auh sobre Gasto",
       x = "Anio")+
  theme(axis.text.x = element_text(size = 7), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 8),
        plot.title = element_blank())+
  scale_color_brewer(palette = "Set2")

print (auh2) 

auh3 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_gasto_social*100))+
  geom_line ()+
  labs(title="Evolucion anual de AUH como % del Gasto Social",
       y="% Auh sobre Gasto Social",
       x = "Anio")+
  theme(axis.text.x = element_text(size = 7), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 8),
        plot.title = element_blank())+
  scale_color_brewer(palette = "Set2")


print (auh3)


