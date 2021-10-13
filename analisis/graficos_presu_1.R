# print ("Graficos de AUH")
# los graficos los divido en diferentes scripts para poder ir poniendo comentarios en el Rmarkdown

titulo1 = "Evolución anual de AUH como % del PIB"
Encoding(titulo1)<-"UTF-8"
titulo2 = "Evolución anual de AUH como % del Gasto Total"
Encoding(titulo2)<-"UTF-8"
titulo3 = "Evolución anual de AUH como % del Gasto Social"
Encoding(titulo3)<-"UTF-8"
anio = "Año"
Encoding(anio)<-"UTF-8"





auh1 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_PIB*100))+
  geom_line ()+
  labs(title=titulo1,
       y="% Auh sobre PBI",
       x = anio)+
  theme(axis.text.x = element_text(size = 7), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 8),
        plot.title = element_blank())+
  scale_color_brewer(palette = "Set2")

print (auh1)

auh2 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_gasto_total*100))+
  geom_line ()+
  labs(title=titulo2,
       y="% Auh sobre Gasto",
       x = anio)+
  theme(axis.text.x = element_text(size = 7), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 8),
        plot.title = element_blank())+
  scale_color_brewer(palette = "Set2")

print (auh2) 

auh3 <- gasto_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario,y=auh_sobre_gasto_social*100))+
  geom_line ()+
  labs(title=titulo3,
       y="% Auh sobre Gasto Social",
       x = anio)+
  theme(axis.text.x = element_text(size = 7), axis.title.x = element_blank(),
        axis.text.y = element_text(size = 7), axis.title.y = element_text(size = 8),
        plot.title = element_blank())+
  scale_color_brewer(palette = "Set2")


print (auh3)


