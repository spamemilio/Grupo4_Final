#GRAFICO DE BURBUJAS

#leyenda = "Grafico de burbujas para graficar mejor la relacion entre asignaciones familiares y otros gastos sociales (sin Seguridad Social)"
#print (leyenda)

burbuja_asig_promedio <- 
  funcion_unificado_sin_seguridad_con_familiares %>% 
  group_by (funcion_desc)  %>% 
  summarise (porc_sobre_gasto_total = mean(porc_sobre_gasto_total))

data = burbuja_asig_promedio

packing <- circleProgressiveLayout(data$porc_sobre_gasto_total, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

burflia <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, funcion_desc = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  scale_fill_manual(values = magma(nrow(data))) +
  geom_text(data = data, aes(x, y, size=porc_sobre_gasto_total, label = funcion_desc)) +
  scale_size_continuous(range = c(1,4)) +
  labs(title = "Relacion entre asignaciones familiares \n y otros gastos \n sociales (sin Seg. Social)")+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

print(burflia)

#leyenda = "Grafico de burbujas para graficar mejor relacion entre AUH y otros gastos sociales (sin Seguridad Social)"
#print (leyenda)


burbuja_auh_promedio <- 
  funcion_unificado_sin_seguridad_solo_auh %>% 
  group_by (funcion_desc)  %>% 
  summarise (porc_sobre_gasto_total = mean(porc_sobre_gasto_total))

data = burbuja_auh_promedio

packing <- circleProgressiveLayout(data$porc_sobre_gasto_total, sizetype='area')
data <- cbind(data, packing)
dat.gg <- circleLayoutVertices(packing, npoints=50)

bur_auh <- ggplot() + 
  geom_polygon(data = dat.gg, aes(x, y, funcion_desc = id, fill=as.factor(id)), colour = "black", alpha = 0.6) +
  scale_fill_manual(values = magma(nrow(data))) +
  geom_text(data = data, aes(x, y, size=porc_sobre_gasto_total, label = funcion_desc)) +
  scale_size_continuous(range = c(1,4)) +
  labs(title = "Relacion entre AUH \n y otros gastos sociales \n (sin Seg. Social y otras asignaciones)")+
  scale_fill_brewer(palette = "Set2")+
  scale_color_brewer(palette = "Set2")+
  theme_void() + 
  theme(legend.position="none") +
  coord_equal()

print(bur_auh)