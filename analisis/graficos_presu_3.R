leyenda = "Mas analisis, ahora comparando la finalidad funcion Seguridad Social con las otras pertenecientes a Gasto Social"
print(leyenda)

leyenda = "Como vemos en este grafico el gasto en seguridad social explica gran parte del gasto social. Sin embargo, la gran mayoria son jubilaciones por lo que vamos a sacar la finalidad seguridad social y dejar solo la parte de asignaciones familiares"
print (leyenda)

unif1 <- funcion_unificado_por_anio %>% 
  ggplot(aes(x=ejercicio_presupuestario, y = porc_sobre_gasto_total)) +
  geom_line(aes(color=funcion_desc))+
  guides(color=guide_legend(ncol=1))+
  theme(legend.position = "bottom")

print(unif1)

