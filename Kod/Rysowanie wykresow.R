###WYKRESY======================================================================
##Estetyka----------------------------------------------------------------------
#Ograniczenie wyświetlania liczb w notacji wykładniczej
options(scipen = 9999)

#Rozzrzut punktów (dla czytelności)
jitter <- position_jitter(width = 0.3, height = 0, seed=123)

#Ustalenie wspólnego motywu
main_plot_theme <- theme(legend.position = "none", #brak legendy
                         panel.background = element_rect(fill = "white"), #białe tło
                         panel.border = element_rect(colour = "black", fill = NA), #czrana ramka
                         panel.grid.major.y = element_line(colour = alpha("gray", 0.4)))#szare poziome linie siatki
                         #panel.grid.major.x = element_line(colour = alpha("black", 0.8), linetype = "dashed")) 

ggplot(data, aes(x = time, y = GDP_pc, color = geo)) +
  geom_point(position = jitter, size=2) + 
  geom_text_repel(mapping = aes(label=  geo), color = "black",  size = 2, 
                  max.overlaps = 20, position = jitter, min.segment.length = 0)+
  geom_vline(xintercept = seq(2019.5, 2022.5, b=1), linetype = "dashed") +
  scale_y_continuous(n.breaks = 8) + 
  main_plot_theme

ggplot(data, aes(x = factor(time), y = GDP_pc)) +
  geom_boxplot(outlier.shape = NA, fill = alpha("blue", 0.1)) + 
  geom_point(position = jitter, size=2, aes(x = factor(time), y = GDP_pc, color = geo)) + 
  geom_text_repel(mapping = aes(label=  geo), color = "black",  size = 2, 
                  max.overlaps = 20, position = jitter, min.segment.length = 0) +
  scale_y_continuous(n.breaks = 8) + 
  main_plot_theme

