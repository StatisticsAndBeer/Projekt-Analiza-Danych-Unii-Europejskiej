###WYKRESY======================================================================
project_path <- "C:/Users/miesz/Desktop/Studia/Projekt-Analiza-Danych-Unii-Europejskiej"

##Estetyka----------------------------------------------------------------------
#Ograniczenie wyświetlania liczb w notacji wykładniczej
options(scipen = 9999)


#Rozrzut punktów (dla czytelności)
jitter <- position_jitter(width = 0.3, height = 0, seed=123)

#Ustalenie wspólnego motywu
main_plot_theme <- theme(legend.position = "none", #brak legendy
                         panel.background = element_rect(fill = "white"), #białe tło
                         panel.border = element_rect(colour = "black", fill = NA), #czrana ramka
                         panel.grid.major.y = element_line(colour = alpha("gray", 0.4)),#szare poziome linie siatki
                         plot.title = element_text(hjust = 0.5)) 

#Ustalenie nazw zmiennych 
vars <- c("DISP_INC_hh", "EXP", "G_CONS", "G_DEBT", "GDP_pc", "HICP", "UNEMP")
var_names <- list(geo = "Kraj",
                  time = "Rok",
                  DISP_INC_hh = "Skorygowany dochód rozporządzalny brutto gospodarstw domowych (moc nabywcza)",
                  EXP = "Eksport towarów i usług [% PKB]",
                  G_CONS = "Rządowe wydatki na konsumpcję [% PKB]",
                  G_DEBT = "Zadłużenie rządowe [%PKB]",
                  GDP_pc = "Przychód krajowy brutto na mieszkańca [EUR]",
                  HICP = "Wskaźniki cen towarów i usług konsumpcyjnych",
                  UNEMP = "Stopa bezrobocia [%]",
                  region = "Strefa")

# ggplot(data, aes(x = time, y = GDP_pc, color = geo)) +
#   geom_point(position = jitter, size=2) + 
#   geom_text_repel(mapping = aes(label=  geo), color = "black",  size = 2, 
#                   max.overlaps = 20, position = jitter, min.segment.length = 0)+
#   geom_vline(xintercept = seq(2019.5, 2022.5, b=1), linetype = "dashed") +
#   scale_y_continuous(n.breaks = 8) + 
#   main_plot_theme


setwd(paste(project_path, "/Wykresy/Boxploty", sep=""))

for(i in vars){
  title <- var_names[[i]]
  
  file_name <- paste(var_names[[i]], ".png", sep="")
  
  p <- ggplot(data, aes(x = factor(time), y = unlist(data[i]))) +
    geom_boxplot(outlier.shape = NA, fill = alpha("blue", 0.1)) + 
    geom_point(position = jitter, size=2, aes(x = factor(time), y = unlist(data[i]), color = geo)) + 
    geom_text_repel(mapping = aes(label=  geo), color = "black",  size = 2, 
                    max.overlaps = 20, position = jitter, min.segment.length = 0) +
    scale_y_continuous(n.breaks = 8) + 
    xlab("Rok") +
    ylab("") + 
    ggtitle(title) +
    main_plot_theme
  
    ggsave(filename = file_name, plot = p, height = 1600, width = 2400, units = "px")
}

