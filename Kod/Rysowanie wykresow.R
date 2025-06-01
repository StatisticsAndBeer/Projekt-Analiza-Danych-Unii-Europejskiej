###WYKRESY======================================================================
p<-c("ggplot2", "tidyr", "reshape2", "dplyr", "stringr", "ineq", "ggrepel")

for (i in p){
  #if(!require(i, character.only = TRUE)){install.packages(i)}
  if(!i %in% installed.packages()){
    install.packages(i)
  }
  library(i, character.only = TRUE)
}
rm(p, i)

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
                  DISP_INC_hh = "Skorygowany dochód brutto gospodarstw domowych do dyspozycji [standard mocy nabywzcej, (PPS 2020)]",
                  EXP = "Eksport towarów i usług [wolumeny łańcuchowo powiązane (2015), milion euro]",
                  G_CONS = "Rządowe wydatki na konsumpcję [milion euro, bieżące ceny]",
                  G_DEBT = "Zadłużenie rządowe [% PKB]",
                  GDP_pc = "Przychód krajowy brutto na mieszkańca [wolumeny łańcuchowo powiązane (2015), euro na mieszkańca]",
                  HICP = "Wskaźniki cen towarów i usług konsumpcyjnych",
                  UNEMP = "Stopa bezrobocia [%]",
                  region = "Strefa")

#Wstępna analiza----------------------------------------------------------------
#Boxploty - wstępna analiza.....................................................
setwd(paste(project_path, "/Wykresy/Boxploty", sep=""))

for(i in vars){
  title <- str_wrap(var_names[[i]], width = 70)
  
  file_name <- paste(i, ".png", sep="")
  
  p <- ggplot(data, aes(x = time, y = unlist(data[i]))) +
    geom_boxplot(outlier.shape = NA, fill = alpha("blue", 0.1)) + 
    geom_point(position = jitter, size=2, 
               aes(x = time, y = unlist(data[i]), color = geo)) + 
    geom_text_repel(mapping = aes(label=  geo), color = "black",  size = 2, 
                    max.overlaps = 20, position = jitter, min.segment.length = 0) +
    scale_y_continuous(n.breaks = 8) + 
    xlab("Rok") +
    ylab("") + 
    ggtitle(title) +
    main_plot_theme
  
    ggsave(filename = file_name, plot = p, height = 1600, width = 2400, units = "px")
}

rm(i, p, file_name, title)

#Gęstość - wstępna analiza......................................................
setwd(paste(project_path, "/Wykresy/Gęstość", sep=""))


for(i in vars){
  title <- str_wrap(var_names[[i]], width = 70)
  
  file_name <- paste(i, " gestosc.png", sep="")
  
  p <- ggplot(data, aes(x = unlist(data[i]))) +
    geom_density(fill = "blue", alpha =0.1) +
    facet_wrap(~time, nrow = 1) +
    scale_y_continuous(n.breaks = 8) + 
    scale_x_continuous(n.breaks = 5) +
    ylab("Częstotliwość") + 
    xlab("Wartość zmiennej") +
    ggtitle(title) +
    main_plot_theme +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave(filename = file_name, plot = p, height = 1600, width = 2400, units = "px")
}

rm(i, p, file_name, title)

setwd(project_path)

#Badanie normalnoći-------------------------------------------------------------
#qq-ploty.......................................................................
library(reshape2)
library(stringr)

setwd(paste(project_path, "/Wykresy/q-q", sep=""))

q_q_data <- melt(data=data, id.vars = c("time", "geo", "region"), 
                 variable.name = "variable", value.name = "value")

file_name <- paste("Wykres q-q.png")
title <- str_wrap("Wykresy kwantyl-kwantyl dla poszczególnych zmiennych na przestrzeni lat",
                  width = 70)
  
p <- ggplot(q_q_data, aes(sample = value)) +
  geom_qq_line() +
  geom_qq()+
  facet_grid(rows = vars(variable), cols = vars(time), scales = "free") +
  scale_y_continuous(n.breaks = 4) +
  ylab("Wartości rzeczywste zmiennych") +
  xlab("Teoretyczne wartości rozkładu normalnego") +
  ggtitle(title) +
  main_plot_theme +
  theme(panel.grid.major.x = element_line(colour = alpha("gray", 0.4)))
  
ggsave(filename = file_name, plot = p, height = 2400, width = 1800, units = "px")


rm(p, file_name, q_q_data)

#Badanie nierówności------------------------------------------------------------
#Krzywa lorenza.................................................................
library(ineq)

setwd(paste(project_path, "/Wykresy/Lorenz", sep=""))

#Zmiana formatu danych dla odpowiedniego rysowania wykresu
#Inflcja pominięta ze wzglęu na ujemne wartości
lorenz_data <- melt(data = data %>% select(-HICP), 
                    id.vars = c("time", "geo", "region"), 
                 variable.name = "variable", value.name = "value")

lorenz_data <- lorenz_data %>%
  group_by(variable, time) %>%
  summarize(lorenz = list(Lc(value)$L),
            p = list(Lc(value)$p),
            .groups = "drop") %>%
  unnest(c(p, lorenz)) %>%
  rename(L = lorenz)

file_name <- paste("Krzywe Lorenza.png")
title <- str_wrap("Krzywa Lorenza obrazująca dystrybucję lub zróżnicowanie 
                  poszczególnych zmiennycw karajch UE w latach 2019-2023",
                  width = 70)

p <- ggplot(lorenz_data, aes(x = p, y = L)) +
  geom_line(color = "blue", size = 0.8) +
  geom_ribbon(aes(ymin = L, ymax = p), fill = "blue", alpha = 0.1)+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  facet_grid(rows = vars(variable), cols = vars(time), scales = "free") +
  scale_y_continuous(n.breaks = 5) +
  scale_x_continuous(n.breaks = 5) +
  xlab("Kwantyl") +
  ylab("Skumulowany odsetek rozpatrywanej zmiennej") +
  ggtitle(title)+
  main_plot_theme +
  theme(panel.grid.major.x = element_line(colour = alpha("gray", 0.4)),
        axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(filename = file_name, plot = p, height = 2400, width = 1800, units = "px")

rm(file_name, title, jitter, lorenz_data, main_plot_theme, p)
