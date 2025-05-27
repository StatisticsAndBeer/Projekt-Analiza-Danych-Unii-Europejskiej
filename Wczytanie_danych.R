p<-c("eurostat", "ggplot2", "tidyr", "knitr", "tmap",
     "sf", "dplyr", "xlsx", "stringr", "purrr", "ggrepel")
#  "skimr", "ggfortify", "lmtest", "kableExtra", 
for (i in p){
  #if(!require(i, character.only = TRUE)){install.packages(i)}
  if(!i %in% installed.packages()){
    install.packages(i)
  }
  library(i, character.only = TRUE)
}
rm(p, i)



#Wczytanie kodów państw UE
CNTR_eu<-unlist(eu_countries %>%
  mutate(geo=code) %>%
  select(geo))
YEARS=2019:2023

#setwd("")

#Usunięcie z mapy regionów poza kontynentem europejskim
CNTR <- st_read(dsn = "./Country Maps") %>%
  filter(LEVL_CODE == 2) %>%
  filter(CNTR_CODE %in% CNTR_eu) %>%
  filter(!str_detect(NUTS_ID, "FRY[0-9]+")) %>%
  filter(!str_detect(NUTS_ID, "ES7[0-9]+")) %>%
  filter(!str_detect(NUTS_ID, "PT2[0-9]+")) %>%
  filter(!str_detect(NUTS_ID, "PT3[0-9]+")) %>%
  filter(!NUTS_ID == "NO0B") %>%
  select(CNTR_CODE, geometry, NUTS_ID)

#Przygotowanie mapy dla krajów:
CNTR_data <- CNTR %>%
  group_by(CNTR_CODE) %>%
  summarise(n = n()) %>%
  st_cast("MULTIPOLYGON") %>%
  select(CNTR_CODE, geometry)

#Sprawdzenie
tm_shape(CNTR_data) +
  tm_polygons()
rm(CNTR)

# WCZYTANIE DANYCH Z EUROSTATU ####
# Real GDP per capita ####
#ewentualnie "sdg_08_10" bo jako base year używa 2020, a nie 2015
GDP_pc<- get_eurostat(id = "tipsna40", time_format = "num",
                         filters = list(time = YEARS, freq = "A", geo=CNTR_eu)) %>%
  select(geo, time, values) %>%
  rename(GDP_pc = values)


# Uneployment Rate ####
UNEMP <- get_eurostat(id = "tps00203", time_format = "num",
                           filters = list(time = YEARS, freq = "A", geo=CNTR_eu, unit = "PC_ACT")) %>%
  select(geo, time, values) %>%
  rename(UNEMP = values)



# HICP - inflation rate
HICP <- get_eurostat(id = "tec00118", time_format = "num",
                           filters = list(time = YEARS, freq = "A", geo=CNTR_eu)) %>%
  select(geo, time, values) %>%
  rename(HICP = values)


# Governemntal consumption expenditure (Percentage of GDP) ####
G_CONS <- get_eurostat(id = "tec00010", time_format = "num",
                     filters = list(time = YEARS, freq = "A", geo=CNTR_eu, unit = "PC_GDP")) %>%
  select(geo, time, values) %>%
  rename(G_CONS = values)



# General government gross debt - Percentage of gross domestic product (GDP) ####
G_DEBT <- get_eurostat(id = "sdg_17_40", time_format = "num",
                        filters = list(time = YEARS, freq = "A", geo=CNTR_eu, unit = "PC_GDP")) %>%
  select(geo, time, values) %>%
  rename(G_DEBT = values)


# Adjusted gross disposable income of households per capita in PPS ####
DISP_INC_hh <- get_eurostat(id = "tec00113", time_format = "num",
                         filters = list(time = YEARS, freq = "A", geo=CNTR_eu)) %>%
  select(geo, time, values) %>%
  rename(DISP_INC_hh = values)



# Export of goods and servieces in % of GDP ####
EXP <- get_eurostat(id = "tet00003", time_format = "num",
                         filters = list(time = YEARS, freq = "A", geo=CNTR_eu,
                                        na_item="P6")) %>%
  select(geo, time, values) %>%
  rename(EXP = values)
  


dfs <- list(DISP_INC_hh, EXP, G_CONS, G_DEBT, GDP_pc,
                  HICP, UNEMP)
data <- reduce(dfs, function(x, y) full_join(x, y, by = c("geo", "time")))

rm(DISP_INC_hh, EXP, G_CONS, G_DEBT, GDP_pc,
   HICP, UNEMP)
rm(dfs)


#Przykładowy wykres

p <- ggplot(data) +
  aes(x = time, y = GDP_pc, color = geo) +
  geom_line() + 
  geom_text(data = data %>% filter(time == 2023), aes(label = geo),
           hjust = -0.1, show.legend = FALSE) +
  scale_x_continuous(breaks = seq(2013, 2023, by = 2))

ggsave(filename = "Wykres.png", plot = p, width = 3200, height = 2400, units = "px")
p
rm(p)



