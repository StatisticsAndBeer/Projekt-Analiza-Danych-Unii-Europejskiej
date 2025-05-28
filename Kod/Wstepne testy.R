#TESTY STATYSTYCZNE=============================================================
#Testowanie normalności---------------------------------------------------------
#Przygotowanie..................................................................
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

#Templatka macierzy przechowującej wyniki testów statystycznych i statystyk
#Kolumny to zmienne, rzędy to lata
test_mat <- matrix(ncol = length(vars), nrow = length(YEARS))
colnames(test_mat) <- vars
rownames(test_mat) <- YEARS

#Test normalnoći Shapiro-Wilka..................................................
shapiro_mat <- test_mat

for(i in vars){
  for(j in YEARS){
    p_val <- shapiro.test(unlist(data[data$time==j, i]))$p.value
    shapiro_mat[as.character(j),i] <- round(p_val, 4)
  }
}
rm(i, j, p_val)

shapiro_mat

#Wnioski:
#Większość zmiennych nie ma rozkładów normalnych, należy zastosować testy 
#nieparametryczne.

#Współczynniki skośności........................................................
library(e1071)

skewness_mat <- test_mat

for(i in vars){
  for(j in YEARS){
    skew <- skewness(unlist(data[data$time==j, i]), na.rm = T)
    skewness_mat[as.character(j),i] <- round(skew, 4)
  }
}
rm(i, j, skew)

skewness_mat

#Współczynniki kurtozy.........................................................

kurtosis_mat <- test_mat

for(i in vars){
  for(j in YEARS){
    kurt <- kurtosis(unlist(data[data$time==j, i]), na.rm = T)
    kurtosis_mat[as.character(j),i] <- round(kurt, 4)
  }
}
rm(i, j, kurt)

kurtosis_mat

#Wnioski........................................................................
#Dane dla większości zmiennych nie pochodzą z rozkładu normalnego, 
#Często pojawiają się obserwaje odstające oraz zjawisko leptokurtyczności
#Należy stosować testy nieparametryczne

#Testowanie różnic na przestrzeni lat-------------------------------------------
#Wyznaczenie outlierów..........................................................
#(Na wszelki wypadek - na razie testy nieparametryczne)
outliers <- list(EXP = c("LU"),
                 GDP_pc = c("LU", "IE"),
                 G_CONS = "IE",
                 DISP_INC_hh = "",
                 UNEMP = c("ES", "EL"),
                 HICP = "",
                 G_DEBT = c("EL", "IT"))
#Wyszczególnienie zmiennych o normalnym rozkładzie..............................
#(Na wszelki wypadek - na razie dla wszystkich testy nieparametryczne)
norm_vars <- c("DISP_INC_hh", "G_CONS")

#Test Friedmana.................................................................
friedman_res <- setNames(numeric(length(vars)), vars)

#Wyjaśnienie kodu:
#Dla każdej ze zmiennych przeprowadzamy nieparaemtryczny test friedmana.
#Jeśli przynajmniej w jednym z lat rozkład danej zmiennej różnił się od pozostałych
#jest to podstawą do dalszych badań

for(i in vars){
  #Wypisanie formuły testu
  #Obserwacje i odpowiadające im wartości zmiennej są grupowane względem roku (time) 
  #i parowane względem kraju (geo)
  test_formula <- as.formula(paste(i, " ~ time | geo"))
  
  result <- friedman.test(formula = test_formula, data = data %>% arrange(time, geo))
  print(result)
  friedman_res[i] <- round(result$p.value, 10)
}
rm(i, result, test_formula)

friedman_res

#Wnioski:
#Dla każdej zmiennej p-value bliskie 0 - z dużą pewnością można stwierdić, że
#wartości poszczególnych zmiennych zmieniły się przynajmniej raz na przestrzeni lat


#Testy Wilcoxon'a z roku na rok.................................................
wilcoxon_mat <- test_mat
start_years <- c(2019:2022, 2019)
end_years <- c(2020:2023, 2023)
rownames(wilcoxon_mat) <- paste(start_years, end_years, sep = "=>")

wilcoxon_conf_int <- wilcoxon_mat

#Wyjaśnienie kodu:
#Aby sprawdzić czy wartość danej zmiennej w populacji zmieniła się z roku na rok
#Wykonujemy dla każej zmiennej sparowany test Wilcoxona dla dwóch następujących po sobie
#lat. Np. test dla zmiennej GDP_pc dla lat 2019 i 2020 - sprawdzamy czy populacja
#krajów UE różni się pod względem GDP_pc w latach 2019 i 2020; następnie analogiczne
#porównanie dla lat 2020 i 2021, etc. Na końcu porównujemy rok startowy (2019)
#z końcowym (2023) żeby wykazać potencjalną długoterminową zmianę, nieuchwyconą
#w porównaniach z roku na rok. Schemat powtarzamy dla każdej ze zmiennych.
#Wyniki prezentowane są w macierzy jako wartości p-value danego testu.

for(i in vars){
  for(j in 1:length(start_years)){
    time_step <- paste(start_years[j], end_years[j], sep = "=>")
    
    #Wybór alternatywy testu - "greater" gdy mediana w roku pierwszym jest 
    #większa niż w roku następującym po nim, "less" w przeciwnym wyadku.
    median_start <- median(unlist(data[data$time==start_years[j], i]), na.rm = TRUE)
    median_end <- median(unlist(data[data$time==end_years[j], i]), na.rm = TRUE)
    
    test_alt <- ifelse(median_end - median_start < 0, "greater", "less")
    
    #Wybranie z zestawu danych jedynie obserwacji dla dwóch testowanych lat
    #(konieczne do działania testu)
    test_data <- data %>%
      filter(time %in% c(start_years[j], end_years[j])) %>%
      arrange(time, geo)
      
    #Wypisanie formuły testu  
    #Obserwacje i odpowiadające im awrtości zmiennej są grupowane względem roku (time)
    #i domyślnie parowane względem kraju
    test_formula <- as.formula(paste(i, " ~ time", sep = ""))
    results <- wilcox.test(formula = test_formula, data = test_data,
                           paired = TRUE, alternative = test_alt,
                           conf.int = TRUE, exact = FALSE)
    conf_int <- round(results$conf.int, 4)
    
    print(time_step)
    print(results)
    wilcoxon_mat[time_step, i] <- round(results$p.value, 5)
    wilcoxon_conf_int[time_step, i ] <- paste(conf_int[1], conf_int[2], sep = "; ")
  }
}

rm(i, j, results, test_formula, median_start, median_end, time_step, test_alt, 
   test_data, start_years, end_years)

wilcoxon_mat
wilcoxon_conf_int

#Wnioski:
#Co do zasady, wartości zmiennych zmieniały się na przestrzeni lat. Można jednak
#Wyróżnic poszczególne sytuacje, gdy pozostały one takie same na początku i końcu
#okresu, co oznacza stabilizację po pandemii i wojnie




