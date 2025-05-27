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

test_matrix <- matrix(ncol = length(vars), nrow = length(YEARS))
colnames(test_matrix) <- vars
rownames(test_matrix) <- YEARS

#Test normalnoći Shapiro-Wilka..................................................
shapiro_matrix <- test_matrix

for(i in vars){
  for(j in YEARS){
    p_val <- shapiro.test(unlist(data[data$time==j, i]))$p.value
    shapiro_matrix[as.character(j),i] <- round(p_val, 4)
  }
}
rm(i, j, p_val)

shapiro_matrix

#Współczynniki skośności........................................................
library(e1071)

skewness_matrix <- test_matrix

for(i in vars){
  for(j in YEARS){
    skew <- skewness(unlist(data[data$time==j, i]), na.rm = T)
    skewness_matrix[as.character(j),i] <- round(skew, 4)
  }
}
rm(i, j, skew)

skewness_matrix

#Współczynniki kurtozy.........................................................

kurtosis_matrix <- test_matrix

for(i in vars){
  for(j in YEARS){
    kurt <- kurtosis(unlist(data[data$time==j, i]), na.rm = T)
    kurtosis_matrix[as.character(j),i] <- round(kurt, 4)
  }
}
rm(i, j, kurt)

kurtosis_matrix
