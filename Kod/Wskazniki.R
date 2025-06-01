library(ineq)      

#WSPÓŁCZYNNIK KONCENTRACJI K JAKO MIARA ZRÓŻNICOWANIA DOCHODÓW I INWESTYCJI GMIN 
# Jan Czempas 2012


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

vars <- c("DISP_INC_hh", "EXP", "G_CONS", "G_DEBT", "GDP_pc", "UNEMP")

#Templatka macierzy przechowującej wyniki wskaznikow
#Kolumny to zmienne, rzędy to lata
wsk_mat <- matrix(ncol = length(vars), nrow = length(YEARS))
colnames(wsk_mat) <- vars
rownames(wsk_mat) <- YEARS


# MACIERZ WARTOŚCI GINIEGO 
gini_mat<-wsk_mat

for (i in vars){
  for (j in YEARS){
    gini <- ineq(unlist(data[data$time==j, i]), type = "Gini")
    gini_mat[as.character(j),i] <- round(gini, 4)
  }
}
gini_mat



# MACIERZ WARTOŚCI KUKUŁY 
kuk_mat<-wsk_mat

a <- (27 + sqrt(27))/(27-1)
for (i in vars){
  for (j in YEARS){
    f<-sqrt(sum(apply(X=data[data$time==j, i], MARGIN = 1,
             FUN = function(x) x^2))/(sum(data[data$time==j, i])^2))
    kuk_mat[as.character(j),i] <- round((a*(f-1)) + 1 ,4)
    }
}
kuk_mat


rm(a, i, j, gini, f)
rm(wsk_mat, var_names, lorenz_curve, vars)
# rm(gini_mat, kuk_mat)







