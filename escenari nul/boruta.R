## BORUTA
setwd("~/Documents/uni/4t/TFG/metodes nul")
library(Boruta)
resultats_fp_boruta <- data.frame(N = integer(), pi = numeric(), FP = numeric(), FP_percent=numeric())

system.time({
  for (i in seq_along(comb)) {
    N <- comb[[i]]$N
    pi <- comb[[i]]$pi
    fp_boruta <- numeric(iters)  # vector buit per acumular FPs
    
    for (j in 1:iters) {
      print(paste("Escenari", i, "- Simulació", j))
      X_df <- as.data.frame(X[[i]][[j]]) #dades per a l’escenari i simulació actual
      y_fac <- Y[[i]][[j]]
      df_boruta <- data.frame(y = y_fac, X_df)
      
      model_boruta <- Boruta(y ~ ., data = df_boruta, doTrace = 0) #model Boruta per seleccionar variables rellevants per predir y
      seleccionades <- getSelectedAttributes(model_boruta, withTentative = FALSE) #variables seleccionades com a rellevants pel model
      
      fp_boruta[j] <- length(seleccionades)  # guardem el nombre de variables seleccionades
    }
    
    resultats_fp_boruta <- rbind(resultats_fp_boruta,
                                 data.frame(N = N, pi = pi, FP = mean(fp_boruta), FP_percent=mean(fp_boruta)/num_vars))
  }
})

print(resultats_fp_boruta)

save(resultats_fp_boruta, file = "resultats_fp_boruta.RData")

#user     system    elapsed 
#677854.238   4438.846 247817.997 