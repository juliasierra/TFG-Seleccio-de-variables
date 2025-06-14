#BORUTA
library(Boruta)

# DataFrame per guardar les mètriques
resultats_boruta <- data.frame(
  N = integer(), pi = numeric(), 
  Sensibilitat = numeric(), Especificitat = numeric(),
  VPP = numeric(), VPN = numeric(), Accuracy = numeric(),
  NetBenefit = numeric()
)
vars_seleccionades_boruta <- list()

# Bucle principal
system.time({
  for (i in seq_along(comb)) {
    N <- comb[[i]]$N
    pi <- comb[[i]]$pi
    metrics <- list()
    vars_selec_bin <- matrix(0, nrow = iters, ncol = 100) #creem matriu per emmagatzemar les variables seleccionades
    
    for (j in 1:iters) {
      print(paste("Escenari", i, "- Simulació", j))
      X_df <- as.data.frame(X[[i]][[j]])
      y <- Y[[i]][[j]]
      df_boruta <- data.frame(y = y, X_df)
      
      # Selecció de variables
      model_boruta <- Boruta(y ~ ., data = df_boruta, doTrace = 0)
      vars_seleccionades <- as.factor(getSelectedAttributes(model_boruta, withTentative = FALSE))
      vars_selec_bin[j, vars_seleccionades] <- 1
      metrics[[j]] <- metriques(vars_seleccionades)
    
    }
    
    # Guardem resultats
    resultats_boruta <- rbind(resultats_boruta, data.frame(
      N = N, pi = pi,
      Sensibilitat = mean(sapply(metrics, function(x) x["sensibilitat"]), na.rm = TRUE),
      Especificitat = mean(sapply(metrics, function(x) x["especificitat"]), na.rm = TRUE),
      VPP = mean(sapply(metrics, function(x) x["VPP"]), na.rm = TRUE),
      VPN = mean(sapply(metrics, function(x) x["VPN"]), na.rm = TRUE),
      Accuracy = mean(sapply(metrics, function(x) x["accuracy"]), na.rm = TRUE),
      NetBenefit = mean(sapply(metrics, function(x) x["net_benefit"]), na.rm = TRUE)
    ))
    vars_seleccionades_boruta[[i]] <- vars_selec_bin
  }
})

# Guardem els resultats
save(resultats_boruta, file = "resultats_boruta.RData")
print(resultats_boruta)

#user    system   elapsed 
#39171.907   151.085 20863.243 