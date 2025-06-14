# RANDOM FOREST (sense Boruta)
setwd("~/Documents/uni/4t/TFG/metodes nul")
library(randomForest)

# Funció per seleccionar les variables més importants segons importància
escoger_rf <- function(x, porcentage_min = 3) {
  nada <- NA
  if (max(x) < porcentage_min) {
    return(nada)
  }
  l <- length(x)
  cog <- c()
  for (i in 1:(l - 1)) {
    cog <- c(cog, abs(x[i] - x[i + 1]))
  }
  punt.tall2 <- which.max(cog)
  return(names(x)[1:punt.tall2])
}

# Resultats
resultats_fp_rf <- data.frame(N = integer(), pi = numeric(), FP = numeric(), FP_percent = numeric())

system.time({
  for (i in seq_along(comb)) {
    N <- comb[[i]]$N
    pi <- comb[[i]]$pi
    fp_rf <- numeric(iters)
    
    for (j in 1:iters) {
      print(paste("Escenari", i, "- Simulació", j))
      X_df <- as.data.frame(X[[i]][[j]])
      y_fac <- as.factor(Y[[i]][[j]])
      model_rf <- randomForest(x = X_df, y = y_fac, importance = TRUE) #model de Random Forest amb càlcul de la importància de variables
      importancia <- importance(model_rf, type = 1)  # mportàncies de les variables
      importancia_ordenada <- sort(importancia[, 1], decreasing = TRUE) #importàncies en ordre descendent
      seleccionades <- escoger_rf(importancia_ordenada, porcentage_min = 3) #variables més importants segons el tall
      fp_rf[j] <- ifelse(is.na(seleccionades[1]), 0, length(seleccionades))
    }
    
    num_vars <- ncol(X[[i]][[1]])  # Nombre de variables explicatives (100)
    resultats_fp_rf <- rbind(resultats_fp_rf, data.frame(N = N,pi = pi,FP = mean(fp_rf),FP_percent = mean(fp_rf) / num_vars))
  }
})

print(resultats_fp_rf)
save(resultats_fp_rf, file = "resultats_fp_rf_randomForest.RData")
#user    system   elapsed 
#28688.629    96.248 30792.522 