# RANDOM FOREST
setwd("~/Documents/uni/4t/TFG/metodes 50/metodes50_def")
library(bigstep)
library(randomForest)

# Funció per seleccionar les variables més importants segons importància
escoger_rf <- function(x, porcentage_min = 3) {
  nada <- NA
  if (max(x) < porcentage_min) { #si importància màx és menor que el llindar, no es considera cap rellevant
    return(nada)
  }
  l <-length(x)
  cog <- c() #vector buit que s'omplirà amb les diferències absolutes 
  for (i in 1:(l - 1)) {
    cog <- c(cog, abs(x[i] - x[i + 1])) #diferència absoluta entre cada parella de valors consecutius
  }
  punt.tall2 <- which.max(cog) #posició on hi ha la màxima diferència entre valors consecutius 
  return(names(x)[1:punt.tall2])
}

escoger_rf_v2 <- function(x, porcentage_min = 3) {
  nada <- NA
  if (max(x) < porcentage_min) { #si importància màx és menor que el llindar, no es considera cap rellevant
    return(nada)
  }
  l <-length(x)
  cog <- c() #vector buit que s'omplirà amb les diferències absolutes 
  for (i in 1:(l - 1)) {
    cog <- c(cog, abs(x[i] - x[i + 1])) #diferència absoluta entre cada parella de valors consecutius
  }
  cog2 <- sort(cog, decreasing = T)
  punt.tall2 <- cog2[2] #posició on hi ha la màxima diferència entre valors consecutius 
  return(names(x)[1:punt.tall2])
}



# Resultats
resultats_rf <- data.frame(
  N = integer(), pi = numeric(), iter = integer(),
  Sensibilitat = numeric(), Especificitat = numeric(),
  VPP = numeric(), VPN = numeric(), Accuracy = numeric(),
  NetBenefit = numeric()
)
vars_seleccionades_rf <- list()
system.time({
  for (i in seq_along(comb)) {
    N <- comb[[i]]$N
    pi <- comb[[i]]$pi
    metrics <- list()
    vars_selec_bin <- matrix(0, nrow = iters, ncol = 100) #creem matriu per emmagatzemar les variables seleccionades
    
    for (j in 1:iters) {
      print(paste("Escenari", i, "- Simulació", j))
      X_df <- as.data.frame(X[[i]][[j]])
      y_fac <- as.factor(Y[[i]][[j]])
      model_rf <- randomForest(x = X_df, y = y_fac, importance = TRUE) #model de Random Forest amb càlcul de la importància de variables
      importancia <- importance(model_rf, type = 1)  # mportàncies de les variables
      importancia_ordenada <- sort(importancia[, 1], decreasing = TRUE) #importàncies en ordre descendent
      vars_seleccionades <- as.factor(escoger_rf(importancia_ordenada, porcentage_min = 3)) #variables més importants segons el tall
      vars_selec_bin[j, vars_seleccionades] <- 1
      metrics[[j]] <- metriques(vars_seleccionades)
    }
    
    resultats_rf <- rbind(resultats_rf, data.frame(
      N = N, pi = pi,
      Sensibilitat = mean(sapply(metrics, function(x) x["sensibilitat"]), na.rm = TRUE),
      Especificitat = mean(sapply(metrics, function(x) x["especificitat"]), na.rm = TRUE),
      VPP = mean(sapply(metrics, function(x) x["VPP"]), na.rm = TRUE),
      VPN = mean(sapply(metrics, function(x) x["VPN"]), na.rm = TRUE),
      Accuracy = mean(sapply(metrics, function(x) x["accuracy"]), na.rm = TRUE),
      NetBenefit = mean(sapply(metrics, function(x) x["net_benefit"]), na.rm = TRUE)
    ))
    vars_seleccionades_rf[[i]] <- vars_selec_bin
  }
})

print(resultats_rf)
save(resultats_rf,vars_seleccionades_rf, file = "randomForest50_def.RData")
#user    system   elapsed 
#22868.381    68.973 27677.668  