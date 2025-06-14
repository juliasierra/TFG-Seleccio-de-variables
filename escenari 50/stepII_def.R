## STEPWISE 
setwd("~/Documents/uni/4t/TFG/metodes 50")
library(bigstep)

resultats_stepII <- data.frame(
  N = integer(), pi = numeric(),
  Sensibilitat = numeric(), Especificitat = numeric(),
  VPP = numeric(), VPN = numeric(), Accuracy = numeric(),
  NetBenefit = numeric()
)
vars_seleccionades_stepII <- list()

system.time({
  for (i in seq_along(comb)) {
    N <- comb[[i]]$N
    pi <- comb[[i]]$pi
    metrics <- list()
    vars_selec_bin <- matrix(0, nrow = iters, ncol = 100) #creem matriu per emmagatzemar les variables seleccionades
    
    for (j in 1:iters) {
      print(paste("Escenari", i, "- Simulació", j))
      # Preparem dades
      X_sub <- X[[i]][[j]] # seleccionem la comb i i la iteració j
      colnames(X_sub) <- paste0("X", 1:ncol(X_sub))
      Y_bin <- as.vector(Y[[i]][[j]])
      
      d <- prepare_data(y = Y_bin, X_sub, type = "logistic")
      model_step <- stepwise(d, crit = aic)
      
      #X_selected <- as.data.frame(X_sub[, model_step$model, drop = FALSE]) codi antic
      #model_final <- glm(Y_bin ~ ., data = X_selected, family = binomial())
    
      vars_seleccionades <- as.factor(model_step$model)
      vars_selec_bin[j, vars_seleccionades] <- 1
      metrics[[j]] <- metriques(vars_seleccionades)
    }
    # Guardar resultats
    resultats_stepII <- rbind(resultats_stepII, data.frame(
      N = N, pi = pi,
      Sensibilitat = mean(sapply(metrics, function(x) x["sensibilitat"]), na.rm = TRUE),
      Especificitat = mean(sapply(metrics, function(x) x["especificitat"]), na.rm = TRUE),
      VPP = mean(sapply(metrics, function(x) x["VPP"]), na.rm = TRUE),
      VPN = mean(sapply(metrics, function(x) x["VPN"]), na.rm = TRUE),
      Accuracy = mean(sapply(metrics, function(x) x["accuracy"]), na.rm = TRUE),
      NetBenefit = mean(sapply(metrics, function(x) x["net_benefit"]), na.rm = TRUE)
    ))
    vars_seleccionades_stepII[[i]] <- vars_selec_bin
  }
})
print (resultats_stepII)

# Guardem els resultats
save(resultats_stepII,vars_seleccionades_stepII, file = "step50_def.RData")

#  user   system  elapsed 
#105157.30  17361.04 157560.27 

