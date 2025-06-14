#ELASTIC NET
setwd("~/Documents/uni/4t/TFG/metodes 50/metodes50_def")
library(glmnet)

# Inicialitzem els resultats
resultats_enet <- data.frame(
  N = integer(), pi = numeric(), iter = integer(),
  Sensibilitat = numeric(), Especificitat = numeric(),
  VPP = numeric(), VPN = numeric(), Accuracy = numeric(),
  NetBenefit = numeric()
)
vars_seleccionades_enet <- list()
# Ajust i càlcul
system.time({
  for (i in seq_along(comb)) {
    N <- comb[[i]]$N
    pi <- comb[[i]]$pi
    metrics <- list()
    vars_selec_bin <- matrix(0, nrow = iters, ncol = 100) #creem matriu per emmagatzemar les variables seleccionades
    for (j in 1:iters) {
      print(paste("Escenari", i, "- Simulació", j))
      X_mat <- as.matrix(X[[i]][[j]])
      y_vec <- Y[[i]][[j]]
      # Ajust del model Elastic Net (alpha = 0.5)
      lambda_estim <- cv.glmnet(X_mat, y_vec, family = "binomial", alpha = 0.5)
      model_enet <- glmnet(X_mat, y_vec, alpha = 0.5, family = "binomial", lambda = lambda_estim$lambda.min)
      
      # Càlcul de mètriques
      #vars_seleccionades <- which(coefficients(model_enet)[-1] != 0)
      vars_seleccionades <- which(coefficients(model_enet)[-1] >= sd(as.vector(X[[i]][[j]]))/20)
      vars_selec_bin[j, vars_seleccionades] <- 1
      metrics[[j]] <- metriques(vars_seleccionades, nv=num_vars)
      
      
    }
    resultats_enet <- rbind(resultats_enet, data.frame(
      N = N, pi = pi,
      N = N, pi = pi,
      Sensibilitat = mean(sapply(metrics, function(x) x["sensibilitat"]), na.rm = TRUE),
      Especificitat = mean(sapply(metrics, function(x) x["especificitat"]), na.rm = TRUE),
      VPP = mean(sapply(metrics, function(x) x["VPP"]), na.rm = TRUE),
      VPN = mean(sapply(metrics, function(x) x["VPN"]), na.rm = TRUE),
      Accuracy = mean(sapply(metrics, function(x) x["accuracy"]), na.rm = TRUE),
      NetBenefit = mean(sapply(metrics, function(x) x["net_benefit"]), na.rm = TRUE)
    ))
    vars_seleccionades_enet[[i]] <- vars_selec_bin
  }
})


# Guardem resultats
print(resultats_enet)
save(resultats_enet, vars_seleccionades_enet, file = "resultats_enet_v2.RData")
#user  system elapsed 
#423.710  19.803 451.373 
#488.563  21.267 522.451  (/20)