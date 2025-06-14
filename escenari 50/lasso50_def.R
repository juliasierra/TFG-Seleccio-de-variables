#LASSO
library(glmnet)

# Inicialització del dataframe de resultats
resultats_lasso <- data.frame(
  N = integer(), pi = numeric(), 
  Sensibilitat = numeric(), Especificitat = numeric(),
  VPP = numeric(), VPN = numeric(), Accuracy = numeric(),
  NetBenefit = numeric()
)
vars_seleccionades_lasso <- list()

# Bucle de simulació
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
      
      # Ajust del model LASSO
      lambda_estim <- cv.glmnet(X_mat, y_vec, family = "binomial", alpha = 1)
      model_lasso <- glmnet(X_mat, y_vec, alpha = 1, family = "binomial",lambda = lambda_estim$lambda.min)
      #vars_seleccionades <- which(coefficients(model_lasso)[-1] != 0)
      vars_seleccionades <- which(coefficients(model_lasso)[-1] >= sd(as.vector(X[[i]][[j]]))/20)
      vars_selec_bin[j, vars_seleccionades] <- 1
      metrics[[j]] <- metriques(vars_seleccionades)
      
      
    }
    resultats_lasso <- rbind(resultats_lasso, data.frame(
      N = N, pi = pi,
      Sensibilitat = mean(sapply(metrics, function(x) x["sensibilitat"]), na.rm = TRUE),
      Especificitat = mean(sapply(metrics, function(x) x["especificitat"]), na.rm = TRUE),
      VPP = mean(sapply(metrics, function(x) x["VPP"]), na.rm = TRUE),
      VPN = mean(sapply(metrics, function(x) x["VPN"]), na.rm = TRUE),
      Accuracy = mean(sapply(metrics, function(x) x["accuracy"]), na.rm = TRUE),
      NetBenefit = mean(sapply(metrics, function(x) x["net_benefit"]), na.rm = TRUE)
    ))
    vars_seleccionades_lasso[[i]] <- vars_selec_bin
    
  }
})

# Guardar
print(resultats_lasso)
save(resultats_lasso, vars_seleccionades_lasso, file = "lasso50_v2.RData")
# user  system elapsed 
#421.331  24.889 460.396 
#489.308  25.244 534.188 (/20)
