## ELASTIC NET
setwd("~/Documents/uni/4t/TFG/metodes nul")
library(glmnet)

resultats_fp_enet <- data.frame(N = integer(), pi = numeric(), FP = numeric(), FP_percent=numeric())
system.time({
for (i in seq_along(comb)) {
  N <- comb[[i]]$N
  pi <- comb[[i]]$pi
  fp_enet <- numeric(iters)
  
  for (j in 1:iters) {
    print(paste("Escenari", i, "- Simulació", j))
    # Agafem N files i num_vars columnes
    X_mat <- as.matrix(X[[i]][[j]])
    # Generem una nova Y binària amb la prevalença pi
    y_bin <- Y[[i]][[j]]
    # Ajustem el model Elastic Net (alpha = 0.5)
    lambda_estim <- cv.glmnet(X_mat, y_bin, family="binomial", alpha=0.5)
    model_enet <- glmnet(X_mat, y_bin, alpha = 0.5, family = "binomial", lambda = lambda_estim$lambda.min)
    coef_enet <- coef(model_enet)[-1]
    
    # Comptem falsos positius (coeficients no zero)
    fp_enet[j] <- sum(coef_enet != 0)
  }
  
  resultats_fp_enet <- rbind(resultats_fp_enet, data.frame(N = N, pi = pi, FP = mean(fp_enet), FP_percent=mean(fp_enet)/num_vars))
}
})
print(resultats_fp_enet)
save(resultats_fp_enet, file = "resultats_fp_enet.RData")

#user  system elapsed 
#316.516   3.309 321.026 