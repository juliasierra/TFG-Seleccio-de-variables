## LASSO (arreglat)
setwd("~/Documents/uni/4t/TFG/metodes nul")
library(glmnet)
resultats_fp_lasso <- data.frame(N = numeric(), pi = numeric(), FP = numeric(), FP_percent=numeric())

system.time({
for (i in seq_along(comb)) {
  N <- comb[[i]]$N
  pi <- comb[[i]]$pi
  fp_lasso <- numeric(iters)
  
  for (j in 1:iters) {
    print(paste("Escenari", i, "- Simulació", j))
    X_mat <- as.matrix(X[[i]][[j]])
    y_bin <- Y[[i]][[j]]
    
    lambda_estim <- cv.glmnet(X_mat, y_bin, family="binomial", alpha=1)
    model_lasso <- glmnet(X_mat, y_bin, alpha = 1, family = "binomial", lambda = lambda_estim$lambda.min)
    coef_lasso <- model_lasso$beta[,ncol(model_lasso$beta)]
    fp_lasso[j] <- sum(coef_lasso != 0)
  }
  
  resultats_fp_lasso <- rbind(resultats_fp_lasso, data.frame(N = N, pi = pi, FP = mean(fp_lasso), FP_percent = mean(fp_lasso)/num_vars))
}
})
print(resultats_fp_lasso)
save(resultats_fp_lasso, file = "resultats_fp_lasso.RData")
