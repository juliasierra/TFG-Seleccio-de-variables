## Bucle per al model log penalized
for (i in seq_along(comb)) {  
  N <- comb[[i]]$N
  pi <- comb[[i]]$pi
  falsos_positius_pen <- numeric(iters)
  
  for (j in 1:iters) {  
    print(paste("Escenari", i, "- Simulació", j))
    X_sim <- X[[i]][[j]]
    colnames(X_sim) <- paste0("X", 1:num_vars)
    y_sim <- Y[[i]][[j]]
    df <- data.frame(y = y_sim, X_sim)
    model_pen <- logistf(y ~ ., data = df, pl = FALSE)
    falsos_positius_pen[j] <- calcula_fp(model_pen, penalized = TRUE)
  }
  
  mitjana_pen <- mean(falsos_positius_pen, na.rm = TRUE)
  resultats_pen <- rbind(resultats_pen, data.frame(N, pi, FP = mitjana_pen))
}

print("Resultats Model Log Penalized")
print(resultats_pen)
setwd("~/Documents/uni/4t/TFG/models")
save(resultats_pen, file = "resultats_pen.RData")
