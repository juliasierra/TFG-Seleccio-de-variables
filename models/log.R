for (i in seq_along(comb)) {
  N <- comb[[i]]$N
  pi <- comb[[i]]$pi
  falsos_positius_log <- numeric(iters)
  
  for (j in 1:iters) {
    print(paste("Escenari", i, "- Simulació", j))
    X_sim <- X[[i]][[j]]
    colnames(X_sim) <- paste0("X", 1:num_vars)
    y_sim <- Y[[i]][[j]]
    df <- data.frame(y = y_sim, X_sim)
    model_log <- tryCatch(glm(y ~ ., data = df, family = binomial(link="log")), #chat (no dona error, sino que dona NA, ns si està bé)
                          error = function(e) NULL)
    falsos_positius_log[j] <- if (!is.null(model_log)) calcula_fp(model_log) else NA
  }
  mitjana_log <- mean(falsos_positius_log, na.rm = TRUE)
  resultats_log <- rbind(resultats_log, data.frame(N, pi, FP = mitjana_log))
}
print("Resultats Model Log Binomial")
print(resultats_log)
setwd("~/Documents/uni/4t/TFG/models")
save(resultats_log, file = "resultats_log.RData")
