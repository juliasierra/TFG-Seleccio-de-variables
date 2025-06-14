## BOOTSTRAP (arreglat)
setwd("~/Documents/uni/4t/TFG/metodes nul")
library(bigstep)

# Iteracions pel bootstrap
iters_boot <- iters*2  
num_vars2 <- num_vars/2
# Inicialització
B <- 20
resultats_fp_boot <- data.frame(N = integer(), pi = numeric(), FP = numeric(), FP_percent = numeric())

system.time({
for (i in seq_along(comb)) {
  N <- comb[[i]]$N
  pi <- comb[[i]]$pi
  fp_boot <- numeric(iters_boot)
  
 
  
  for (j in 1:iters_boot) {
    print(paste("Escenari", i, "- Simulació", j))
    X_base <- X2[[i]][[j]]
    y_base <- Y2[[i]][[j]]
    n_boot <- nrow(X_base)
    
    seleccio_vars <- matrix(0, nrow = B, ncol = num_vars2) #matriu que recull si una variable és seleccionada o no
    colnames(seleccio_vars) <- paste0("X", 1:num_vars2)
    
    for (b in 1:B) {
      idx_boot <- sample(1:n_boot, n_boot, replace = TRUE) #mostra bootstrap amb repetició
      X_boot <- X_base[idx_boot, ]
      y_boot <- y_base[idx_boot]
      
      d <- prepare_data(y = y_boot, X_boot, type="logistic")
      model <- stepwise(d, crit = aic)
      
      seleccio_vars[b, as.numeric(model$model)] <- 1 #marca les variables seleccionades a seleccio_vars
    }
    
    freq_seleccio <- colMeans(seleccio_vars) #freqüència amb què s'ha seleccionat cada variable 
    fp_boot[j] <- sum(freq_seleccio > 0.75) # Fals positiu si ha estat seleccionada almenys el 75% dels cops
  }
  
  resultats_fp_boot <- rbind(resultats_fp_boot, data.frame(N = N, pi = pi, FP = mean(fp_boot), FP_percent = mean(fp_boot) / num_vars2))
                            
  
}
})

print(resultats_fp_boot)
save(resultats_fp_boot, freq_seleccio, fp_boot, file = "resultats_fp_boot.RData")

#   user     system    elapsed 
#179611.331   8981.673 209074.718