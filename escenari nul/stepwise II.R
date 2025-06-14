##STEPWISE II (comprovar si funciona i correr)
# Carrega el paquet
library(bigstep)


# Resultats
resultats_fp_bigstep <- data.frame(N = integer(), pi = numeric(), FP = numeric(), FP_percent = numeric())

# Iteracions amb bigstep::stepwise
system.time({
  for (i in seq_along(comb)) {
    N <- comb[[i]]$N
    pi <- comb[[i]]$pi
    fp_bigstep <- numeric(iters)
    
    for (j in 1:iters) {
      print(paste("Escenari", i, "- Simulació", j))
      # Preparem les dades
      X_sub <- X[[i]][[j]]
      Y_bin <- Y[[i]][[j]]
      # Construcció de la taula en format bigstep
      dades_big <- prepare_data(Y_bin, X_sub)
      
      # Model stepwise amb criteri BIC (per defecte)
      model_big <- stepwise(dades_big, crit = aic)
      
      # Comptem quants predictors s’han seleccionat (falsos positius en el model nul)
      fp_bigstep[j] <- length(model_big$model)
    }
    
    resultats_fp_bigstep <- rbind(resultats_fp_bigstep,
                                  data.frame(N = N, pi = pi,
                                             FP = mean(fp_bigstep),
                                             FP_percent = mean(fp_bigstep) / num_vars))
  }
})

# Mostra i desa resultats
print(resultats_fp_bigstep)
save(resultats_fp_bigstep, file = "resultats_fp_bigstep.RData")

#user    system   elapsed 
#65601.79   3090.71 104402.46 

