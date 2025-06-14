## BOOTSTRAP
setwd("~/Documents/uni/4t/TFG/metodes 50/metodes50_def")
library(bigstep)

# Inicialització
B <- 20
resultats_boot50 <- data.frame(
  N = integer(), pi = numeric(),
  Sensibilitat = numeric(), Especificitat = numeric(),
  VPP = numeric(), VPN = numeric(), Accuracy = numeric(),
  NetBenefit = numeric()
)
vars_seleccionades_boot <- list()
system.time({
  for (i in seq_along(comb)) {
    N <- comb[[i]]$N
    pi <- comb[[i]]$pi
    metrics <- list()
    vars_selec_bin <- matrix(0, nrow = iters_boot, ncol = num_vars2) #creem matriu per emmagatzemar les variables seleccionades
    
    for (j in 1:iters_boot) {
      print(paste("Escenari", i, "- Simulació", j))
      X_base <- X2[[i]][[j]]
      y_base <- Y2[[i]][[j]]
      
      
      seleccio_vars <- matrix(0, nrow = B, ncol = num_vars2) #matriu que recull si una variable és seleccionada o no
      colnames(seleccio_vars) <- paste0("X", 1:num_vars2)
      
      for (b in 1:B) {
        idx_boot <- sample(1:N, N, replace = TRUE) #mostra bootstrap amb repetició
        X_boot <- X_base[idx_boot, ]
        y_boot <- y_base[idx_boot]
        
        d <- prepare_data(y = y_boot, X_boot, type="logistic")
        model <- stepwise(d, crit = aic)
        
        seleccio_vars[b, as.factor(model$model)] <- 1 #marca les variables seleccionades a seleccio_vars
      }
      
      freq_seleccio <- colMeans(seleccio_vars) #freqüència amb què s'ha seleccionat cada variable 
      # Mètriques
      vars_seleccionades <- which (freq_seleccio>0.75)
      vars_selec_bin[j, vars_seleccionades] <- 1
      metrics[[j]] <- metriques(vars_seleccionades)
      
      
    }
    # Guardar resultats
    resultats_boot50 <- rbind(resultats_boot50, data.frame(
      N = N, pi = pi,
      Sensibilitat = mean(sapply(metrics, function(x) x["sensibilitat"]), na.rm = TRUE),
      Especificitat = mean(sapply(metrics, function(x) x["especificitat"]), na.rm = TRUE),
      VPP = mean(sapply(metrics, function(x) x["VPP"]), na.rm = TRUE),
      VPN = mean(sapply(metrics, function(x) x["VPN"]), na.rm = TRUE),
      Accuracy = mean(sapply(metrics, function(x) x["accuracy"]), na.rm = TRUE),
      NetBenefit = mean(sapply(metrics, function(x) x["net_benefit"]), na.rm = TRUE)
    ))
    vars_seleccionades_boot[[i]] <- vars_selec_bin
    
    
  }
})

print(resultats_boot50)
save(resultats_boot50, vars_seleccionades_boot, file = "boot50_def.RData")

#  user    system   elapsed 
#14739.486   281.203 72482.788 



library(caret)
metriques <- function(vars_seleccionades, p=0.5, num_vars) {
  num_sig <- num_vars/2
  real_labels <- c(rep(1, num_sig), rep(0, num_vars - num_sig)) #variables que s'haurien (o no) de seleccionar
  pred_labels <- rep(0, num_vars)
  pred_labels[vars_seleccionades] <- 1 #posem 1 en les variables que ha seleccionat el model
  
  # creem matriu de confusió
  cm <- confusionMatrix(as.factor(pred_labels), as.factor(real_labels), positive = "1") 
  TP <- cm$table[2, 2] 
  FP <- cm$table[2, 1]
  FN <- cm$table[1, 2]
  TN <- cm$table[1, 1]
  
  # calculem mètriques
  sensibilitat <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  especificitat <- ifelse((TN + FP) == 0, NA, TN / (TN + FP))
  VPP <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  VPN <- ifelse((TN + FN) == 0, NA, TN / (TN + FN))
  accuracy <- (TP + TN) / (TP + TN + FP + FN)
  net_benefit <- (TP / num_vars) - (FP / num_vars) * (p / (1 - p))
  
  return(c(sensibilitat = sensibilitat,
           especificitat = especificitat,
           VPP = VPP,
           VPN = VPN,
           accuracy = accuracy,
           net_benefit = net_benefit))
}
#####################################################
resultats_boot50 <- data.frame(
  N = integer(), pi = numeric(),
  Sensibilitat = numeric(), Especificitat = numeric(),
  VPP = numeric(), VPN = numeric(), Accuracy = numeric(),
  NetBenefit = numeric()
)

system.time({
  for (i in seq_along(comb)) {
    N <- comb[[i]]$N
    pi <- comb[[i]]$pi
    metrics <- list()
    vars_selec_bin <- matrix(0, nrow = iters_boot, ncol = num_vars2) #creem matriu per emmagatzemar les variables seleccionades
    
    for (j in 1:iters_boot) {
      print(paste("Escenari", i, "- Simulació", j))
      metrics[[j]] <- metriques2(vars_seleccionades_boot[[i]][j,], nv=num_vars2)
    }
    # Guardar resultats
    resultats_boot50 <- rbind(resultats_boot50, data.frame(
      N = N, pi = pi,
      Sensibilitat = mean(sapply(metrics, function(x) x["sensibilitat"]), na.rm = TRUE),
      Especificitat = mean(sapply(metrics, function(x) x["especificitat"]), na.rm = TRUE),
      VPP = mean(sapply(metrics, function(x) x["VPP"]), na.rm = TRUE),
      VPN = mean(sapply(metrics, function(x) x["VPN"]), na.rm = TRUE),
      Accuracy = mean(sapply(metrics, function(x) x["accuracy"]), na.rm = TRUE),
      NetBenefit = mean(sapply(metrics, function(x) x["net_benefit"]), na.rm = TRUE)
    ))
    
    
    
  }
})

print(resultats_boot50)
save(resultats_boot50, vars_seleccionades_boot, file = "boot50_def.RData")
