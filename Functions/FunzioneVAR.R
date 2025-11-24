## Funzione per stimare un modello VAR con rolling expanding window   

##   Y_mat : matrice delle serie endogene (T x k)
##   T1    : indice che separa training (1:T1) e previsione/CV
##   max_p : numero massimo di lag da considerare
##
## La funzione:
##   1) per ogni lag p = 1,...,max_p stima un VAR in modo ricorsivo usando una rolling expanding window
##   2) genera previsioni one-step-ahead fuori campione
##   3) calcola gli errori di previsione (RMSE, MAE)
##   4) verifica la stability del VAR (moduli delle radici)
##   5) restituisce un dataframe con le metriche per ogni p

run_VAR_rolling <- function(Y_mat, T1, max_p = 12, type = "const") {
  
  n <- nrow(Y_mat)    # numero di osservazioni totali
  k <- ncol(Y_mat)    # numero di variabili endogene (dimensione del VAR)
  results <- list()   # lista vuota dove salvare i risultati per ogni lag p
  
  # Loop sui valori del lag p da testare
  for (p in 1:max_p) {
    message("Stimo VAR con p = ", p)
    # Matrici per salvare previsioni e valori veri
    preds <- matrix(NA, n - T1, k)      # memorizza le previsioni 1-step-ahead
    true_vals <- matrix(NA, n - T1, k)  # memorizza i valori osservati corrispondenti
    colnames(preds) <- colnames(true_vals) <- colnames(Y_mat)
    
    # Rolling expanding window
    ## Per i = 1,...,(n - T1) si amplia progressivamente l'insieme di training e si genera una previsione 1-step per ogni punto
    for (i in 1:(n - T1)) {
      Y_train <- Y_mat[1:(T1 + i - 1), ]
      VAR_fit <- try(VAR(Y_train, p = p, type = type), silent = TRUE)    # Stima del VAR su training corrente
      # Se la stima da errore (es. modello singolare), assegno NA
      if (inherits(VAR_fit, "try-error")) {
        preds[i, ] <- NA
      } else {
        fcst <- predict(VAR_fit, n.ahead = 1)$fcst
        preds[i, ] <- sapply(fcst, function(x) x[1])
      }
      # Salvo il valore reale al tempo T1+i
      true_vals[i, ] <- Y_mat[T1 + i, ]
    }
    
    # Score forecast RMSE, MAE e altre metriche 
    sc <- score_forecast(true_vals, preds) 
    sc$p <- p    # aggiungo informazione sul lag usato
    
    ## Test di stability del VAR (modulo massimo delle radici): stimo il VAR sull'intero dataset per verificare se stabile
    VAR_full <- try(VAR(Y_mat, p = p, type = type), silent = TRUE)
    if (!inherits(VAR_full, "try-error")) {
      sc$max_root <- max(abs(roots(VAR_full)))    # radice caratteristica piÃ¹ grande
    } else {
      sc$max_root <- NA
    }
    # Salvo il risultato per questo valore di p
    results[[paste0("p_", p)]] <- sc
  }
  # Combino tutti i risultati per tutti i lag p in un unico dataframe
  final <- do.call(rbind, results)
  rownames(final) <- NULL
  
  return(final)
}


