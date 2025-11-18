## Funzione per stimare un modello VAR con rolling expanding window 
run_VAR_rolling <- function(Y_mat, T1, max_p = 12, type = "const") {
  
  n <- nrow(Y_mat)    # numero di osservazioni
  k <- ncol(Y_mat)    # numero di serie (dimensione VAR)
  results <- list()
  
  for (p in 1:max_p) {
    message("Stimo VAR con p = ", p)
    preds <- matrix(NA, n - T1, k)     # memorizza le previsioni 1-step-ahead
    true_vals <- matrix(NA, n - T1, k)  # memorizza i valori osservati corrispondenti
    colnames(preds) <- colnames(true_vals) <- colnames(Y_mat)
    
    # Rolling expanding window
    for (i in 1:(n - T1)) {
      Y_train <- Y_mat[1:(T1 + i - 1), ]
      VAR_fit <- try(VAR(Y_train, p = p, type = type), silent = TRUE)
      
      if (inherits(VAR_fit, "try-error")) {
        preds[i, ] <- NA
      } else {
        fcst <- predict(VAR_fit, n.ahead = 1)$fcst
        preds[i, ] <- sapply(fcst, function(x) x[1])
      }
      
      true_vals[i, ] <- Y_mat[T1 + i, ]
    }
    
    # Score forecast
    sc <- score_forecast(true_vals, preds)
    sc$p <- p
    
    # Stabilità: controllo stabilità del VAR
    VAR_full <- try(VAR(Y_mat, p = p, type = type), silent = TRUE)
    if (!inherits(VAR_full, "try-error")) {
      sc$max_root <- max(abs(roots(VAR_full)))
    } else {
      sc$max_root <- NA
    }
    
    results[[paste0("p_", p)]] <- sc
  }
  # Combino tutti i risultati
  final <- do.call(rbind, results)
  rownames(final) <- NULL
  
  return(final)
}


