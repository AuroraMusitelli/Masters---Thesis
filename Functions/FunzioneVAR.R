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
  n <- nrow(Y_mat)
  k <- ncol(Y_mat)
  results <- vector("list", max_p)
  
  # valori veri fuori campione (T1+1,...,n)
  true_vals <- Y_mat[(T1 + 1):n, , drop = FALSE]
  colnames_true <- colnames(Y_mat)
  
  for (p in 1:max_p) {
    message("Lag p = ", p)
    preds <- matrix(NA, n - T1, k)
    colnames(preds) <- colnames_true
    
    # rolling–expanding
    for (i in 1:(n - T1)) {
      Y_train <- Y_mat[1:(T1 + i - 1), , drop = FALSE]
      
      VAR_fit <- try(VAR(Y_train, p = p, type = type), silent = TRUE)
      if (!inherits(VAR_fit, "try-error")) {
        fcst <- predict(VAR_fit, n.ahead = 1)$fcst
        preds[i, ] <- vapply(fcst, function(x) x[1], numeric(1))}
    }
    
    # score forecast per questo p
    sc <- score_forecast(true_vals, preds)
    sc$p <- p
    
    # stability (radice massima)
    VAR_full <- try(VAR(Y_mat, p = p, type = type), silent = TRUE)
    sc$max_root <- if (!inherits(VAR_full, "try-error")) {
      max(abs(roots(VAR_full)))
    } else {
      NA_real_
    }
    
    results[[p]] <- sc
  }
  
  final <- do.call(rbind, results)
  rownames(final) <- NULL
  final
}


