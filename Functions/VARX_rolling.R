## Funzione per rolling–expanding cross-validation del lag p in un VARX 
run_VARX_rolling <- function(Y_mat, X_mat, T1, T2, max_p = 12){
  
  T_tot <- nrow(Y_mat)
  stopifnot(nrow(X_mat) == T_tot)
  
  out_list <- list()
  
  for (p in 1:max_p) {
    
    n_cv      <- T2 - T1
    preds_cv  <- matrix(NA, n_cv, ncol(Y_mat))
    true_cv   <- Y_mat[(T1 + 1):T2, , drop = FALSE]
    colnames(preds_cv) <- colnames(true_cv) <- colnames(Y_mat)
    
    # Rolling / expanding nella porzione di CV (T1+1 : T2)
    for (t in (T1 + 1):T2) {
      idx_train <- 1:(t - 1)
      
      Y_train <- Y_mat[idx_train, , drop = FALSE]
      
      # esogene di training: le salvo in un oggetto globale X_exog,
      # così la call di VAR/predict le trova sempre
      X_exog <<- X_mat[idx_train, , drop = FALSE]
      
      VARX_t <- VAR(
        y      = Y_train,
        p      = p,
        type   = "const",
        exogen = X_exog
      )
      
      # esogene al tempo t per la previsione 1-step-ahead
      X_fore <- matrix(X_mat[t, , drop = FALSE], nrow = 1)
      colnames(X_fore) <- colnames(X_exog)
      
      fc <- predict(VARX_t, n.ahead = 1, dumvar = X_fore)$fcst
      
      preds_cv[t - T1, ] <- sapply(fc, function(x) x[1])
    }
    
    # Calcolo errori solo sulle righe senza NA
    valid_idx <- which(
      rowSums(is.na(preds_cv)) == 0 &
        rowSums(is.na(true_cv))  == 0
    )
    
    if (length(valid_idx) == 0) {
      # nessuna osservazione valida: metto NaN
      RMSE <- rep(NaN, ncol(Y_mat))
      MSFE <- rep(NaN, ncol(Y_mat))
      MAE  <- rep(NaN, ncol(Y_mat))
    } else {
      err  <- preds_cv[valid_idx, , drop = FALSE] - true_cv[valid_idx, , drop = FALSE]
      MSFE <- colMeans(err^2)
      RMSE <- sqrt(MSFE)
      MAE  <- colMeans(abs(err))
    }
    
    df_p <- tibble::tibble(
      Serie = colnames(Y_mat),
      RMSE  = as.numeric(RMSE),
      MSFE  = as.numeric(MSFE),
      MAE   = as.numeric(MAE),
      p     = p
    )
    
    out_list[[p]] <- df_p
  }
  
  dplyr::bind_rows(out_list)
}




