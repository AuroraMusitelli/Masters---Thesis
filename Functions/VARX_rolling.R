## Funzione per rolling–expanding cross-validation del lag p in un VARX 
run_VARX_rolling <- function(Y_mat, X_mat, T1, max_p = 12){
  
  T_tot <- nrow(Y_mat)
  T2    <- floor(0.9 * T_tot)    # fine CV / inizio test, come per gli altri modelli
  
  out_list <- list()
  
  for (p in 1:max_p) {
    
    n_cv      <- T2 - T1
    preds_cv  <- matrix(NA, n_cv, ncol(Y_mat))
    true_cv   <- Y_mat[(T1 + 1):T2, , drop = FALSE]
    colnames(preds_cv) <- colnames(true_cv) <- colnames(Y_mat)
    
    for (t in (T1 + 1):T2) {
      idx_train <- 1:(t - 1)
      
      Y_train <- Y_mat[idx_train, , drop = FALSE]
      X_exog <<- X_mat[idx_train, , drop = FALSE]
      
      VARX_t <- VAR(
        y      = Y_train,
        p      = p,
        type   = "const",
        exogen = X_exog
      )
      
      # esogene al tempo t (1-step-ahead)
      X_fore <- matrix(X_mat[t, , drop = FALSE], nrow = 1)
      fc     <- predict(VARX_t, n.ahead = 1, dumvar = X_fore)$fcst
      
      preds_cv[t - T1, ] <- sapply(fc, function(x) x[1])
    }
    
    sc_p   <- score_forecast(true_cv, preds_cv)
    sc_p$p <- p
    
    out_list[[p]] <- sc_p
  }
  
  dplyr::bind_rows(out_list)
}



## Funzione per RMSE relativo 
add_rmse_rel <- function(df_scores, y_true){
  sd_test <- apply(y_true, 2, sd)
  df_scores$RMSE_rel <- df_scores$RMSE / sd_test[df_scores$Serie]
  return(df_scores)
}






