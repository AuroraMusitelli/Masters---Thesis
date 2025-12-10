## Funzione per calcolare VARX
run_VARX_rolling <- function(Y_mat, X_mat, T1, T2, max_p = 12){
  
  T_tot <- nrow(Y_mat)
  stopifnot(nrow(X_mat) == T_tot)
  
  out_list <- list()
  
  for (p in 1:max_p) {
    
    n_cv      <- T2 - T1
    preds_cv  <- matrix(NA, n_cv, ncol(Y_mat))
    true_cv   <- Y_mat[(T1 + 1):T2, , drop = FALSE]
    colnames(preds_cv) <- colnames(true_cv) <- colnames(Y_mat)
    
    for (t in (T1 + 1):T2) {
      idx_train <- 1:(t - 1)
      
      Y_train <- Y_mat[idx_train, , drop = FALSE]
      X_train <- X_mat[idx_train, , drop = FALSE]
      
      # Stima VARX sulla finestra expanding
      VARX_t <- try(
        VAR(
          y      = Y_train,
          p      = p,
          type   = "const",
          exogen = X_train
        ),
        silent = TRUE
      )
      if (inherits(VARX_t, "try-error")) next
      
      # Previsione 1-step per il tempo t
      y_hat_t <- tryCatch(
        predict_VARX_one_step(VARX_t, Y_full = Y_mat, X_full = X_mat, t = t),
        error = function(e) rep(NA_real_, ncol(Y_mat))
      )
      
      preds_cv[t - T1, ] <- y_hat_t
    }
    
    # Calcolo RMSE / MSFE / MAE serie per serie, ignorando gli NA
    RMSE <- MSFE <- MAE <- rep(NA_real_, ncol(Y_mat))
    
    for (j in seq_len(ncol(Y_mat))) {
      y_true_j <- true_cv[, j]
      y_hat_j  <- preds_cv[, j]
      ok       <- which(!is.na(y_true_j) & !is.na(y_hat_j))
      
      if (length(ok) == 0) {
        RMSE[j] <- NaN
        MSFE[j] <- NaN
        MAE[j]  <- NaN
      } else {
        err      <- y_hat_j[ok] - y_true_j[ok]
        MSFE[j] <- mean(err^2)
        RMSE[j] <- sqrt(MSFE[j])
        MAE[j]  <- mean(abs(err))
      }
    }
    
    df_p <- tibble::tibble(
      Serie = colnames(Y_mat),
      RMSE  = RMSE,
      MSFE  = MSFE,
      MAE   = MAE,
      p     = p
    )
    
    out_list[[p]] <- df_p
  }
  
  dplyr::bind_rows(out_list)
}


