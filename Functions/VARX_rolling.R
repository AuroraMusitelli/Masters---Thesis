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
    
    # Rolling / expanding sulla porzione di CV (T1+1 : T2)
    for (t in (T1 + 1):T2) {
      idx_train <- 1:(t - 1)
      
      Y_train <- Y_mat[idx_train, , drop = FALSE]
      
      # Esogene di training: X_exog è ***globale*** (serve a predict/all'oggetto VAR)
      X_exog <<- X_mat[idx_train, , drop = FALSE]
      
      # Stima VARX; se fallisce, salta quell’iterazione
      VARX_t <- try(
        VAR(
          y      = Y_train,
          p      = p,
          type   = "const",
          exogen = X_exog
        ),
        silent = TRUE
      )
      
      if (inherits(VARX_t, "try-error")) {
        next
      }
      
      # Esogene al tempo t per previsione (1-step-ahead)
      X_fore <- matrix(X_mat[t, , drop = FALSE], nrow = 1)
      colnames(X_fore) <- colnames(X_exog)
      
      fc <- try(
        predict(VARX_t, n.ahead = 1, dumvar = X_fore)$fcst,
        silent = TRUE
      )
      
      if (inherits(fc, "try-error")) {
        next
      }
      
      preds_cv[t - T1, ] <- sapply(fc, function(x) x[1])
    }
    
    # ---- DEBUG MINIMO: quante previsioni NON-NA abbiamo per questo p? ----
    cat("p =", p,
        "  quota non-NA in preds_cv:",
        mean(!is.na(preds_cv)),
        "\n")
    # ----------------------------------------------------------------------
    # Calcolo RMSE / MSFE / MAE per OGNI SERIE, ignorando gli NA (colonna per colonna)
    RMSE <- MSFE <- MAE <- rep(NA_real_, ncol(Y_mat))
    
    for (j in seq_len(ncol(Y_mat))) {
      yj_true  <- true_cv[, j]
      yj_hat   <- preds_cv[, j]
      ok_idx   <- which(!is.na(yj_true) & !is.na(yj_hat))
      
      if (length(ok_idx) == 0) {
        RMSE[j] <- NaN
        MSFE[j] <- NaN
        MAE[j]  <- NaN
      } else {
        err      <- yj_hat[ok_idx] - yj_true[ok_idx]
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



