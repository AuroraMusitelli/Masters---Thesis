## Funzione di valutazione forecast (RMSE e MAE)
score_forecast <- function(y_true, y_hat) {
  
  stopifnot(all(dim(y_true) == dim(y_hat)))
  
  cols <- colnames(y_true)
  results <- data.frame(
    Serie = cols,
    RMSE = NA,
    MAE  = NA,
    MAPE = NA,
    RMSE_rel = NA
  )
  
  for (j in seq_along(cols)) {
    yt <- y_true[, j]
    yh <- y_hat[, j]
    
    rmse <- sqrt(mean((yt - yh)^2, na.rm = TRUE))
    mae  <- mean(abs(yt - yh), na.rm = TRUE)
    
    # Per MAPE evito divisioni per 0
    mape <- mean(abs((yt - yh) / ifelse(yt == 0, NA, yt)), na.rm = TRUE) * 100
    
    # RMSE relativo = RMSE / sd della serie
    rmse_rel <- rmse / sd(yt, na.rm = TRUE)
    
    results[j, "RMSE"]     <- rmse
    results[j, "MAE"]      <- mae
    results[j, "MAPE"]     <- mape
    results[j, "RMSE_rel"] <- rmse_rel
  }
  
  return(results)
}
