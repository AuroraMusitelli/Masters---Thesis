## Funzione di valutazione forecast (RMSE e MAE)
score_forecast_complete <- function(true, pred) {
  
  # ---- Calcolo errori base ----
  errors <- true - pred
  
  RMSE  <- apply(errors, 2, function(e) sqrt(mean(e^2, na.rm = TRUE)))
  MSFE  <- apply(errors, 2, function(e) mean(e^2, na.rm = TRUE))
  MAE   <- apply(errors, 2, function(e) mean(abs(e), na.rm = TRUE))
  
  # ---- Deviazione standard della serie vera ----
  sd_y  <- apply(true, 2, sd)
  
  # ---- RMSE relativo ----
  RMSE_rel <- RMSE / sd_y
  
  # ---- Funzione di interpretazione ----
  interpret_rmse_rel <- function(r){
    if (r < 0.10) return("Eccellente")
    if (r < 0.20) return("Ottimo")
    if (r < 0.30) return("Buono")
    if (r < 0.50) return("Accettabile")
    return("Scarso")
  }
  
  Interpretazione <- sapply(RMSE_rel, interpret_rmse_rel)
  
  # ---- Output in dataframe ordinato ----
  result <- data.frame(
    Serie = colnames(true),
    RMSE = RMSE,
    MSFE = MSFE,
    MAE = MAE,
    RMSE_rel = RMSE_rel,
    Interpretazione = Interpretazione
  )
  
  return(result)
}
