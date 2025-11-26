## Funzione di valutazione forecast (RMSE e MAE)
score_forecast <- function(true, pred) {
  
  # ---- Calcolo errori base ----
  errors <- true - pred
  
  RMSE  <- apply(errors, 2, function(e) sqrt(mean(e^2, na.rm = TRUE)))
  MSFE  <- apply(errors, 2, function(e) mean(e^2, na.rm = TRUE))
  MAE   <- apply(errors, 2, function(e) mean(abs(e), na.rm = TRUE))
  
  # ---- Deviazione standard della serie vera ----
  sd_y  <- apply(true, 2, sd)
  
  # ---- RMSE relativo ----
  RMSE_rel <- RMSE / sd_y
  
  # ---- Output in dataframe ordinato ----
  result <- data.frame(
    Serie = colnames(true),
    RMSE = RMSE,
    MSFE = MSFE,
    MAE = MAE,
    RMSE_rel = RMSE_rel
  )
  
  return(result)
}
