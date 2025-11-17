## Funzione di valutazione forecast (RMSE e MAE)
score_forecast <- function(y_true, y_hat){
  # allineo eventuali nomi mancanti
  colnames(y_hat) <- colnames(y_true)
  e <- y_true - y_hat
  MSFE  <- colMeans(e^2, na.rm = TRUE)     # Mean Squared Forecast Error
  RMSE  <- sqrt(MSFE)                      # Root Mean Squared Error
  MAE   <- colMeans(abs(e), na.rm = TRUE)  # Mean Absolute Error
  tibble(
    Serie = colnames(y_true),
    RMSE = RMSE,
    MSFE = MSFE,
    MAE = MAE,
    RMSE_media = mean(RMSE),
    MSFE_media = mean(MSFE),
    MAE_media = mean(MAE))}