## Funzione di analisi serie storiche (ADF, Ljung-Box, ACF, PACF) -----
analisi_serie_ts <- function(ts_data, max_lag = 36, ljung_lags = c(1,2,3,6,12,18,24,36)){
  if(!tsibble::is_tsibble(ts_data)) stop("Il dataset deve essere un oggetto tsibble.")   # Controllo che l'input sia una tsibble
  # Seleziono solo le colonne numeriche del dataset
  serie_numeriche <- ts_data[, sapply(ts_data, is.numeric), drop=FALSE]
  plots_acf <- list(); plots_pacf <- list(); ljung_results <- list(); adf_results <- list()
  # Creo cartella per salvare i grafici 
  dir.create("ACF_PACF_Plots", showWarnings = FALSE)
  
  # Ciclo su ogni serie numerica
  for(nome_serie in colnames(serie_numeriche)){
    serie <- na.omit(as.numeric(serie_numeriche[[nome_serie]]))     # Estraggo la serie come vettore numerico e rimuovo eventuali NA
    if(length(serie) < 10) next  
    ts_serie <- ts(serie, frequency = 12)
    
    # Calcolo grafici ACF e PACF
    plots_acf[[nome_serie]] <- forecast::ggAcf(ts_serie, lag.max = max_lag) + ggtitle(paste("ACF -", nome_serie))
    plots_pacf[[nome_serie]] <- forecast::ggPacf(ts_serie, lag.max = max_lag) + ggtitle(paste("PACF -", nome_serie))
    
    # Salvo i grafici nella cartella creata
    ggsave(filename = file.path("ACF_PACF_Plots", paste0("ACF_", nome_serie, ".png")),
           plot = plots_acf[[nome_serie]], width = 7, height = 5, dpi = 300)
    ggsave(filename = file.path("ACF_PACF_Plots", paste0("PACF_", nome_serie, ".png")),
           plot = plots_pacf[[nome_serie]], width = 7, height = 5, dpi = 300)
    
    # Test di autocorrelazione (Ljung-Box)
    lb_pvalues <- sapply(ljung_lags, function(lg) 
      tryCatch(Box.test(ts_serie, lag = lg, type="Ljung-Box")$p.value, error=function(e) NA))
    ljung_results[[nome_serie]] <- data.frame(Lag = ljung_lags, P_Value = round(lb_pvalues,4))
    
    # Test ADF (drift e trend)
    adf_results[[nome_serie]] <- list()    # Inizializzo lista per i risultati ADF della serie corrente
    for(tipo in c("drift","trend")){
      adf_test <- tryCatch(urca::ur.df(ts_serie, type=tipo, selectlags="AIC"), error=function(e) NULL)
      if(!is.null(adf_test)){
        adf_stat <- adf_test@teststat[1]
        adf_crit <- adf_test@cval[1,"5pct"]
        adf_results[[nome_serie]][[tipo]] <- data.frame(
          Test_Statistic=adf_stat,
          Critical_5pct=adf_crit,
          Stationary=adf_stat<adf_crit)
      } else {
        adf_results[[nome_serie]][[tipo]] <- data.frame(Test_Statistic=NA,Critical_5pct=NA,Stationary=NA)
      }
    }
  }
  return(list(ACF=plots_acf, PACF=plots_pacf, Ljung_Box=ljung_results, ADF=adf_results))
}



## Sintesi risultati ADF / Ljung-Box -----
sintesi_risultati_ts <- function(risultati, nome_blocco="Serie"){
  serie_nomi <- names(risultati$ADF)   # Estraggo i nomi delle serie analizzate
  
  # Creo una tabella riepilogativa per ogni serie
  output <- lapply(serie_nomi, function(serie){
    # Risultati ADF con drift e con trend per la serie corrente
    adf_drift <- risultati$ADF[[serie]][["drift"]]
    adf_trend <- risultati$ADF[[serie]][["trend"]]
    ljung <- risultati$Ljung_Box[[serie]]
    p_value_lag1 <- ljung$P_Value[ljung$Lag==1]
    
    tibble(
      Serie = serie,
      ADF_Drift_Stat = round(adf_drift$Test_Statistic,3),     # Statistica ADF (drift)
      ADF_Drift_Crit_5pct = round(adf_drift$Critical_5pct,3),  # Valore critico 5% (drift)
      Stationary_Drift = adf_drift$Stationary,    # TRUE/FALSE stazionarietà (drift)
      ADF_Trend_Stat = round(adf_trend$Test_Statistic,3),   # Statistica ADF (trend)
      ADF_Trend_Crit_5pct = round(adf_trend$Critical_5pct,3),   # Valore critico 5% (trend)
      Stationary_Trend = adf_trend$Stationary,    # TRUE/FALSE stazionarietà (trend)
      LjungBox_pvalue_lag1 = round(p_value_lag1,4),   # p-value Ljung-Box al lag 1
      LjungBox_AnySignificant = any(ljung$P_Value<0.05))}) %>% # TRUE se almeno un lag significativo  
    bind_rows()    # Unisco tutte le righe in un'unica tabella
  # Sintesi dei risultati
  cat("\n========= RISULTATI SINTETICI:", nome_blocco, "=========\n")
  print(output)
  return(output)}




