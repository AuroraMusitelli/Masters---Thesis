## ----- 5.1 Funzione di analisi serie storiche (ADF, Ljung-Box, ACF, PACF) -----
analisi_serie_ts <- function(ts_data, max_lag = 36, ljung_lags = c(1,2,3,6,12,18,24,36)){
  if(!tsibble::is_tsibble(ts_data)) stop("Il dataset deve essere un oggetto tsibble.")
  
  serie_numeriche <- ts_data[, sapply(ts_data, is.numeric), drop=FALSE]
  plots_acf <- list(); plots_pacf <- list(); ljung_results <- list(); adf_results <- list()
  # Creo cartella per salvare i grafici 
  dir.create("ACF_PACF_Plots", showWarnings = FALSE)
  
  # Ciclo su ogni serie numerica
  for(nome_serie in colnames(serie_numeriche)){
    serie <- na.omit(as.numeric(serie_numeriche[[nome_serie]]))
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
    adf_results[[nome_serie]] <- list()
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


## ----- 5.2 Sintesi risultati ADF / Ljung-Box -----
sintesi_risultati_ts <- function(risultati, nome_blocco="Serie"){
  serie_nomi <- names(risultati$ADF)   # Estraggo i nomi delle serie analizzate
  
  # Creo una tabella riepilogativa per ogni serie
  output <- lapply(serie_nomi, function(serie){
    adf_drift <- risultati$ADF[[serie]][["drift"]]
    adf_trend <- risultati$ADF[[serie]][["trend"]]
    ljung <- risultati$Ljung_Box[[serie]]
    p_value_lag1 <- ljung$P_Value[ljung$Lag==1]
    
    tibble(
      Serie = serie,
      ADF_Drift_Stat = round(adf_drift$Test_Statistic,3),
      ADF_Drift_Crit_5pct = round(adf_drift$Critical_5pct,3),
      Stationary_Drift = adf_drift$Stationary,
      ADF_Trend_Stat = round(adf_trend$Test_Statistic,3),
      ADF_Trend_Crit_5pct = round(adf_trend$Critical_5pct,3),
      Stationary_Trend = adf_trend$Stationary,
      LjungBox_pvalue_lag1 = round(p_value_lag1,4),
      LjungBox_AnySignificant = any(ljung$P_Value<0.05))}) %>% bind_rows()
  # Sintesi dei risultati
  cat("\n========= RISULTATI SINTETICI:", nome_blocco, "=========\n")
  print(output)
  return(output)}


## ----- 5.3 Trasformazione (differenziazione/detrend) -----
trasforma_serie <- function(df, tabella, ts_ref){
  decisioni <- tabella %>%
    dplyr::mutate(
      Regola = dplyr::case_when(
        Stationary_Drift==FALSE & Stationary_Trend==FALSE ~ "diff",     # se la serie non stazionaria con drift O con trend = differenziazione 
        Stationary_Drift==FALSE & Stationary_Trend==TRUE  ~ "detrend",  # trend deterministico = detrendizzazione
        Stationary_Drift==TRUE  & Stationary_Trend==TRUE  ~ "none",     # se stazionaria = nessuna trasformazione
        TRUE ~ "none"))
  df_out <- df
  trasformazioni <- list()
  # Applico le trasformazioni decise nella funzione per ogni serie storica
  for(serie in decisioni$Serie){
    if(!(serie %in% colnames(df))) next
    x <- df[[serie]]
    regola <- decisioni$Regola[match(serie, decisioni$Serie)]
    applied <- list(Differenziazione=FALSE, Detrend=FALSE)
    # Differenziazione
    if(regola=="diff"){
      x <- c(x[1], diff(x))
      applied$Differenziazione <- TRUE}
    # Detrend tramite regressione lineare
    if(regola=="detrend"){
      time_idx <- seq_along(x)
      fit <- lm(x ~ time_idx)
      x <- residuals(fit)
      applied$Detrend <- TRUE}
    # Aggiorno i dati con la serie trasformate
    df_out[[serie]] <- x
    trasformazioni[[serie]] <- applied}
  # Ricreo l'oggetto ts con le stesse specifiche temporali della serie originale
  ts_out <- ts(df_out, start=start(ts_ref), frequency=frequency(ts_ref))
  
  # Creo tabella riepilogativa delle trasformazioni
  tabella_trasf <- tibble::tibble(
    Serie = names(trasformazioni),
    Differenziazione = sapply(trasformazioni, function(x) x$Differenziazione),
    Detrend = sapply(trasformazioni, function(x) x$Detrend))
  return(list(data=df_out, ts=ts_out, tabella=tabella_trasf))}


## ----- 5.4 Sintesi finale post-trasformazioni -----
sintesi_stazionarieta_finale <- function(risultati_final, nome_blocco="Serie"){
  serie_nomi <- names(risultati_final$ADF)
  # Costruisco tabella di stazionarietà finale
  output <- lapply(serie_nomi, function(serie){
    adf_drift <- risultati_final$ADF[[serie]][["drift"]]
    adf_trend <- risultati_final$ADF[[serie]][["trend"]]
    
    tibble(
      Serie = serie,
      Stationary_Drift = adf_drift$Stationary,
      Stationary_Trend = adf_trend$Stationary,
      Stazionaria_Finalmente = adf_drift$Stationary & adf_trend$Stationary)}) %>% bind_rows()
  cat("\n--- Sintesi Stazionarietà Finale:", nome_blocco, "---\n")
  print(output)
  return(output)}


## ----- 5.5 Report finale: trasformazioni + stazionarietà -----
report_finale <- function(tabella_trasf, tabella_finale, nome_blocco="Serie"){
  # Unisco risultati di trasformazioni e stazionarietà finale
  report <- dplyr::left_join(tabella_trasf, tabella_finale, by="Serie")
  cat("\n=============== REPORT FINALE:", nome_blocco, "===============\n")
  print(report, n=Inf)
  
  # Evidenzio le serie che NON sono ancora stazionarie
  non_staz <- report %>% filter(Stazionaria_Finalmente == FALSE)
  if(nrow(non_staz)>0){
    cat("\n*** Serie NON stazionarie dopo le trasformazioni:", nome_blocco, "***\n")
    print(non_staz, n=Inf)}
  return(report)}


