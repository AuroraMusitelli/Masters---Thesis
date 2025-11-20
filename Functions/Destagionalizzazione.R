## Funzione: Destagionalizzazione X-13ARIMA-SEATS      
destag_tsibble <- function(tsib) {
  
  # Converto il tsibble in tibble per manipolarlo più facilmente
  tmp <- as_tibble(tsib)
  
  # Identifico tutte le colonne numeriche da destagionalizzare (elimino Month e Date_idx perché sono indici temporali)
  cols <- tmp %>%
    dplyr::select(-Month, -Date_idx) %>%
    names()
  
  message("Destagionalizzazione in corso per ", length(cols), " serie...")
  
  # Ciclo su ogni colonna/variabile da destagionalizzare
  for (col in cols) {
    cat("processo:", col, "\n")
    
    # Estraggo la singola serie e la porto in formato numerico
    serie <- tmp[[col]]
    serie <- as.numeric(serie)
    
    # X-13 non accetta valori mancanti, quindi applico una interpolazione lineare minima solo dove necessario
    if (anyNA(serie)) {
      serie <- na.approx(serie, na.rm = FALSE)}
    
    # Costruisco un oggetto ts mensile con anno e mese iniziali
    ts_obj <- ts(
      serie,
      start = c(year(tmp$Month[1]), month(tmp$Month[1])),
      freq = 12)
    
    # Applico il filtro X-13 ARIMA-SEATS con gestione errori
    tryCatch({
      
      # Stima completa X-13
      fit <- seas(ts_obj)
      
      # Estraggo la serie destagionalizzata (componente S11)
      sa <- fit$series$s11
      
      # Sostituisco la colonna originale con quella destagionalizzata
      tmp[[col]] <- as.numeric(sa)
      
    }, error = function(e) {
      # Alcune serie possono non avere stagionalità (troppo corte, costanti, ecc.)
      warning("Errore con la serie ", col, 
              ": la lascio originale (nessuna destagionalizzazione).")
    })
  }
  
  # Ricostruisco il tsibble mantenendo Date_idx come indice temporale
  tsib_out <- tmp %>%
    as_tsibble(index = Date_idx)
  
  return(tsib_out)
}

