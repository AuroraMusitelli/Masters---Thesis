#### FUNZIONE: Destagionalizzazione X-13 per tsibble      
destag_tsibble <- function(tsib) {
  
  tmp <- as_tibble(tsib)
  cols <- tmp %>%
    dplyr::select(-Month, -Date_idx) %>%
    names()
  
  message("Destagionalizzazione in corso per ", length(cols), " serie...")
  
  for (col in cols) {
    cat("processo:", col, "\n")
    
    serie <- tmp[[col]]
    serie <- as.numeric(serie)
    
    # interpolazione minima per NA (X-13 non li accetta)
    if (anyNA(serie)) {
      serie <- na.approx(serie, na.rm = FALSE)
    }
    
    # costruisco la serie ts mensile
    ts_obj <- ts(serie, start = c(year(tmp$Month[1]), month(tmp$Month[1])), freq = 12)
    
    # applico destagionalizzazione X-13 ARIMA-SEATS
    tryCatch({
      
      fit <- seas(ts_obj)
      sa <- fit$series$s11   # serie destagionalizzata
      
      tmp[[col]] <- as.numeric(sa)
      
    }, error = function(e) {
      warning("Errore con la serie ", col, ": la lascio originale (nessuna destagionalizzazione).")
    })
  }
  
  # ricostruisco tsibble
  tsib_out <- tmp %>%
    as_tsibble(index = Date_idx)
  
  return(tsib_out)
}

