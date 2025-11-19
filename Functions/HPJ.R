# Pre-processing con filtro HPJ: serie cicliche stazionarie
# Funzione: prende un tsibble con Month, Date_idx + colonne numeriche e restituisce SOLO la componente ciclica (y - trend HPJ)
make_hpj_cycle <- function(tsib) {
  tmp <- as_tibble(tsib)
  
  # Colonne da filtrare (tutte tranne Month e Date_idx)
  cols_to_filter <- tmp %>%
    dplyr::select(-Month, -Date_idx) %>% names()
  
  # Applico HPJ colonna per colonna e tengo il ciclo (= y - trend)
  tmp[cols_to_filter] <- lapply(tmp[cols_to_filter], function(x) {
    v <- as.numeric(x)
    
    # Se tutta NA lascio cosi (evito crash) 
    if (all(is.na(v))) return(v)
    # Se ci sono NA, interpolazione lineare semplice
    if (anyNA(v)) {
      v <- zoo::na.approx(v, na.rm = FALSE)
    }
    # Filtro HPJ: di default stima automaticamente lambda e la penalit
    fit <- hpj(v)
    
    # Trend HPJ (con salti)
    trend_hpj <- as.numeric(fit$hpj)
    
    # Componente ciclica: serie - trend
    cycle <- v - trend_hpj
    
    # Per sicurezza: dove v era NA, lascio NA anche nel ciclo
    cycle[is.na(v)] <- NA
    return(cycle)
  })
  # Ricostruisco tsibble con Date_idx come indice
  tsib_out <- tmp %>%
    as_tsibble(index = Date_idx)
  return(tsib_out)
}



