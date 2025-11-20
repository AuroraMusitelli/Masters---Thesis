## Funzione per individuare i break strutturali  
analisi_break_strutturali <- function(ts_matrix, nome_blocco = "Serie") {
  
  dir.create("Breaks strutturali", showWarnings = FALSE)   # Crea cartella per salvare grafici
  
  risultati <- list()   # Lista che conterrà le info dettagliate
  tabella <- list()     # Lista che conterrà le righe della tabella finale
  
  for (serie in colnames(ts_matrix)) {                     # Loop su ogni serie
    y <- ts_matrix[, serie]                                # Estrae la singola serie
    if (all(is.na(y)) || sum(!is.na(y)) < 50) next         # Salta serie insufficienti
    
    df <- data.frame(y = as.numeric(y), t = seq_along(y))  # Crea df per breakpoints
    
    bp <- tryCatch(                                        # Stima breakpoints con robustezza a errori
      breakpoints(y ~ t, data = df, h = 0.15),
      error = function(e) NULL
    )
    if (is.null(bp)) next                                  # Se errore -> salta
    
    pos <- bp$breakpoints                                  # Estrae posizioni dei break
    
    if (!all(is.na(pos))) {                                # Se ci sono break validi
      
      date_ts  <- time(y)[pos]                             # Converte posizioni in date TS
      date_fmt <- zoo::as.yearmon(date_ts)                 # Converte in formato anno-mese
      
      # Salvataggio del grafico
      png(
        filename = file.path("Breaks strutturali", paste0("Breaks_", nome_blocco, "_", serie, ".png")),
        width = 1200, height = 800, res = 300
      )
      
      plot(y, main = paste("Break Strutturali -", nome_blocco, serie), # Grafico serie
           xlab = "Tempo", ylab = serie)
      abline(v = date_ts, col = "blue", lty = 2)                        # Linee verticali sui break
      
      dev.off()                                                         # Chiude il file PNG
      
      risultati[[serie]] <- list(                                       # Salva risultati grezzi
        breakpoints = pos,
        break_dates = date_fmt
      )
      
      tabella[[serie]] <- tibble::tibble(                               # Righe tabella finale
        Serie = serie,
        N_Breaks = length(pos),
        Break_Dates = paste(format(date_fmt, "%Y-%m"), collapse = ", ")
      )
    }
  }
  
  tabella_out <- if (length(tabella)) dplyr::bind_rows(tabella) else tibble::tibble()
  
  if (nrow(tabella_out)) {                                              # Stampa risultati in console
    cat("\n===== RISULTATI BREAK STRUTTURALI -", nome_blocco, "=====\n")
    print(tabella_out, n = Inf)
  }
  
  return(list(raw = risultati, summary = tabella_out))                  # Output finale
}
