# ---- Funzione per individuare i break strutturali 
analisi_break_strutturali <- function(ts_matrix, nome_blocco = "Serie") {
  # Creo la cartella per salvare i grafici
  dir.create("Breaks strutturali", showWarnings = FALSE)
  
  risultati <- list()
  tabella <- list()
  
  for (serie in colnames(ts_matrix)) {
    y <- ts_matrix[, serie]
    if (all(is.na(y)) || sum(!is.na(y)) < 50) next
    df <- data.frame(y = as.numeric(y), t = seq_along(y))
    
    bp <- tryCatch(breakpoints(y ~ t, data = df, h = 0.15), error = function(e) NULL)
    if (is.null(bp)) next
    
    pos <- bp$breakpoints
    if (!all(is.na(pos))) {
      date_ts  <- time(y)[pos]
      date_fmt <- zoo::as.yearmon(date_ts)
      
      # SALVATAGGIO NELLA CARTELLA "Breaks strutturali"
      png(filename = file.path("Breaks strutturali", paste0("Breaks_", nome_blocco, "_", serie, ".png")),
          width = 1200, height = 800, res = 300)
      
      plot(y, main = paste("Break Strutturali -", nome_blocco, serie),
           xlab = "Tempo", ylab = serie)
      abline(v = date_ts, col = "blue", lty = 2)
      dev.off()
      
      risultati[[serie]] <- list(breakpoints = pos, break_dates = date_fmt)
      tabella[[serie]] <- tibble::tibble(
        Serie = serie,
        N_Breaks = length(pos),
        Break_Dates = paste(format(date_fmt, "%Y-%m"), collapse = ", "))}}
  
  tabella_out <- if (length(tabella)) dplyr::bind_rows(tabella) else tibble::tibble()
  
  if (nrow(tabella_out)) {
    cat("\n===== RISULTATI BREAK STRUTTURALI -", nome_blocco, "=====\n")
    print(tabella_out, n = Inf)}
  return(list(raw = risultati, summary = tabella_out))
}