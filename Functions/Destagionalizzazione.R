## ----- 6.1 Funzione per verificare stagionalità delle serie storiche -----
ha_stagionalita <- function(ts_data, soglia = 0.3) {
  acf_vals <- acf(ts_data, lag.max = 24, plot = FALSE)$acf
  return(abs(acf_vals[13]) > soglia)}  # lag 12 per dati mensili


## ----- 6.2 Funzione per destagionalizzazione con dummy mensili -----
destag_dummies <- function(ts_data) {
  if (!is.ts(ts_data)) stop("Input deve essere un oggetto ts")
  freq <- frequency(ts_data)
  month_factor <- factor(cycle(ts_data))
  fit <- lm(as.numeric(ts_data) ~ month_factor)
  residui <- resid(fit)
  return(ts(residui, start = start(ts_data), frequency = freq))}


## ----- 6.3 Funzione per applicare destagionalizzazione per ogni serie -----
applica_destag_dummies <- function(ts_matrix) {
  destag_list <- list()
  info_list <- list()
  
  for (serie in colnames(ts_matrix)) {
    ts_col <- ts_matrix[, serie]
    if (all(is.na(ts_col))) next
    
    stagionale <- ha_stagionalita(ts_col)
    if (stagionale) {
      ts_destag <- destag_dummies(ts_col)
      destag_list[[serie]] <- ts_destag
      info_list[[serie]] <- TRUE
    } else {
      destag_list[[serie]] <- ts_col
      info_list[[serie]] <- FALSE
    }
  }
  destag_matrix <- do.call(cbind, destag_list)
  destag_matrix <- ts(destag_matrix, start = start(ts_matrix), frequency = frequency(ts_matrix))
  
  tabella_info <- tibble(
    Serie = names(info_list),
    Stagionale = unlist(info_list)
  )
  return(list(data = destag_matrix, info = tabella_info))}


## ----- 6.4 Funzione per grafico unico di tutte le serie storiche -----
plot_tutte_destag <- function(ts_originale, ts_destag, titolo = "Serie Originali vs Destagionalizzate") {
  time_index <- time(ts_originale)
  df_originale <- as.data.frame(ts_originale)
  df_destag <- as.data.frame(ts_destag)
  df_originale$Time <- df_destag$Time <- time_index
  
  df_long <- bind_rows(
    pivot_longer(df_originale, -Time, names_to = "Serie", values_to = "Valore") %>%
      mutate(Tipo = "Originale"),
    pivot_longer(df_destag, -Time, names_to = "Serie", values_to = "Valore") %>%
      mutate(Tipo = "Destagionalizzata"))
  
  ggplot(df_long, aes(x = Time, y = Valore, color = Serie, linetype = Tipo)) +
    geom_line(linewidth = 0.8) +
    labs(title = titolo, x = "Tempo", y = "Valore") +
    theme_minimal() +
    theme(legend.position = "bottom", legend.text = element_text(size = 8)) +
    guides(color = guide_legend(ncol = 2))}
