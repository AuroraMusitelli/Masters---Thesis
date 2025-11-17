## --------- Funzioni per struttura HVAR --------- ##

# Estrae, da un oggetto BigVAR.results, quali lag sono attivi per ogni equazione
get_hlag_activity <- function(res, series_names = NULL, eps = 1e-6) {
  beta_mat <- coef(res)              # k x (k*p + 1)
  k <- nrow(beta_mat)
  p <- res@lagmax                    # numero di lag massimi usati nel modello
  
  # tolgo l'intercetta
  B <- beta_mat[, -1, drop = FALSE]  # k x (k*p)
  if (ncol(B) != k * p) {
    stop("Dimensione inattesa: controlla che il modello sia un VAR puro (senza esogene) con lagmax = p.")
  }
  
  # matrice logica k x p: TRUE se, per quell'equazione e lag, almeno un coefficiente è != 0
  active <- matrix(FALSE, nrow = k, ncol = p)
  for (ell in 1:p) {
    idx_cols <- ((ell - 1) * k + 1):(ell * k)   # colonne relative al lag ell
    block <- B[, idx_cols, drop = FALSE]       # k x k (eq x variabili)
    active[, ell] <- apply(abs(block) > eps, 1, any)
  }
  
  if (!is.null(series_names)) {
    rownames(active) <- series_names
  }
  colnames(active) <- paste0("Lag", 1:p)
  
  return(active)
}

# Converte la matrice k x p in formato "tidy" per ggplot
hlag_activity_tidy <- function(active_mat) {
  df <- as.data.frame(active_mat)
  df$Serie <- rownames(active_mat)
  
  df_long <- df %>%
    relocate(Serie) %>%
    pivot_longer(
      cols      = starts_with("Lag"),
      names_to  = "Lag",
      values_to = "Active"
    ) %>%
    mutate(
      Lag   = as.numeric(gsub("Lag", "", Lag)),
      Serie = factor(Serie, levels = rev(unique(Serie)))  # per avere l'ordine verticale 
    )
  
  df_long
}

# Grafico tipo "block structure" (celle grigie = lag attivi, bianche = lag nulli)
plot_hlag_activity <- function(active_mat, titolo = "Struttura HLAG") {
  df_long <- hlag_activity_tidy(active_mat)
  
  ggplot(df_long, aes(x = Lag, y = Serie, fill = Active)) +
    geom_tile(color = "grey60") +
    scale_fill_manual(values = c("TRUE" = "grey40", "FALSE" = "white")) +
    scale_x_continuous(breaks = 1:ncol(active_mat)) +
    labs(title = titolo,
         x = "Lag",
         y = "Equazione (serie risposta)",
         fill = "Attivo") +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, face = "bold")
    )
}
