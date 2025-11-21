## Funzioni per struttura HVAR  

##  Queste funzioni permettono di:
##   1) estrarre quali lag sono attivi (coefficiente != 0) per ogni equazione e per ogni struttura HLag (HC, HOO, HE)
##   2) convertire la matrice logica in formato "tidy" per la visualizzazione
##   3) creare il classico “block plot” che mostra la sparsità imposta da BigVAR.


# Funzione per estrarre, da un oggetto BigVAR.results, quali lag sono attivi per ogni equazione
get_hlag_activity <- function(res, series_names = NULL, eps = 1e-6) {
  beta_mat <- coef(res)              # matrice delle stime: k x (k*p + 1)
  k <- nrow(beta_mat)                # numero di variabili endogene
  p <- res@lagmax                    # numero di lag massimi usati nel modello
  
  # tolgo l'intercetta
  B <- beta_mat[, -1, drop = FALSE]  # k x (k*p)
  if (ncol(B) != k * p) {
    stop("Dimensione inattesa: controllare modello sia un VAR puro (senza esogene) con lagmax = p.")
  }
  
  # Matrice logica k x p: TRUE se, per quell'equazione e lag, almeno un coefficiente è != 0
  active <- matrix(FALSE, nrow = k, ncol = p)
  for (ell in 1:p) {
    idx_cols <- ((ell - 1) * k + 1):(ell * k)   # colonne relative al lag ell
    block <- B[, idx_cols, drop = FALSE]       # estraggo blocco k x k del lag ell
    active[, ell] <- apply(abs(block) > eps, 1, any)   # Verifico se almeno un coefficiente del blocco è diverso da zero
  }
  # Assegno nomi alle righe e alle colonne
  if (!is.null(series_names)) {
    rownames(active) <- series_names
  }
  colnames(active) <- paste0("Lag", 1:p)
  
  return(active)
}


# Funzione che converte la matrice k x p in formato "tidy" per ggplot
hlag_activity_tidy <- function(active_mat) {
  df <- as.data.frame(active_mat)
  df$Serie <- rownames(active_mat)
  
  df_long <- df %>%
    relocate(Serie) %>%   # metto la colonna Serie come prima
    pivot_longer(
      cols      = starts_with("Lag"),
      names_to  = "Lag",
      values_to = "Active"
    ) %>%
    mutate(
      Lag   = as.numeric(gsub("Lag", "", Lag)),  # trasformo Lag in numero
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


