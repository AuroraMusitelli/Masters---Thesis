## Funzione grafici sparsity plot per HVAR 
# implementazione della funzione per osservare meglio i grafici sparsity plot di HVAR con i nomi sulle righe e colonne del grafico
library(stringr) 

sparsity_plot_facets <- function(res, title, var_names) {
  
  B     <- coef(res)
  B_mat <- as.matrix(B)
  
  df <- as.data.frame(as.table(B_mat))
  colnames(df) <- c("Equazione", "Regressore", "Valore")
  
  df <- df %>%
    mutate(
      # decodifico YjLk
      j = as.integer(str_match(Regressore, "^Y(\\d+)L(\\d+)$")[, 2]),
      L = as.integer(str_match(Regressore, "^Y(\\d+)L(\\d+)$")[, 3]),
      Var_reg = ifelse(!is.na(j), var_names[j], NA_character_),
      
      # equazioni sulle righe: Yj -> nome variabile
      eq_j     = as.integer(str_match(as.character(Equazione), "^Y(\\d+)$")[, 2]),
      Eq_label = ifelse(!is.na(eq_j), var_names[eq_j], as.character(Equazione))
    ) %>%
    # tolgo l'intercetta: mi interessano solo i coeff.
    filter(!is.na(L))
  
  # ------- ORDINE LOGICO RIGHE/COLONNE --------
  # colonne (regressori) nello stesso ordine del dataset
  df$Var_label <- factor(df$Var_reg, levels = var_names)
  
  # righe: VOGLIO vedere in alto la prima variabile del dataset 
  df$Eq_label  <- factor(df$Eq_label, levels = rev(var_names))
  
  # fattore lag con etichette 
  L_vals    <- sort(unique(df$L))
  df$Lag_f  <- factor(df$L,
                      levels = L_vals,
                      labels = paste0("(", L_vals, ")"))
  
  ggplot(df, aes(x = Var_label, y = Eq_label, fill = Valore)) +
    geom_tile() +
    scale_fill_gradient2(
      low  = "red",
      mid  = "white",
      high = "blue",
      midpoint = 0) +
    facet_wrap(~ Lag_f, nrow = 1) +
    labs(
      title = title,
      x     = "Variabile (regressore)",
      y     = "Equazioni del VAR",
      fill  = "Coeff."
    ) +
    theme_minimal(base_size = 11) +
    theme(
      axis.text.x  = element_text(angle = 90, vjust = 0.5, hjust = 1),
      axis.text.y  = element_text(size = 9),
      plot.title   = element_text(face = "bold", hjust = 0.5),
      strip.text   = element_text(face = "bold"))}




## Plot per CO2
var_names_CO2 <- colnames(Y_mat_CO2)
p_CO2_HC_fac  <- sparsity_plot_facets(res_CO2_HC,"HLAGC - Componentwise (CO2)", var_names_CO2)
p_CO2_HOO_fac <- sparsity_plot_facets(res_CO2_HOO,"HLAGOO - Own/Other (CO2)", var_names_CO2)
p_CO2_HE_fac  <- sparsity_plot_facets(res_CO2_HE, "HLAGELEM - Elementwise (CO2)", var_names_CO2)

print(p_CO2_HC_fac)
print(p_CO2_HOO_fac)
print(p_CO2_HE_fac)



## Plot per ENERGY
var_names_ENERGY <- colnames(Y_mat_ENERGY)
p_ENERGY_HC_fac  <- sparsity_plot_facets(res_ENERGY_HC, "HLAGC - Componentwise (ENERGY)", var_names_ENERGY)
p_ENERGY_HOO_fac <- sparsity_plot_facets(res_ENERGY_HOO, "HLAGOO - Own/Other (ENERGY)", var_names_ENERGY)
p_ENERGY_HE_fac  <- sparsity_plot_facets(res_ENERGY_HE, "HLAGELEM - Elementwise (ENERGY)", var_names_ENERGY)

print(p_ENERGY_HC_fac)
print(p_ENERGY_HOO_fac)
print(p_ENERGY_HE_fac)



