# Funzione per salvare gli istogrammi con curva normale stimata + QQ-plot
salva_grafico_distribuzione <- function(df, settore, fonte, metrica, output_folder = "Distribuzioni_EmpiricheDati") {
  
  df_sub <- df %>%
    filter(Sector == settore, Source == fonte, Metric == metrica, !is.na(Value))   # filtro dati per settore, fonte e metrica
  if (nrow(df_sub) < 30) return(NULL)   # se meno di 30 osservazioni (non significativo)
  
  # Istogramma con curva normale stimata
  p1 <- ggplot(df_sub, aes(x = Value)) +
    geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "red", alpha = 0.5) + # istogramma con densità
    stat_function(fun = dnorm, 
                  args = list(mean = mean(df_sub$Value), sd = sd(df_sub$Value)), 
                  color = "blue", linewidth = 1) + # curva normale stimata con media e sd dei dati
    ggtitle(paste("Istogramma -", metrica, "-", settore, "-", fonte)) +
    theme_minimal()
  
  # QQ-plot per confrontare quantili empirici e teorici della distribuzione normale
  p2 <- ggplot(df_sub, aes(sample = Value)) +
    stat_qq(color = "red") +
    stat_qq_line(color = "blue") +
    ggtitle(paste("QQ-Plot -", metrica, "-", settore, "-", fonte)) +
    theme_minimal()
  if (!dir.exists(output_folder)) dir.create(output_folder)   # creo cartella output
  nome_file <- paste0(metrica, "_", gsub(" ", "_", settore), "_", gsub(" ", "_", fonte), ".png") # nome file pulito
  
  # salvo istogramma e QQ-plot in file separati
  ggsave(filename = file.path(output_folder, nome_file), plot = p1, width = 8, height = 6, dpi = 300)
  ggsave(filename = file.path(output_folder, paste0("QQ_", nome_file)), plot = p2, width = 8, height = 6, dpi = 300)}


# Funzione per applicare test di normalità a ciascuna serie storica (Sector / Source / Metric)
test_normalita <- function(df_long) {
  risultati <- df_long %>%
    filter(!is.na(Value)) %>%   # rimuovo valori mancanti
    group_by(Sector, Source, Metric) %>%   # raggruppo per combinazione
    summarise(
      N = n(),   # numero osservazioni
      JB_p_value = tryCatch(jarque.bera.test(Value)$p.value, error = function(e) NA),  # Jarque-Bera
      Shapiro_p = if (n() <= 5000) tryCatch(shapiro.test(Value)$p.value, error = function(e) NA) else NA, # Shapiro-Wilk
      KS_p = tryCatch(lillie.test(Value)$p.value, error = function(e) NA),  # Kolmogorov-Smirnov 
      .groups = "drop") %>%
    mutate(
      # Classificazione "Normale" / "Non Normale" in base a valore del p-value (< 0.05 = non normale)
      JB_Result = ifelse(JB_p_value < 0.05, "Non Normale", "Normale"),
      Shapiro_Result = ifelse(Shapiro_p < 0.05, "Non Normale", "Normale"),
      KS_Result = ifelse(KS_p < 0.05, "Non Normale", "Normale"),
      Consenso = ifelse(JB_Result == "Normale" & Shapiro_Result == "Normale" & KS_Result == "Normale",
                        "Normale", "Non Normale")   # se tutti e 3 dicono "Normale" -> Normale
    ) %>%
    arrange(Metric, Sector, Source)   # ordino risultati
  return(risultati)}
