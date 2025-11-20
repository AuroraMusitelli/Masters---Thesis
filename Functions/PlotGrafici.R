## Funzione generica per i grafici  
plot_time_series <- function(data, date_col, value_col, title, y_label) {
  ggplot(data, aes(x = !!sym(date_col), y = !!sym(value_col))) +   # Mappa colonne dinamiche su assi
    geom_line(color = "blue", size = 0.9) +                        # Linea della serie temporale
    theme_minimal(base_size = 13) +    # Tema pulito e leggibile
    labs(title = title, x = "Mese", y = y_label) +
    theme(plot.title = element_text(face = "bold", hjust = 0.5), panel.grid.minor = element_blank())}


