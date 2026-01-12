#######################   TESI MAGISTRALE   #########################    
############   Aurora Musitelli, Matricola: 856741   ################ 



#########################################################################
#### 0. Importazione delle librerie utilizzate per il lavoro di tesi ####
#########################################################################

## --- Manipolazione e visualizzazione dati ---
library(tidyverse)   
library(readxl)          
library(lubridate)       

## --- Serie storiche ---
library(tsibble)        
library(tsbox)          
library(zoo)           
library(xts)           
library(forecast)      
library(fpp2)     
library(seastests)
library(strucchange)

## --- Test di stazionarietà e diagnostica ---
library(tseries)         
library(urca)            
library(performance) 
library(seasonal)
library(jumps)
library(RJDemetra)

## --- Modelli avanzati multivariati ---
library(vars)           
library(BigVAR)         
library(glmnet)   
library(expm)

## --- Gerarchia temporale ---
library(hts)            

## --- Pubblicazione e grafica avanzata ---
library(ggpubr)       
library(writexl)
library(devtools)
library(gridExtra)




##############################################################
#### FORECAST / PREVISIONI per il modello migliore VARX-L ####
##############################################################
h <- 60       # Orizzonte di previsione 60 mesi = 5 anni
p_lag <- 12     # ritardi endogeni
s_lag <- 6      # ritardi esogeni



# 1. PREVISIONE VARIABILI ESOGENE (SARIMA) 
# Per ogni variabile esogena stimo un modello SARIMA e produco previsioni ad h passi in avanti
X_fut <- apply(X_mat, 2, function(x) {as.numeric(forecast(auto.arima(x, seasonal = TRUE), h = h)$mean)})
X_all <- rbind(X_mat, X_fut)   # Unisco dati esogeni storici + futuri



# 2. FUNZIONE DI FORECAST RICORSIVO + TREND 
# uso i coefficienti stimati del modello VARX-L
# genero previsioni ricorsive multi-step
# riaggiungo il trend stimato separatamente con ARIMA
run_forecast_full <- function(res_model, Y_mat, X_all_mat, h_target) {
  n_v <- ncol(Y_mat)                # Numero variabili endogene
  B <- BigVAR::coef(res_model)        # Matrice coefficienti VARX-L
  intercept <- as.numeric(B[, 1])    # Intercette
  Phi <- as.matrix(B[, 2:ncol(B)])   # Coefficienti lag Y e X
  # Matrice dati storici + previsioni
  Y_all_loop <- rbind(Y_mat, matrix(0, nrow = h_target, ncol = n_v))
  T_s <- nrow(Y_mat)
  
  # Forecast VARX-L
  for (t in 1:h_target) {
    # Lag delle variabili endogene
    idx_y <- seq(T_s + t - 1, T_s + t - p_lag)
    lags_Y <- as.vector(t(Y_all_loop[idx_y, ]))
    # Lag delle variabili esogene (previste)
    idx_x <- seq(T_s + t - 1, T_s + t - s_lag)
    lags_X <- as.vector(t(X_all_mat[idx_x, ]))
    # Equazione VARX-L
    Y_all_loop[T_s + t, ] <- intercept + (Phi %*% c(lags_Y, lags_X))}
  
  # Ricostruzione del Trend
  preds_reali <- matrix(NA, nrow = h_target, ncol = n_v)
  for(j in 1:n_v) {
    tr_f <- as.numeric(forecast(auto.arima(Y_mat[,j]), h = h_target)$mean)
    preds_reali[,j] <- Y_all_loop[(T_s+1):(T_s+h_target), j] + tr_f
  }
  colnames(preds_reali) <- colnames(Y_mat)
  return(as.data.frame(preds_reali))
}



# 3. ESECUZIONE PER CO2 E ENERGY 
df_forecast_CO2 <- run_forecast_full(res_CO2_VARXL, Y_mat_CO2, X_all, h)
df_forecast_EN  <- run_forecast_full(res_EN_VARXL, Y_mat_EN, X_all, h)
# Aggiungo le date
dates_fore <- seq.Date(from = max(dataCO2_varx$Month) %m+% months(1), by = "month", length.out = h)
df_forecast_CO2$Month <- df_forecast_EN$Month <- dates_fore

dataCO2_forecast <- as.data.frame(df_forecast_CO2)
dataENERGY_forecast <- as.data.frame(df_forecast_EN)

saveRDS(dataCO2_forecast,   "dataCO2_forecast.rds")
saveRDS(dataENERGY_forecast, "dataENERGY_forecast.rds")



# 4. Funzione per generare grafico a pannelli
grafico <- function(df_hist_raw, df_prev, titolo, y_lab) {
  # Preparazione dati storici 
  df_h_long <- df_hist_raw %>% dplyr::select(Month, any_of(colnames(df_prev)[colnames(df_prev) != "Month"])) %>% 
    pivot_longer(-Month, names_to = "Settore_Fonte", values_to = "Valore") %>% mutate(Tipo = "Storico")
  # Preparazione dati previsti
  df_p_long <- df_prev %>% pivot_longer(-Month, names_to = "Settore_Fonte", values_to = "Valore") %>% mutate(Tipo = "Previsione")
  # Unissco
  df_full <- rbind(df_h_long, df_p_long)
  
  # Creazione grafico ggplot
  ggplot(df_full, aes(x = Month, y = Valore, color = Tipo)) +
    geom_line(linewidth = 0.7) + facet_wrap(~ Settore_Fonte, scales = "free_y", ncol = 2) + 
    scale_color_manual(values = c("Storico" = "black", "Previsione" = "green")) +
    geom_vline(xintercept = as.numeric(max(df_hist_raw$Month)), linetype = "dashed", color = "red", alpha = 0.5) +
    labs(title = titolo, subtitle = "Modello VARX-L: Analisi dinamica 2025-2030", x = "Anno", y = y_lab) +
    theme_minimal() + theme(legend.position = "bottom",
          strip.text = element_text(face = "bold", size = 9), panel.spacing = unit(1, "lines"))}


# Grafico CO2 
p_co2_final <- grafico(dataCO2_varx, df_forecast_CO2, "Forecast Emissioni CO2 USA per Settore e Fonte", "Million Metric Tons")
print(p_co2_final)

# Grafico ENERGY 
p_energy_final <- grafico(dataENERGY_varx, df_forecast_EN, "Forecast Consumi Energetici USA per Settore e Fonte", "Trillion BTU")
print(p_energy_final)




