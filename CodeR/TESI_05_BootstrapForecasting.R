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
library(mFilter) 

## --- Pubblicazione e grafica avanzata ---
library(ggpubr)       
library(writexl)
library(devtools)
library(gridExtra)
library(future.apply)





####################################################################
#### ALGORITMO: BOOTSTRAP FORECASTING (SOLO COMPONENTE CICLICA) ####
####################################################################

# 0. SETUP PARAMETRI E AMBIENTE 
h <- 60        
B <- 1000        
L_block <- 5        # Lunghezza del blocco (L) Wild Block Bootstrap
set.seed(856741) 
plan(multisession, workers = parallel::detectCores() - 1)    # velocizza la Phase 2


# 1. FUNZIONE DI FORECAST RECURSIVE [STEP E] 
# Questa funzione implementa la previsione iterativa del modello VARX-L.
# Calcola y_{t+1} usando i lag di y e x, poi usa y_{t+1} per calcolare y_{t+2}.
manual_forecast <- function(Y_data, X_data, X_future, beta, p, s, h_ahead) {
  K <- nrow(beta)
  fcst <- matrix(0, h_ahead, K)
  Y_all <- Y_data 
  
  for(i in 1:h_ahead) {
    # Estrazione dei lag per le endogene (p) e le esogene (s)
    lags_y <- as.vector(t(Y_all[nrow(Y_all):(nrow(Y_all)-p+1), ]))
    X_combined <- rbind(X_data, X_future)
    current_t_x <- nrow(X_data) + i - 1
    lags_x <- as.vector(t(X_combined[current_t_x:(current_t_x-s+1), ]))
    
    # Costruzione del vettore dei regressori Z (Intercetta + Lags)
    Z <- c(1, lags_y, lags_x)
    y_next <- beta %*% Z    # Proiezione lineare (Previsione)
    fcst[i, ] <- as.numeric(y_next)
    Y_all <- rbind(Y_all, t(y_next)) 
  }
  return(fcst)
}


# 2. DECOMPOSIZIONE & RESIDUI [STEP A, B, D] 
# [A] TRAMO-SEATS: Estrazione Stagionalità (S) e Serie Destagionalizzata (SA)
extract_tramo_components <- function(y) {
  modello <- RJDemetra::tramoseats(ts(y, frequency = 12), spec = "RSA5")
  return(list(s = as.numeric(modello$final$series[,"s"]), sa = as.numeric(modello$final$series[,"sa"])))
}


# [B] HPJ Filter: Scomposizione della serie SA in Trend (T) e Ciclo (C)
# Estrae anche i residui epsilon_t per la Phase 2.
get_hpj_components <- function(y_sa, serie_name) {
  fit <- hpj(zoo::na.approx(as.numeric(y_sa), na.rm = FALSE))
  res_epsilon <- as.numeric(fit$residuals)
  # Salvataggio residui filtro per diagnostica 
  write.csv(res_epsilon, paste0("Residui_Export/EPSILON_residui_HPJ_", serie_name, ".csv"))
  return(list(trend = fit$hpj, cycle = as.numeric(y_sa) - fit$hpj, res_eps = res_epsilon))
}


# [D] SARIMA sulle Esogene: Necessario per prevedere i futuri valori di X_t
# Estrae i residui eta_t per il bootstrap delle esogene.
M <- ncol(X_mat)
modelli_x <- lapply(1:M, function(m) forecast::auto.arima(X_mat[, m]))
res_eta <- sapply(modelli_x, residuals)
write.csv(res_eta, "Residui_Export/ETA_residui_esogene.csv") 


# 3. MASTER BOOTSTRAP FUNCTION [PHASE 1 & 2] 
run_full_bootstrap <- function(Y_endog, X_exog, lambda_val, resids_input, type_name) {
  K <- ncol(Y_endog)
  
  # --- PHASE 1: STIMA ORIGINALE [STEP C]  ---
  fit_orig <- BigVAR.fit(cbind(Y_endog, X_exog), p = 12, struct = "Lag", lambda = lambda_val, 
                         intercept = TRUE, VARX = list(k = K, s = 6, contemp = FALSE))
  
  # Salvataggio Coefficienti Beta_hat
  coef_matrix <- fit_orig[,,1]
  write.csv(coef_matrix, paste0("Residui_Export/Coefficienti_VARXL_", type_name, ".csv"))
  
  # Salvataggio residui omega_t del modello VARX 
  res_omega <- resids_input 
  write.csv(res_omega, paste0("Residui_Export/OMEGA_residui_VARX_", type_name, ".csv"))
  
  # Calcolo del Point Forecast 
  X_fut_point <- sapply(modelli_x, function(m) as.numeric(forecast(m, h = h)$mean))
  point_fcst <- manual_forecast(Y_endog, X_exog, X_fut_point, coef_matrix, 12, 6, h)
  

  # --- PHASE 2: BOOTSTRAP LOOP ---
  # Calcolo dei valori fittati (C_hat = C - residui omega_t) per la generazione delle pseudo-serie
  Y_fit_vals <- Y_endog[(12 + 1):nrow(Y_endog), ] - res_omega
  
  boot_dist_list <- future_lapply(1:B, function(b) {
    # 1. Generazione Pseudo-esogene X* tramite simulazione SARIMA (Step D)
    X_ps <- sapply(modelli_x, function(m) as.numeric(simulate(m, nsim = nrow(X_exog))))
    X_f_b <- sapply(modelli_x, function(m) as.numeric(forecast(m, h = h)$mean))
    
    # 2. Resampling dei residui tramite Wild Block Bootstrap e creazione Pseudo-endogene Y*
    # Questo passaggio ricrea la variabilità stocastica della Phase 2
    Y_ps_res <- Res_Block_Wild_BootGenerator(e = res_omega, nsim = 1, yhat = Y_fit_vals, 
                                             L = L_block, Wild = TRUE, Seed = b)
    Y_ps <- Y_endog; Y_ps[(12 + 1):nrow(Y_endog), ] <- Y_ps_res$y_b[,,1]
    
    # 3. Ristima: il modello viene ri-allenato sulle pseudo-serie per catturare l'incertezza dei parametri
    fit_b <- BigVAR.fit(cbind(Y_ps, X_ps), p = 12, struct = "Lag", lambda = lambda_val, 
                        intercept = TRUE, VARX = list(k = K, s = 6, contemp = FALSE))
    
    # 4. Forecast della replica b
    return(manual_forecast(Y_ps, X_ps, X_f_b, fit_b[,,1], 12, 6, h))
  }, future.seed = TRUE)
  
  # Distribuzione Bootstrap
  boot_dist <- array(unlist(boot_dist_list), dim = c(h, K, B))
  saveRDS(boot_dist, paste0("Residui_Export/Distrib_Boot_", type_name, ".rds"))
  
  # Calcolo dell'MSE del Bootstrap: misura la discrepanza media tra le simulazioni e la previsione puntuale
  mse_val <- mean((boot_dist - as.vector(point_fcst))^2)
  return(list(dist = boot_dist, point = point_fcst, names = colnames(Y_endog), mse = mse_val))
}


# 4. FUNZIONE ESPORTAZIONE CSV PER TESI
export_forecast_results <- function(risultati, type_name) {
  dist <- risultati$dist
  K <- dim(dist)[2]
  nomi <- risultati$names
  
  df_final <- data.frame(Mese_Ahead = 1:h)
  for(k in 1:K) {
    df_final[[paste0(nomi[k], "_Point")]] <- risultati$point[,k]
    df_final[[paste0(nomi[k], "_Low95")]] <- apply(dist[,k,], 1, quantile, 0.025)
    df_final[[paste0(nomi[k], "_Upp95")]] <- apply(dist[,k,], 1, quantile, 0.975)
  }
  write.csv(df_final, paste0("Risultati_Finali/Tabella_Forecast_", type_name, ".csv"), row.names = FALSE)
}


# 5. ESECUZIONE E VISUALIZZAZIONE 
# Inizio del monitoraggio temporale totale
inizio_totale <- Sys.time()
cat("\n>>> INIZIO ELABORAZIONE COMPLETA:", inizio_totale, "<<<\n")

# --- Elaborazione CO2 ---
cat("\nAvvio simulazione CO2...")
tempo_co2_start <- Sys.time()
res_CO2 <- run_full_bootstrap(Y_mat_CO2, X_mat, lambda_opt_CO2_VARXL, res_CO2_VARXL@resids, "CO2")
tempo_co2_end <- Sys.time()
cat("\nTempo impiegato per CO2:", round(difftime(tempo_co2_end, tempo_co2_start, units = "mins"), 2), "minuti.\n")

# --- Elaborazione Energy ---
cat("\nAvvio simulazione Energy...")
tempo_en_start <- Sys.time()
res_EN  <- run_full_bootstrap(Y_mat_EN, X_mat, lambda_opt_EN_VARXL, res_EN_VARXL@resids, "Energy")
tempo_en_end <- Sys.time()
cat("\nTempo impiegato per Energy:", round(difftime(tempo_en_end, tempo_en_start, units = "mins"), 2), "minuti.\n")

# Esportazione tabelle e plot
cat("\nSalvataggio risultati e generazione grafici...")
export_forecast_results(res_CO2, "CO2")
export_forecast_results(res_EN, "Energy")


# Funzione Plot 
plot_thesis_results <- function(risultati, Y_original, type_name) {
  n_hist_total <- nrow(Y_original) 
  h <- nrow(risultati$point)
  dist <- risultati$dist
  point <- risultati$point
  nomi <- risultati$names
  
  low <- apply(dist, c(1, 2), quantile, 0.025)
  upp <- apply(dist, c(1, 2), quantile, 0.975)
  
  data_fine <- as.Date("2025-07-01")
  date_storia <- seq(from = data_fine, length.out = n_hist_total, by = "-1 month")
  date_storia <- rev(date_storia)
  date_forecast <- seq(from = data_fine, length.out = h + 1, by = "1 month")
  
  df_full <- data.frame()
  for(k in 1:ncol(point)) {
    last_hist_val <- Y_original[nrow(Y_original), k]
    df_h <- data.frame(T = date_storia, Val = Y_original[,k], Type = "Storico", Serie = nomi[k], L = NA, U = NA)
    df_f <- data.frame(T = date_forecast, Val = c(last_hist_val, point[,k]), Type = "Previsione", 
      Serie = nomi[k], L = c(last_hist_val, low[,k]), U = c(last_hist_val, upp[,k]))
    df_full <- dplyr::bind_rows(df_full, df_h, df_f)}
  
  p <- ggplot(df_full, aes(x = T, y = Val)) +
    # Area di incertezza
    geom_ribbon(data = subset(df_full, Type == "Previsione"), 
                aes(ymin = L, ymax = U, fill = "Incertezza 95%"), alpha = 0.2) +
    geom_line(data = subset(df_full, Type == "Storico"), aes(color = "Storico"), size = 0.4) +
    geom_line(data = subset(df_full, Type == "Previsione"), aes(color = "Previsione"), size = 0.6) +
    facet_wrap(~Serie, scales = "free_y", nrow = 5, ncol = 2) + 
    scale_x_date(date_labels = "%Y", date_breaks = "5 years", expand = c(0.02, 0)) + 
    scale_color_manual(values = c("Storico" = "black", "Previsione" = "green")) +
    scale_fill_manual(values = c("Incertezza 95%" = "purple")) +
    labs(title = paste("Previsioni Ciclo:", type_name), 
         x = "Anno", y = "Ciclo economico",
         color = "Tipo", fill = "") +
    theme_minimal() + 
    theme(legend.position = "bottom",panel.grid.minor = element_blank(), # Pulisce il grafico
          strip.text = element_text(size = 9, face = "bold"),
          axis.text = element_text(size = 8))
  ggsave(paste0("Risultati_Finali/Plot_Completo_", type_name, ".png"), p, width = 14, height = 18)
  return(p)
}

print(plot_thesis_results(res_CO2, Y_mat_CO2, "CO2"))
print(plot_thesis_results(res_EN, Y_mat_EN, "Energy"))


# Fine del monitoraggio temporale 
fine_totale <- Sys.time()
tempo_totale <- difftime(fine_totale, inizio_totale, units = "auto")

cat("\nCOMPLETATO CON SUCCESSO")
cat("\nInizio:", format(inizio_totale, "%H:%M:%S"))
cat("\nFine:", format(fine_totale, "%H:%M:%S"))
cat("\nTempo totale di esecuzione:", round(tempo_totale, 2), units(tempo_totale))
