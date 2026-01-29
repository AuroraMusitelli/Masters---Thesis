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




##############################
#### 1. Importo i dataset ####
##############################

#-----------------------
## VARIABILI ENDOGENE ##
#-----------------------
### Caricamento dei dati pre-processati precedentemente
url1 <- "https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Data/dataCO2_cycle.rds"
url2 <- "https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Data/dataENERGY_cycle.rds"
# Percorsi locali temporanei GitHub
tmp_file1 <- tempfile(fileext = ".rds")
tmp_file2 <- tempfile(fileext = ".rds")
# Scarico i file
download.file(url1, destfile = tmp_file1, mode = "wb")
download.file(url2, destfile = tmp_file2, mode = "wb")
# Leggo i file RDS
dataCO2 <- readRDS(tmp_file1)
dataENERGY <- readRDS(tmp_file2)



## Aggregazione per macro-fonte (sommatoria delle serie trasformate)
# CO2: Aggregazione per macro-fonte mantengo i 5 settori
dataCO2_agg <- dataCO2 %>%
  mutate(
    Fossile_Industrial = rowSums(dplyr::select(., starts_with("CO2_Industrial_") &
                                                 (contains("Coal") | contains("Petroleum") | contains("NaturalGas") |
                                                    contains("MotorGasoline") | contains("FuelOil"))), na.rm = TRUE),
    Rinnovabile_Industrial = rowSums(dplyr::select(., starts_with("CO2_Industrial_") &
                                                     (contains("BiomassEnergy") | contains("HydroelectricPower") |
                                                        contains("WindEnergy") | contains("SolarEnergy") |
                                                        contains("GeoEnergy"))), na.rm = TRUE),

    Fossile_Commercial = rowSums(dplyr::select(., starts_with("CO2_Commercial_") &
                                                 (contains("Coal") | contains("Petroleum") | contains("NaturalGas") |
                                                    contains("MotorGasoline") | contains("FuelOil"))), na.rm = TRUE),
    Rinnovabile_Commercial = rowSums(dplyr::select(., starts_with("CO2_Commercial_") &
                                                     (contains("BiomassEnergy") | contains("HydroelectricPower") |
                                                        contains("WindEnergy") | contains("SolarEnergy") |
                                                        contains("GeoEnergy"))), na.rm = TRUE),

    Fossile_Residential = rowSums(dplyr::select(., starts_with("CO2_Residential_") &
                                                  (contains("Coal") | contains("Petroleum") | contains("NaturalGas") |
                                                     contains("MotorGasoline") | contains("FuelOil"))), na.rm = TRUE),
    Rinnovabile_Residential = rowSums(dplyr::select(., starts_with("CO2_Residential_") &
                                                      (contains("BiomassEnergy") | contains("HydroelectricPower") |
                                                         contains("WindEnergy") | contains("SolarEnergy") |
                                                         contains("GeoEnergy"))), na.rm = TRUE),

    Fossile_ElectricPower = rowSums(dplyr::select(., starts_with("CO2_ElectricPower_") &
                                                    (contains("Coal") | contains("Petroleum") | contains("NaturalGas") |
                                                       contains("MotorGasoline") | contains("FuelOil"))), na.rm = TRUE),
    Rinnovabile_ElectricPower = rowSums(dplyr::select(., starts_with("CO2_ElectricPower_") &
                                                        (contains("BiomassEnergy") | contains("HydroelectricPower") |
                                                           contains("WindEnergy") | contains("SolarEnergy") |
                                                           contains("GeoEnergy"))), na.rm = TRUE),
   
    Fossile_Transportation = rowSums(dplyr::select(., starts_with("CO2_Transportation_") &
                                                     (contains("Coal") | contains("Petroleum") | contains("NaturalGas") |
                                                        contains("MotorGasoline") | contains("FuelOil"))), na.rm = TRUE),
    Rinnovabile_Transportation = rowSums(dplyr::select(., starts_with("CO2_Transportation_") &
                                                         (contains("BiomassEnergy") | contains("HydroelectricPower") |
                                                            contains("WindEnergy") | contains("SolarEnergy") |
                                                            contains("GeoEnergy"))), na.rm = TRUE)) %>%
  dplyr::select(Month, Date_idx, starts_with("Fossile_"), starts_with("Rinnovabile_"))


# ENERGY: Aggregazione per macro-fonte mantenendo i 5 settori
dataENERGY_agg <- dataENERGY %>%
  mutate(
    Fossile_Industrial = rowSums(dplyr::select(., starts_with("Energy_Industrial_") &
                                                 (contains("Coal") | contains("Petroleum") | contains("NaturalGas") |
                                                    contains("MotorGasoline") | contains("FuelOil"))), na.rm = TRUE),
    Rinnovabile_Industrial = rowSums(dplyr::select(., starts_with("Energy_Industrial_") &
                                                     (contains("BiomassEnergy") | contains("HydroelectricPower") |
                                                        contains("WindEnergy") | contains("SolarEnergy") |
                                                        contains("GeoEnergy"))), na.rm = TRUE),

    Fossile_Commercial = rowSums(dplyr::select(., starts_with("Energy_Commercial_") &
                                                 (contains("Coal") | contains("Petroleum") | contains("NaturalGas") |
                                                    contains("MotorGasoline") | contains("FuelOil"))), na.rm = TRUE),
    Rinnovabile_Commercial = rowSums(dplyr::select(., starts_with("Energy_Commercial_") &
                                                     (contains("BiomassEnergy") | contains("HydroelectricPower") |
                                                        contains("WindEnergy") | contains("SolarEnergy") |
                                                        contains("GeoEnergy"))), na.rm = TRUE),

    Fossile_Residential = rowSums(dplyr::select(., starts_with("Energy_Residential_") &
                                                  (contains("Coal") | contains("Petroleum") | contains("NaturalGas") |
                                                     contains("MotorGasoline") | contains("FuelOil"))), na.rm = TRUE),
    Rinnovabile_Residential = rowSums(dplyr::select(., starts_with("Energy_Residential_") &
                                                      (contains("BiomassEnergy") | contains("HydroelectricPower") |
                                                         contains("WindEnergy") | contains("SolarEnergy") |
                                                         contains("GeoEnergy"))), na.rm = TRUE),

    Fossile_ElectricPower = rowSums(dplyr::select(., starts_with("Energy_ElectricPower_") &
                                                    (contains("Coal") | contains("Petroleum") | contains("NaturalGas") |
                                                       contains("MotorGasoline") | contains("FuelOil"))), na.rm = TRUE),
    Rinnovabile_ElectricPower = rowSums(dplyr::select(., starts_with("Energy_ElectricPower_") &
                                                        (contains("BiomassEnergy") | contains("HydroelectricPower") |
                                                           contains("WindEnergy") | contains("SolarEnergy") |
                                                           contains("GeoEnergy"))), na.rm = TRUE),

    Fossile_Transportation = rowSums(dplyr::select(., starts_with("Energy_Transportation_") &
                                                     (contains("Coal") | contains("Petroleum") | contains("NaturalGas") |
                                                        contains("MotorGasoline") | contains("FuelOil"))), na.rm = TRUE),
    Rinnovabile_Transportation = rowSums(dplyr::select(., starts_with("Energy_Transportation_") &
                                                         (contains("BiomassEnergy") | contains("HydroelectricPower") |
                                                            contains("WindEnergy") | contains("SolarEnergy") |
                                                            contains("GeoEnergy"))), na.rm = TRUE)) %>%
  dplyr::select(Month, Date_idx, starts_with("Fossile_"), starts_with("Rinnovabile_"))


## Salvo i dataset aggregati 
dataCO2_agg <- as.data.frame(dataCO2_agg)
dataENERGY_agg <- as.data.frame(dataENERGY_agg)


saveRDS(dataCO2_agg,   "dataCO2_agg.rds")
saveRDS(dataENERGY_agg, "dataENERGY_agg.rds")




#############################################
#### 2. Preparazione dati per VAR e HVAR #### 
#############################################

# Dataset ENDOGENE
Y_CO2    <- dataCO2_agg   %>% dplyr::select(-c(Month, Date_idx))    # elimino Month e Date_idx che NON sono variabili da modellizzare 
Y_ENERGY <- dataENERGY_agg %>% dplyr::select(-c(Month, Date_idx))   # elimino Month e Date_idx che NON sono variabili da modellizzare 

# Matrici per il VAR 
Y_mat_CO2    <- as.matrix(Y_CO2)
Y_mat_ENERGY <- as.matrix(Y_ENERGY)

# INDICI PER ROLLING CROSS VALIDATION (Rolling Window Out-of-Sample Evaluation)
T_tot_CO2 <- nrow(Y_mat_CO2)
T1_CO2 <- floor(0.7 * T_tot_CO2)      # fine training / inizio CV 
T2_CO2 <- floor(0.9 * T_tot_CO2)      # fine CV / inizio test

T_tot_ENERGY <- nrow(Y_mat_ENERGY)
T1_ENERGY <- floor(0.7 * T_tot_ENERGY)
T2_ENERGY <- floor(0.9 * T_tot_ENERGY)            

# Controllo esplicito delle split temporali 
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/CheckSplits.R")

check_splits(Y_mat_CO2,    T1_CO2,    T2_CO2)
check_splits(Y_mat_ENERGY, T1_ENERGY, T2_ENERGY)


## Funzione di valutazione forecast (RMSE, MAE, MSFE)
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/ScoreForecast.R")

# Calcolo RMSE relativo
add_rmse_rel <- function(df_scores, y_true){
  sd_test <- apply(y_true, 2, sd)
  df_scores$RMSE_rel <- df_scores$RMSE / sd_test[df_scores$Serie]
  return(df_scores)}




##############################
#### 3. Stima modello VAR #### 
##############################

## Funzione per stimare modello VAR
# Questa funzione implementa il rolling-expanding window per selezionare il lag ottimale del VAR sulla base delle performance di forecast
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/FunzioneVAR.R")


### --- VAR (CO2) --- ### 
# Rolling-expanding per selezione del lag p
results_CO2 <- run_VAR_rolling(Y_mat_CO2[1:T2_CO2, ], T1_CO2, max_p = 12) 

# Lag ottimale complessivo che minimizza l'RMSE medio
best_p_CO2 <- results_CO2 %>% group_by(p) %>% summarise(RMSE_media = mean(RMSE, na.rm = TRUE)) %>%
  slice_min(RMSE_media) %>% pull(p)  # esce 12 ma instabile
p_use_CO2 <- 6

# Grafico del valore di RMSE medio al variare del lag p. Permette di visualizzare chiaramente quale lag produce l'errore minore
rmse_media_CO2 <- results_CO2 %>% group_by(p) %>% summarise(RMSE_medio = mean(RMSE))
ggplot(rmse_media_CO2, aes(x = p, y = RMSE_medio)) +
  geom_line(size = 1.2, color = "steelblue") + geom_point(size = 3, color = "steelblue") + theme_minimal(base_size = 14) +
  labs(title = "RMSE medio del VAR al variare del lag p (CO2)", x = "Lag p", y = "RMSE medio")


## Stima del VAR finale su 1:T2 (come BigVAR)
# Uso l'intero campione "training + validazione" (1:T2_CO2) per stimare un modello VAR con il lag p_use_CO2 scelto in precedenza
VAR_final_CO2 <- VAR(Y_mat_CO2[1:T2_CO2, ], p = p_use_CO2, type = "const")
coef(VAR_final_CO2)

# Numero di osservazioni previsive (T2+1 : T_tot) 
n_test_CO2 <- T_tot_CO2 - T2_CO2   # n_test_CO2 = numero di previsioni OOS da produrre
preds_CO2 <- matrix(NA, n_test_CO2, ncol(Y_mat_CO2))    # previsioni nel test
true_CO2  <- Y_mat_CO2[(T2_CO2 + 1):T_tot_CO2, ]        # valori veri nel test
colnames(preds_CO2) <- colnames(true_CO2) <- colnames(Y_mat_CO2)

# Rolling / expanding window nel periodo di test
# Per ogni i = 1, ..., n_test_CO2:
# stimo un nuovo VAR_i sui dati fino a T2_CO2 + i - 1 (finestra expanding)
# calcolo la previsione 1-step-ahead per il tempo T2_CO2 + i
for (i in 1:n_test_CO2) {
  # stimo il VAR ogni volta su una finestra crescente (expanding)
  VAR_i <- VAR(Y_mat_CO2[1:(T2_CO2 + i - 1), ], p = p_use_CO2, type = "const")
  fc    <- predict(VAR_i, n.ahead = 1)$fcst     # previsione 1-step-ahead dal modello VAR_i
  preds_CO2[i, ] <- sapply(fc, function(x) x[1])   # estraggo la previsione puntuale per ciascuna serie
}
(sc_VAR_CO2_test <- score_forecast(true_CO2, preds_CO2))   # Calcolo score forecast 
 


### --- VAR (ENERGY) --- ###
# Rolling-expanding per selezione del lag
results_ENERGY <- run_VAR_rolling(Y_mat_ENERGY[1:T2_ENERGY, ], T1_ENERGY, max_p = 12)

# Lag ottimale complessivo che minimizza l'RMSE medio
best_p_ENERGY <- results_ENERGY %>% group_by(p) %>% summarise(RMSE_media = mean(RMSE, na.rm = TRUE)) %>%
  slice_min(RMSE_media) %>% pull(p)  # esce 12 ma instabile
p_use_ENERGY <- 6

# Grafico RMSE medio al variare del lag per ENERGY
rmse_media_ENERGY <- results_ENERGY %>% group_by(p) %>% summarise(RMSE_medio = mean(RMSE))
ggplot(rmse_media_ENERGY, aes(x = p, y = RMSE_medio)) +
  geom_line(size = 1.2, color = "green") + geom_point(size = 3, color = "green") + theme_minimal(base_size = 14) +
  labs(title = "RMSE medio del VAR al variare del lag p (ENERGY)", x = "Lag p", y = "RMSE medio")


## Stima del VAR finale su 1:T2 (come BigVAR)
VAR_final_ENERGY <- VAR(Y_mat_ENERGY[1:T2_ENERGY, ], p = p_use_ENERGY, type = "const")
coef(VAR_final_ENERGY)

# Numero di osservazioni previsive (T2+1 : T_tot) 
n_test_ENERGY <- T_tot_ENERGY - T2_ENERGY   
preds_ENERGY <- matrix(NA, n_test_ENERGY, ncol(Y_mat_ENERGY))
true_ENERGY  <- Y_mat_ENERGY[(T2_ENERGY + 1):T_tot_ENERGY, ]
colnames(preds_ENERGY) <- colnames(true_ENERGY) <- colnames(Y_mat_ENERGY)

# Rolling / expanding window nel periodo di test
for (i in 1:n_test_ENERGY) {
  # stimo il VAR ogni volta su una finestra crescente (expanding)
  VAR_i <- VAR(Y_mat_ENERGY[1:(T2_ENERGY + i - 1), ], p = p_use_ENERGY, type = "const")
  fc    <- predict(VAR_i, n.ahead = 1)$fcst     # previsione 1-step-ahead dal modello VAR_i
  preds_ENERGY[i, ] <- sapply(fc, function(x) x[1])   
}
(sc_VAR_ENERGY_test <- score_forecast(true_ENERGY, preds_ENERGY))




###################################################
#### 4. HVAR / HLag (VAR penalizzato - BigVAR) ####      
###################################################

## Funzione di allineamento y_true e y_hat
align_forecasts <- function(fit_obj, Y_mat, T2, T_tot){
  preds  <- fit_obj@preds                    
  y_true <- Y_mat[(T2+1):T_tot, , drop = FALSE]
  min_len <- min(nrow(preds), nrow(y_true))
  list(y_true = y_true[1:min_len, , drop = FALSE], y_hat  = preds[1:min_len, , drop = FALSE])}


#-----------------
## HVAR per CO2 ##
#-----------------
h_forecast <- 1    # h = 1 (one-step-ahead) per rolling forecasting 

## 4.1.1 Modello HLAGC (Componentwise) 
model_CO2_HC <- constructModel(
  Y      = Y_mat_CO2,          # Matrice delle variabili endogene (non standardizzate)
  p      = 12,                 # Numero massimo di lag endogeni (1 anno)
  struct = "HLAGC",            # Struttura di penalizzazione gerarchica componentwise
  T1     = T1_CO2,             # Fine training / inizio cross-validation
  T2     = T2_CO2,             # Fine cross-validation / inizio test
  gran   = c(400, 7),          # Griglia di lambda
  cv     = "Rolling",          # Rolling cross-validation 
  h      = h_forecast,         # Orizzonte di forecasting = 1
  IC     = FALSE,              # Non uso criteri di informazione per selezionare lambda
  verbose = TRUE,              
  model.controls = list(
    intercept   = TRUE,       
    standardize = TRUE))        

res_CO2_HC <- cv.BigVAR(model_CO2_HC)  # BigVAR seleziona automaticamente il lambda ottimale sulla rolling CV e costruisce il modello finale stimato su 1:T2
aligned_HC  <- align_forecasts(res_CO2_HC, Y_mat_CO2, T2_CO2, T_tot_CO2)    # allineamento previsioni vs valori reali
sc_CO2_HC   <- score_forecast(aligned_HC$y_true, aligned_HC$y_hat)          # calcolo delle metriche di forecast
# Struttura lag per CO2 - quali lag/equazioni hanno coefficiente diverso da zero
SparsityPlot.BigVAR.results(res_CO2_HC)  # LIVELLO DI MAGNITUDINE DEI COEFFICIENTI
B <- coef(res_CO2_HC)
colnames(B)


## 4.1.2 Modello HLAGOO (Own/Other)
model_CO2_HOO <- constructModel(
  Y      = Y_mat_CO2,
  p      = 12,
  struct = "HLAGOO",            # Struttura di penalizzazione Own/Other
  T1     = T1_CO2,
  T2     = T2_CO2,
  gran   = c(400, 7),
  cv     = "Rolling",
  h      = h_forecast,
  IC     = FALSE,
  verbose = TRUE,
  model.controls = list(
    intercept   = TRUE,
    standardize = TRUE))

res_CO2_HOO <- cv.BigVAR(model_CO2_HOO)
aligned_HOO <- align_forecasts(res_CO2_HOO, Y_mat_CO2, T2_CO2, T_tot_CO2)
sc_CO2_HOO  <- score_forecast(aligned_HOO$y_true, aligned_HOO$y_hat)
# Struttura lag per CO2
SparsityPlot.BigVAR.results(res_CO2_HOO)


## 4.1.3 Modello HLAGELEM (Elementwise)
model_CO2_HE <- constructModel(
  Y      = Y_mat_CO2,
  p      = 12,
  struct = "HLAGELEM",          # Struttura di penalizzazione Elementwise 
  T1     = T1_CO2,
  T2     = T2_CO2,
  gran   = c(400, 7),
  cv     = "Rolling",
  h      = h_forecast,
  IC     = FALSE,
  verbose = TRUE,
  model.controls = list(
    intercept   = TRUE,
    standardize = TRUE))

res_CO2_HE <- cv.BigVAR(model_CO2_HE)
aligned_HE <- align_forecasts(res_CO2_HE, Y_mat_CO2, T2_CO2, T_tot_CO2)
sc_CO2_HE  <- score_forecast(aligned_HE$y_true, aligned_HE$y_hat)
# Struttura lag per CO2
SparsityPlot.BigVAR.results(res_CO2_HE)



#--------------------
## HVAR per ENERGY ##
#--------------------
## 4.2.1 Modello HLAGC (Componentwise)
model_ENERGY_HC <- constructModel(
  Y      = Y_mat_ENERGY,
  p      = 12,
  struct = "HLAGC",          # Penalizzazione Componentwise
  T1     = T1_ENERGY,
  T2     = T2_ENERGY,
  gran   = c(400, 7),
  cv     = "Rolling",
  h      = h_forecast,
  IC     = FALSE,
  verbose = TRUE,
  model.controls = list(
    intercept   = TRUE,
    standardize = TRUE))

res_ENERGY_HC <- cv.BigVAR(model_ENERGY_HC)
aligned_ENERGY_HC <- align_forecasts(res_ENERGY_HC, Y_mat_ENERGY, T2_ENERGY, T_tot_ENERGY)
sc_ENERGY_HC     <- score_forecast(aligned_ENERGY_HC$y_true, aligned_ENERGY_HC$y_hat)
# Struttura lag per ENERGY
SparsityPlot.BigVAR.results(res_ENERGY_HC)   # LIVELLO DI MAGNITUDINE DEI COEFFICIENTI
C <- coef(res_ENERGY_HC)
colnames(C)


## 4.2.2 Modello HLAGOO (Own/Other)
model_ENERGY_HOO <- constructModel(
  Y      = Y_mat_ENERGY,
  p      = 12,
  struct = "HLAGOO",         # Penalizzazione Own/Other
  T1     = T1_ENERGY,
  T2     = T2_ENERGY,
  gran   = c(400, 7),
  cv     = "Rolling",
  h      = h_forecast,
  IC     = FALSE,
  verbose = TRUE,
  model.controls = list(
    intercept   = TRUE,
    standardize = TRUE))

res_ENERGY_HOO <- cv.BigVAR(model_ENERGY_HOO)
aligned_ENERGY_HOO <- align_forecasts(res_ENERGY_HOO, Y_mat_ENERGY, T2_ENERGY, T_tot_ENERGY)
sc_ENERGY_HOO     <- score_forecast(aligned_ENERGY_HOO$y_true, aligned_ENERGY_HOO$y_hat)
# Strutture lag per ENERGY
SparsityPlot.BigVAR.results(res_ENERGY_HOO) 


## 4.2.3 Modello HLAGELEM (Elementwise)
model_ENERGY_HE <- constructModel(
  Y      = Y_mat_ENERGY,
  p      = 12,
  struct = "HLAGELEM",       # Penalizzazione Elementwise
  T1     = T1_ENERGY,
  T2     = T2_ENERGY,
  gran   = c(400, 7),
  cv     = "Rolling",
  h      = h_forecast,
  IC     = FALSE,
  verbose = TRUE,
  model.controls = list(
    intercept   = TRUE,
    standardize = TRUE))

res_ENERGY_HE <- cv.BigVAR(model_ENERGY_HE)
aligned_ENERGY_HE <- align_forecasts(res_ENERGY_HE, Y_mat_ENERGY, T2_ENERGY, T_tot_ENERGY)
sc_ENERGY_HE     <- score_forecast(aligned_ENERGY_HE$y_true, aligned_ENERGY_HE$y_hat)
# Strutture lag per ENERGY
SparsityPlot.BigVAR.results(res_ENERGY_HE) 



 






