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
url1 <- "https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Data/dataCO2_agg.rds"
url2 <- "https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Data/dataENERGY_agg.rds"
# Percorsi locali temporanei GitHub
tmp_file1 <- tempfile(fileext = ".rds")
tmp_file2 <- tempfile(fileext = ".rds")
# Scarico i file
download.file(url1, destfile = tmp_file1, mode = "wb")
download.file(url2, destfile = tmp_file2, mode = "wb")
# Leggo i file RDS
dataCO2_agg <- readRDS(tmp_file1)
dataENERGY_agg <- readRDS(tmp_file2)



#----------------------
## VARIABILI ESOGENE ##
#----------------------
### Caricamento dei dati pre-processati precedentemente
url3 <- "https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Data/dataCO2_varx.rds"
url4 <- "https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Data/dataENERGY_varx.rds"
# Percorsi locali temporanei GitHub
tmp_file3 <- tempfile(fileext = ".rds")
tmp_file4 <- tempfile(fileext = ".rds")
# Scarico i file
download.file(url3, destfile = tmp_file3, mode = "wb")
download.file(url4, destfile = tmp_file4, mode = "wb")
# Leggo i file RDS
dataCO2_varx <- readRDS(tmp_file3)
dataENERGY_varx <- readRDS(tmp_file4)




################################################
#### 2. Preparazione dati per VARX e VARX-L #### 
################################################

## Seleziono le variabili ENDOGENE (Y)
Y_CO2 <- dataCO2_varx %>% as_tibble() %>% dplyr::select(starts_with("Fossile"), starts_with("Rinnovabile"))   
Y_mat_CO2 <- as.matrix(Y_CO2)

Y_EN <- dataENERGY_varx %>% as_tibble() %>% dplyr::select(starts_with("Fossile"), starts_with("Rinnovabile"))  
Y_mat_EN <- as.matrix(Y_EN)

## Seleziono le variabili ESOGENE (X)
X <- dataCO2_varx %>%   # Le esogene sono le stesse sia per CO2 che ENERGY
  as_tibble() %>% dplyr::select(GDP_cycle, IPI_cycle, NewHouses_cycle, Occupation_cycle, Pop_cycle)


## Converto in oggetti per VARX
start_year  <- year(min(dataCO2_varx$Month))
start_month <- month(min(dataCO2_varx$Month))
start_year1  <- year(min(dataENERGY_varx$Month))
start_month1 <- month(min(dataENERGY_varx$Month))

Y_CO2_ts  <- ts(as.matrix(Y_CO2), start = c(start_year, start_month), frequency = 12)
Y_EN_ts  <- ts(as.matrix(Y_EN), start = c(start_year1, start_month1), frequency = 12)
X_mat <- as.matrix(X)


## Funzione di valutazione forecast (RMSE, MAE, MSFE)
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/ScoreForecast.R")




###############################
#### 3. Stima modello VARX ####
###############################

### --- VARX (CO2) --- ### 
T_tot_CO2_varx <- nrow(Y_mat_CO2)
T1_CO2_varx    <- floor(0.7 * T_tot_CO2_varx)   # fine training / inizio CV
T2_CO2_varx    <- floor(0.9 * T_tot_CO2_varx)   # fine CV / inizio test

# Split training / test
Y_train_CO2 <- Y_mat_CO2[1:T2_CO2_varx, , drop = FALSE]
Y_test_CO2  <- Y_mat_CO2[(T2_CO2_varx + 1):T_tot_CO2_varx, , drop = FALSE]

X_train_CO2 <- X_mat[1:T2_CO2_varx, , drop = FALSE]
X_test_CO2  <- X_mat[(T2_CO2_varx + 1):T_tot_CO2_varx, , drop = FALSE]


## Stima VARX finale su 1:T2_CO2_varx
p_use_VARX_CO2 <- 6   
VARX_final_CO2 <- VAR(
  y      = Y_train_CO2,
  p      = p_use_VARX_CO2,      
  type   = "const",
  exogen = X_train_CO2)
summary(VARX_final_CO2)

## Rolling / expanding forecast nel periodo di test
n_test_CO2 <- T_tot_CO2_varx - T2_CO2_varx
# Matrice delle previsioni VARX per il test
preds_VARX_CO2 <- matrix(NA, n_test_CO2, ncol(Y_mat_CO2))
colnames(preds_VARX_CO2) <- colnames(Y_mat_CO2)

# Valori veri nel test (come nel VAR)
true_CO2 <- Y_mat_CO2[(T2_CO2_varx + 1):T_tot_CO2_varx, ]
colnames(true_CO2) <- colnames(Y_mat_CO2)

for (i in 1:n_test_CO2) {
  # dati disponibili fino al tempo T2_CO2 + i - 1 (expanding window)
  Y_tr <- Y_mat_CO2[1:(T2_CO2_varx + i - 1), , drop = FALSE]
  X_tr <- X_mat[1:(T2_CO2_varx + i - 1), , drop = FALSE]
  # stimo il VARX_i
  VARX_i <- VAR(
    y      = Y_tr,
    p      = p_use_VARX_CO2,
    type   = "const",
    exogen = X_tr)
  # Esogene 
  X_new <- X_mat[T2_CO2_varx + i, , drop = FALSE]   
  # previsione 1-step-ahead con le esogene corrispondenti
  fc_i <- predict(
    VARX_i, n.ahead = 1, dumvar  = X_new)$fcst
  preds_VARX_CO2[i, ] <- sapply(fc_i, function(x) x[1])}
# Score del VARX sul periodo di test
sc_VARX_CO2_test <- score_forecast(true_CO2, preds_VARX_CO2)




### --- VARX (ENERGY) --- ### 
T_tot_ENERGY_varx <- nrow(Y_mat_EN)
T1_ENERGY_varx    <- floor(0.7 * T_tot_ENERGY_varx)
T2_ENERGY_varx    <- floor(0.9 * T_tot_ENERGY_varx)

# Split training / test (stessa logica del VAR)
Y_train_ENERGY <- Y_mat_EN[1:T2_ENERGY_varx, , drop = FALSE]
Y_test_ENERGY  <- Y_mat_EN[(T2_ENERGY_varx + 1):T_tot_ENERGY_varx, , drop = FALSE]

X_train_ENERGY <- X_mat[1:T2_ENERGY_varx, , drop = FALSE]
X_test_ENERGY  <- X_mat[(T2_ENERGY_varx + 1):T_tot_ENERGY_varx, , drop = FALSE]


# Stima VARX finale su 1:T2_ENERGY_varx 
p_use_VARX_ENERGY <- 6
VARX_final_ENERGY <- VAR(
  Y_train_ENERGY,
  p      = p_use_VARX_ENERGY,
  type   = "const",
  exogen = X_train_ENERGY)
summary(VARX_final_ENERGY)

# Rolling / expanding forecast nel test con VARX
n_test_ENERGY_varx <- T_tot_ENERGY_varx - T2_ENERGY_varx
preds_VARX_ENERGY <- matrix(NA, n_test_ENERGY_varx, ncol(Y_mat_ENERGY))
colnames(preds_VARX_ENERGY) <- colnames(Y_mat_ENERGY)
true_ENERGY_varx <- Y_test_ENERGY   

for (i in 1:n_test_ENERGY_varx) {
  # Dati fino a T2_ENERGY_varx + i - 1
  Y_tr <- Y_mat_ENERGY[1:(T2_ENERGY_varx + i - 1), , drop = FALSE]
  X_tr <- X_mat[1:(T2_ENERGY_varx + i - 1), , drop = FALSE]
  # Stimo il VARX
  VARX_i <- VAR(
    Y_tr,
    p      = p_use_VARX_ENERGY,
    type   = "const",
    exogen = X_tr)
  # Esogene 
  X_new <- X_mat[T2_ENERGY_varx + i, , drop = FALSE]
  # Previsione 1-step-ahead con esogene
  fc_i <- predict(
    VARX_i, n.ahead = 1, dumvar  = X_new)$fcst
  preds_VARX_ENERGY[i, ] <- sapply(fc_i, function(x) x[1])}
# Score del VARX nel periodo di test
sc_VARX_ENERGY_test <- score_forecast(true_ENERGY_varx, preds_VARX_ENERGY)




###############################################
#### 4. VARX-L (VARX penalizzato - BigVAR) ####         
###############################################

### --- VARX-L per il sistema CO2 --- ###
# Matrice completa CO2 + esogene
Y_CO2_VARX <- cbind(Y_mat_CO2, X_mat)

# Numero di variabili endogene (CO2) e lag esogeni massimi
k_CO2 <- ncol(Y_mat_CO2)
VARX_CO2_spec <- list(
  k       = k_CO2,       # prime k colonne di Y_CO2_VARX sono modellate
  s       = 6,          # massimo ordine di lag per le esogene 
  contemp = FALSE)     
p_max <- 12   # massimo ordine di lag endogeno (coerente con HVAR)


# Costruisco il modello VARX-L 
model_CO2_VARXL <- constructModel(
  Y      = Y_CO2_VARX,
  p      = p_max,
  struct = "Lag",                    # struttura VARX-L: Lag, OwnOther, SparseLag, SparseOwnOther, ...
  gran   = c(50, 10),              # profondità e numero punti griglia di lambda
  VARX   = VARX_CO2_spec,    
  T1     = T1_CO2_varx,              # inizio periodo di rolling CV
  T2     = T2_CO2_varx,               # fine periodo di rolling CV
  h      = 1,                         # orizzonte 1-step-ahead per la CV
  cv     = "Rolling",
  IC     = FALSE,
  verbose = TRUE,
  model.controls = list(
    intercept   = TRUE,
    standardize = TRUE))
# Selezione automatica di lambda via rolling CV + valutazione out-of-sample
res_CO2_VARXL <- cv.BigVAR(model_CO2_VARXL)
# Visualizza il pattern di sparsità dei coefficienti
SparsityPlot.BigVAR.results(res_CO2_VARXL)
# Lambda ottimale e frazione di coefficienti attivi
(lambda_opt_CO2_VARXL <- res_CO2_VARXL@OptimalLambda)

# Previsioni out-of-sample (T2+1 : T) per le sole serie endogene (prime k colonne)
preds_CO2_VARXL <- res_CO2_VARXL@preds    
# Valori veri nel periodo di test 
true_CO2_VARXL <- Y_mat_CO2[(T2_CO2_varx + 1):T_tot_CO2_varx, , drop = FALSE]
# Score forecast VARX-L sul periodo di test
sc_VARXL_CO2_test <- score_forecast(true_CO2_VARXL, preds_CO2_VARXL)



### CONFRONTO MODELLI ### 
# Confronto grafico
var_target <- "Rinnovabile_Transportation" 

df_plot_comp <- data.frame(
  Month  = dataCO2_varx$Month[(T2_CO2_varx + 1):T_tot_CO2_varx],
  SerieStorica = as.numeric(true_CO2_VARXL[, var_target]),
  VAR = as.numeric(preds_CO2[, var_target]),
  VARX = as.numeric(preds_VARX_CO2[, var_target]),
  HVAR = as.numeric(aligned_HC$y_hat[, var_target]),     # versione componentwise
  VARXL = as.numeric(preds_CO2_VARXL[, var_target])
) %>% pivot_longer(-Month, names_to = "Modello", values_to = "Valore")

# Grafico finale 
ggplot(df_plot_comp, aes(x = Month, y = Valore, color = Modello, linetype = Modello)) +
  geom_line(aes(linewidth = ifelse(Modello == "SerieStorica", 0.9, 1.0)), alpha = 0.9) +
  scale_color_manual(values = c("SerieStorica" = "black", "VAR" = "orange", "VARX" = "red", "HVAR" = "green","VARXL" = "purple")) +
  scale_linetype_manual(values = c(
    "SerieStorica" = "solid","VAR" = "dashed", "VARX" = "dashed", "HVAR" = "solid", "VARXL" = "solid")) +
  scale_linewidth_identity() + guides(color = guide_legend(override.aes = list(linewidth = 1.4))) + 
  labs(title = paste("Confronto Performance:", var_target), subtitle = "Serie storica vs Modelli", x = "Mese", y = "Ciclo") + theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())


# Unione di tutti gli score (già calcolati in precedenza)
confronto_completo <- sc_VAR_CO2_test %>%
  dplyr::select(Serie, MSFE) %>% dplyr::rename(MSFE_VAR = MSFE) %>%
  left_join(sc_CO2_HC      %>% dplyr::select(Serie, MSFE) %>% rename(MSFE_HVAR = MSFE), by = "Serie") %>%   # Componentwise
  left_join(sc_VARX_CO2_test %>% dplyr::select(Serie, MSFE) %>% rename(MSFE_VARX = MSFE), by = "Serie") %>%
  left_join(sc_VARXL_CO2_test %>% dplyr::select(Serie, MSFE) %>% rename(MSFE_VARXL = MSFE), by = "Serie")

# Aggiungo una colonna che indica il modello vincitore per ogni serie
confronto_completo$Winner <- colnames(confronto_completo[, c("MSFE_VAR", "MSFE_HVAR", "MSFE_VARX", "MSFE_VARXL")])[
  apply(confronto_completo[, c("MSFE_VAR", "MSFE_HVAR", "MSFE_VARX", "MSFE_VARXL")], 1, which.min)]
options(scipen = 999)
print(confronto_completo)




### --- VARX-L per il sistema ENERGY --- ###
# Matrice completa ENERGY + esogene
Y_EN_VARX <- cbind(Y_mat_EN, X_mat)   

k_EN <- ncol(Y_mat_EN)
VARX_EN_spec <- list(
  k       = k_EN,
  s       = 6,
  contemp = FALSE)

# Costruisco il modello VARX-L 
model_EN_VARXL <- constructModel(
  Y      = Y_EN_VARX,
  p      = p_max,
  struct = "Lag",
  gran   = c(50, 10),
  VARX   = VARX_EN_spec,
  T1     = T1_ENERGY_varx,
  T2     = T2_ENERGY_varx,
  h      = 1,
  cv     = "Rolling",
  IC     = FALSE,
  verbose = TRUE,
  model.controls = list(
    intercept   = TRUE,
    standardize = TRUE))
# Selezione automatica di lambda via rolling CV + valutazione out-of-sample
res_EN_VARXL <- cv.BigVAR(model_EN_VARXL)
# Visualizza il pattern di sparsità dei coefficienti
SparsityPlot.BigVAR.results(res_EN_VARXL)
# Lambda ottimale e frazione di coefficienti attivi
(lambda_opt_EN_VARXL <- res_EN_VARXL@OptimalLambda)

# Previsioni out-of-sample (T2+1 : T) per le sole serie endogene (prime k colonne)
preds_EN_VARXL <- res_EN_VARXL@preds   
# Valori veri nel periodo di test 
true_EN_VARXL <- Y_mat_EN[(T2_ENERGY_varx + 1):T_tot_ENERGY_varx, , drop = FALSE]
# Score forecast VARX-L sul periodo di test
sc_VARXL_EN_test <- score_forecast(true_EN_VARXL, preds_EN_VARXL)



### CONFRONTO MODELLI ### 
# Confronto grafico
var_target <- "Rinnovabile_Transportation" 

df_plot_comp <- data.frame(
  Month  = dataENERGY_varx$Month[(T2_ENERGY_varx + 1):T_tot_ENERGY_varx],
  SerieStorica = as.numeric(true_EN_VARXL[, var_target]),
  VAR = as.numeric(preds_ENERGY[, var_target]),
  VARX = as.numeric(preds_VARX_ENERGY[, var_target]),
  HVAR = as.numeric(aligned_ENERGY_HC$y_hat[, var_target]),     # versione componentwise
  VARXL = as.numeric(preds_EN_VARXL[, var_target])
) %>% pivot_longer(-Month, names_to = "Modello", values_to = "Valore")

# Grafico finale 
ggplot(df_plot_comp, aes(x = Month, y = Valore, color = Modello, linetype = Modello)) +
  geom_line(aes(linewidth = ifelse(Modello == "SerieStorica", 0.9, 1.0)), alpha = 0.9) +
  scale_color_manual(values = c("SerieStorica" = "black", "VAR" = "orange", "VARX" = "red", "HVAR" = "green","VARXL" = "purple")) +
  scale_linetype_manual(values = c(
    "SerieStorica" = "solid","VAR" = "dashed", "VARX" = "dashed", "HVAR" = "solid", "VARXL" = "solid")) +
  scale_linewidth_identity() + guides(color = guide_legend(override.aes = list(linewidth = 1.4))) + 
  labs(title = paste("Confronto Performance:", var_target), subtitle = "Serie storica vs Modelli", x = "Mese", y = "Ciclo") + theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())


# Unione di tutti gli score (già calcolati in precedenza)
confronto_completo <- sc_VAR_ENERGY_test %>%
  dplyr::select(Serie, MSFE) %>% dplyr::rename(MSFE_VAR = MSFE) %>%
  left_join(sc_ENERGY_HC      %>% dplyr::select(Serie, MSFE) %>% rename(MSFE_HVAR = MSFE), by = "Serie") %>%
  left_join(sc_VARX_ENERGY_test %>% dplyr::select(Serie, MSFE) %>% rename(MSFE_VARX = MSFE), by = "Serie") %>%
  left_join(sc_VARXL_EN_test %>% dplyr::select(Serie, MSFE) %>% rename(MSFE_VARXL = MSFE), by = "Serie")

# Aggiungo una colonna che indica il modello vincitore per ogni serie
confronto_completo$Winner <- colnames(confronto_completo[, c("MSFE_VAR", "MSFE_HVAR", "MSFE_VARX", "MSFE_VARXL")])[
  apply(confronto_completo[, c("MSFE_VAR", "MSFE_HVAR", "MSFE_VARX", "MSFE_VARXL")], 1, which.min)]
options(scipen = 999)
print(confronto_completo)











