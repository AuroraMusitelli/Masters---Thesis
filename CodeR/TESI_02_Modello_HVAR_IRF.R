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




## Importo le funzioni
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/MBB_length_opt.R")
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/BlockWildParametricBootstrap.R")
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/Make_lag_matrices.R")
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/IRF_from_Phi.R")




############################################################
#### Stima HVAR con BigVAR ed estrazione residui + IRF #####  
############################################################

## Modello HVAR congiunto ENERGY+CO2 
p_lag <- 12
Y     <- Y_mat_joint
K     <- ncol(Y)   # numero variabili del sistema

model_joint_HC <- constructModel(
  Y      = Y_mat_joint,
  p      = 12,
  struct = "HLAGC",     # Componentwise 
  T1     = T1_joint,
  T2     = T2_joint,
  gran   = c(400, 7),
  cv     = "Rolling",
  h      = 1,     
  IC     = FALSE,
  verbose = TRUE,
  model.controls = list(
    intercept   = TRUE,
    standardize = TRUE))
res_joint_HC <- cv.BigVAR(model_joint_HC)
lambda_opt_joint <- res_joint_HC@OptimalLambda     # Lambda ottimale scelto da BigVAR per il sistema congiunto

# Estrazione dei coefficienti HVAR e costruzione di fitted e residui
Bhat      <- as.matrix(coef(res_joint_HC))  # estraggo i coeff. da HVAR  K x (1 + K*p)
intercept <- Bhat[, 1]                      # intercette (K x 1)
Beta      <- Bhat[, -1, drop = FALSE]       # matrice con coeff. dei lag (K x K*p)
Tobs  <- nrow(Y)    # numero tot osservazioni
T_eff <- Tobs - p_lag   # numero di osservazioni utilizzabili nel VAR(p), perché i primi p_lag punti servono solo come lag

# Costruisco la matrice dei regressori laggati e le corrispondenti Y_used
lag_orig <- make_lag_matrices(Y, p_lag)
Z        <- lag_orig$Z       # Z_t = (Y_{t-1},...,Y_{t-p})
Y_used   <- lag_orig$Y_used  # Y_t = osservazioni effettive 

# Valori stimati (fitted) di HVAR: Y_hat_t = c + B * Z_t
Y_hat <- matrix(0, T_eff, K)
for (t in 1:T_eff) {Y_hat[t, ] <- as.numeric(intercept + Beta %*% matrix(Z[t, ], ncol = 1))}

# Residui di HVAR e matrice di covarianza residui
E_hat  <- Y_used - Y_hat       # (T_eff x K)
SigmaU <- cov(E_hat)           # (K x K)   serve per decomposizione di Cholesky e IRF ortogonalizzate




###################
## IRF BOOTSTRAP ##
###################
H_irf   <- 24                 
IRF_hat <- IRF_from_Phi(Beta, SigmaU, H_irf, K, p_lag)  # IRF PUNTUALI (senza bootstrap)

B       <- 1000      
L_block <- 5         
Wild    <- TRUE
set.seed(1234)

# Generazione delle serie bootstrap (residui + fitted)
boot_data <- Res_Block_Wild_BootGenerator(
  e     = E_hat,      # prendo residui E_hat e fitted Y_hat del modello originale
  nsim  = B,
  yhat  = Y_hat,
  L     = L_block,
  Wild  = Wild,      
  Seed  = 1234)

Y_boot   <- boot_data$y_b             # Estraggo solo le serie bootstrap -> array Y*_t = Y_hat + e*_t
IRF_boot <- array(0, dim = c(H_irf, K, K, B))    # Array che conterrà tutte le IRF bootstrap per ogni replica b

# Loop sulle repliche bootstrap b
for (b in 1:B) {
  cat("Bootstrap", b, "di", B, "\n")
  # Ricostruisco una serie completa usando le prime p osservazioni originali
  Y_b <- Y      
  Y_b[(p_lag + 1):Tobs, ] <- Y_boot[, , b]  # mantengo  le prime p osservazioni originali e poi rimpiazzo il resto con la serie bootstrap
  # Ristimo HVAR sulle serie bootstrap con lambda ottimo trovato prima
  fit_b <- BigVAR.fit(
    Y         = Y_b,
    p         = p_lag,
    struct    = "HLAGC",
    lambda    = lambda_opt_joint,
    intercept = TRUE)
  # Coefficienti del modello bootstrap b
  B_b        <- fit_b[, , 1]    # matrice K × (1+Kp)
  intercept_b <- B_b[, 1]                   # intercette bootstrap
  Beta_b      <- B_b[, -1, drop = FALSE]     # coefficienti dei lag bootstrap
  # Ricostruisco Z_b e Y_used_b per bootstrap con la funzione ausiliaria
  lag_b    <- make_lag_matrices(Y_b, p_lag)
  Z_b      <- lag_b$Z
  Y_used_b <- lag_b$Y_used
  # Fitted Y^_t e residui per la replica b
  Y_hat_b <- matrix(0, T_eff, K)
  for (t in 1:T_eff) {
    Y_hat_b[t, ] <- as.numeric(intercept_b + Beta_b %*% matrix(Z_b[t, ], ncol = 1))
  }
  E_b      <- Y_used_b - Y_hat_b   # residui della replica b
  SigmaU_b <- cov(E_b)   # matrice di covarianza della replica b
  # IRF bootstrap per replica b 
  IRF_boot[,,, b] <- IRF_from_Phi(Beta_b, SigmaU_b, H_irf, K, p_lag)}
# !!Ottengo IRF_boot: array H × K × K × B (orizzonte × risposta × shock × replica) con tutte le IRF bootstrap!!




######################################### 
#### Bande di confidenza + grafici  #####
#########################################

# 1) Orizzonte coerente con IRF_hat/IRF_boot 
H_irf <- dim(IRF_hat)[1]
H_vec <- 1:H_irf


# 2) Bande bootstrap 
# A) Percentile 
IRF_low_q  <- apply(IRF_boot, c(1,2,3), quantile, probs = 0.025, na.rm = TRUE)
IRF_high_q <- apply(IRF_boot, c(1,2,3), quantile, probs = 0.975, na.rm = TRUE)

# B) Simmetriche
IRF_sd <- apply(IRF_boot, c(1,2,3), sd, na.rm = TRUE)
z <- qnorm(0.975)
IRF_low_s  <- IRF_hat - z * IRF_sd
IRF_high_s <- IRF_hat + z * IRF_sd

USE_SYMMETRIC <- TRUE  
IRF_low  <- if (USE_SYMMETRIC) IRF_low_s  else IRF_low_q
IRF_high <- if (USE_SYMMETRIC) IRF_high_s else IRF_high_q


# 3) Griglia EN -> CO2 
var_names <- colnames(Y)
idx_EN  <- 1:10
idx_CO2 <- 11:20

Grid <- expand.grid(SectorInput  = var_names[idx_EN], SectorOutput = var_names[idx_CO2], stringsAsFactors = FALSE)


# 4) Costruzione IRF_df  
IRF_list <- vector("list", nrow(Grid))
for (i in 1:nrow(Grid)) {
  shock <- match(Grid$SectorInput[i],  var_names)
  resp  <- match(Grid$SectorOutput[i], var_names)
  
  IRF_list[[i]] <- data.frame(
    SectorInput  = Grid$SectorInput[i],
    SectorOutput = Grid$SectorOutput[i],
    H      = H_vec,
    IRF_m  = IRF_hat[,  resp, shock],
    IRF_lb = IRF_low[,  resp, shock],
    IRF_up = IRF_high[, resp, shock])} 

IRF_df <- bind_rows(IRF_list) %>%
  mutate(IRF_lb = pmin(IRF_lb, IRF_up), IRF_up = pmax(IRF_lb, IRF_up),
    key_in  = sub("^EN_",  "", SectorInput), key_out = sub("^CO2_", "", SectorOutput))   # chiavi per combinazioni esatte



##########################################
#### (1) GRAFICO: combinazioni esatte ####
##########################################
IRF_exact <- IRF_df %>% filter(key_in == key_out)
p_exact_HVAR <- IRF_exact %>% ggplot(aes(x = H, y = IRF_m)) +
  geom_ribbon(aes(ymin = IRF_lb, ymax = IRF_up), alpha = 0.45, fill = "green") + geom_line(linewidth = 0.7, color = "black") +
  geom_hline(yintercept = 0, linewidth = 0.4, color = "black") + facet_wrap(~ key_in, scales = "free_y", ncol = 2) +
  labs(title = if (!USE_SYMMETRIC)
      "Combinazioni esatte Energy -> CO2 (bande 95%)"
    else
      "Combinazioni esatte Energy -> CO2 (bande 95%)",
    x = "Orizzonte (mesi)", y = "IRF modello HVAR") + theme_bw(base_size = 11) +
  theme(plot.title  = element_text(face = "bold", size = 14, hjust = 0.5), strip.text  = element_text(size = 9, face = "bold"), 
    axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7), panel.grid.minor = element_blank())
p_exact_HVAR



#############################################
#### (2) GRAFICO: tutte le combinazioni #####
#############################################
p_all_HVAR <- IRF_df %>% ggplot(aes(x = H, y = IRF_m)) +
  geom_ribbon(aes(ymin = IRF_lb, ymax = IRF_up), alpha = 0.45, fill  = "green") +
  geom_line(linewidth = 0.70) + geom_hline(yintercept = 0, linewidth = 0.4, color = "black") +
  ggh4x::facet_grid2(
    rows = vars(SectorInput), cols = vars(SectorOutput), scales = "free_y", independent = "y", switch = "y",
    labeller = labeller(SectorInput  = lab_map, SectorOutput = lab_map)) +
  labs(title = "Tutte le combinazioni: Energy -> CO2 (bande 95%)", x = "Orizzonte (mesi)", y = "IRF modello HVAR") +
  theme_bw(base_size = 11) + theme(
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    strip.text.y.left = element_text(size = 9, face = "bold"), strip.text.x = element_text(size = 9, face = "bold", angle = 0),
    strip.background = element_rect(fill = "grey95", color = "grey60"), strip.placement = "outside",
    panel.spacing = unit(0.25, "lines"), axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6),
    panel.grid.minor = element_blank())
p_all_HVAR


 



