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
library(purrr)
library(stringr)
library(ggh4x)  
library(grid)
library(purrr)




#############################################################
#### 1. IRF (Impulse Response Functions) non penalizzata #### 
#############################################################

## Funzione per stimare Impulse Response Analysis 
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/IRF.R")

# Si assume che i consumi energetici nei vari settori influenzano le emissioni di CO2, ma non viceversa 
Y_joint <- cbind(Y_ENERGY, Y_CO2)     # Costruisco un dataframe totale ENERGY e CO2
# Per chiarezza nei nomi delle colonne
colnames(Y_joint) <- c(paste0("EN_", colnames(Y_ENERGY)), paste0("CO2_", colnames(Y_CO2)))
Y_mat_joint <- as.matrix(Y_joint)   # trasformo in matrice numerica

# Nuovi indici temporali per questo sistema congiunto
T_tot_joint <- nrow(Y_mat_joint)
T1_joint <- floor(0.7 * T_tot_joint)   # fine training / inizio rolling CV
T2_joint <- floor(0.9 * T_tot_joint)   # fine CV / inizio test


# Selezione lag ottimale per il VAR congiunto ENERGY+CO2 tramite rolling out-of-sample 
results_joint <- run_VAR_rolling(Y_mat_joint[1:T2_joint, ], T1_joint, max_p = 12)
# Per ogni p calcolo l'RMSE medio e scelgo il lag che lo minimizza
best_p_joint <- results_joint %>% group_by(p) %>% summarise(RMSE_media = mean(RMSE, na.rm = TRUE)) %>%
  slice_min(RMSE_media) %>% pull(p)
(p_use_joint <- best_p_joint)     # lag ottimale selezionato sulla base della previsione OOS


## Stima del VAR finale 
VAR_final_joint <- VAR(
  Y_mat_joint,  # uso tutti i dati IRF 
  p    = p_use_joint,         # lag scelto in precedenza
  type = "const")              # includo l'intercetta nel modello VAR



## IRF VAR non penalizzato: ENERGY -> CO2 (con bootstrap e decomposizione di Cholesky) 
# Lista variabili Energy e CO2
sectors <- c("Fossile_Industrial","Fossile_Commercial","Fossile_Residential",
             "Fossile_ElectricPower","Fossile_Transportation",
             "Rinnovabile_Industrial","Rinnovabile_Commercial","Rinnovabile_Residential",
             "Rinnovabile_ElectricPower","Rinnovabile_Transportation")

impulses  <- paste0("EN_",  sectors)     # impulse: consumi energetici
responses <- paste0("CO2_", sectors)     # response: emissioni di CO2


# IRF VAR (bootstrap + Cholesky) EN -> CO2
set.seed(123)
irf_all <- irf(
  VAR_final_joint,                  # modello VAR stimato sul sistema congiunto ENERGY + CO2
  impulse = impulses,                    # elenco delle variabili che ricevono lo shock (Energy)
  response = responses,                  # elenco delle variabili che rispondono (CO2)
  n.ahead = 24,                          # orizzonte di risposta: 24 mesi
  boot = TRUE,                           
  runs = 1000,                           # numero di repliche bootstrap
  ci = 0.95)                           ì

# Orizzonte temporale (0,1,...,H) 
H <- 0:(nrow(irf_all$irf[[1]]) - 1)


# IRF -> dataframe lungo (tutte le coppie)
one_impulse_to_df <- function(imp_name) {
  irf_mat  <- irf_all$irf[[imp_name]]        # matrice IRF: righe = H, colonne = responses
  low_mat  <- irf_all$Lower[[imp_name]]   
  high_mat <- irf_all$Upper[[imp_name]]     
  
  as_tibble(irf_mat) %>%
    mutate(H = H) %>% pivot_longer(-H, names_to = "response", values_to = "IRF") %>%
    left_join(as_tibble(low_mat) %>% mutate(H = H) %>% pivot_longer(-H, names_to = "response", values_to = "low"),
      by = c("H","response")) %>% left_join(as_tibble(high_mat) %>% mutate(H = H) %>% pivot_longer(-H, names_to = "response", values_to = "high"),
      by = c("H","response")) %>% mutate(impulse = imp_name)
}

# Applico la funzione a tutti gli impulsi e combino in un unico dataframe
IRF_df <- map_dfr(names(irf_all$irf), one_impulse_to_df) %>%
  mutate(EN  = str_remove(impulse,  "^EN_"), CO2 = str_remove(response, "^CO2_"))


# Etichette BREVI 
lab_map <- c(
  "Fossile_Industrial" = "F_Ind", "Fossile_Commercial" = "F_Com", "Fossile_Residential" = "F_Res", "Fossile_ElectricPower" = "F_ElecP",
  "Fossile_Transportation" = "F_Transp", "Rinnovabile_Industrial" = "R_Ind", "Rinnovabile_Commercial" = "R_Com",
  "Rinnovabile_Residential" = "R_Res", "Rinnovabile_ElectricPower"  = "R_ElecP", "Rinnovabile_Transportation" = "R_Transp")
lvl <- unname(lab_map)
# Aggiungo fattori con etichette brevi
IRF_df <- IRF_df %>% mutate(EN_lab  = factor(unname(lab_map[EN]),  levels = lvl), CO2_lab = factor(unname(lab_map[CO2]), levels = lvl))


## PLOT 1) SOLO combinazioni ESATTE
IRF_df_pair <- IRF_df %>% filter(EN == CO2)

p_exact <- ggplot(IRF_df_pair, aes(x = H, y = IRF)) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.22, fill = "orange") +
  geom_line(linewidth = 0.9) + geom_hline(yintercept = 0, linewidth = 0.5) +
  facet_wrap(~ EN_lab, scales = "free_y", ncol = 2) +
  labs(title = "Combinazioni esatte: Energy -> CO2 (bande bootstrap 95%)", x = "Orizzonte (mesi)", y = "IRF modello VAR") +
  theme_bw(base_size = 12) +
  theme(plot.title = element_text(face = "bold"), strip.text = element_text(face = "bold", size = 11), panel.grid.minor = element_blank())
p_exact


## PLOT 2) TUTTE le combinazioni
p_all <- ggplot(IRF_df, aes(x = H, y = IRF)) + geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.25, fill = "orange") +
  geom_line(linewidth = 0.7) + geom_hline(yintercept = 0, linewidth = 0.4) +
  ggh4x::facet_grid2(
    rows = vars(EN_lab),       # impulsi sulle righe
    cols = vars(CO2_lab),       # risposte sulle colonne
    scales = "free_y",         # scala y libera
    independent = "y",         # scala y indipendente per ogni pannello della figura
    switch = "y") +                                                         
  labs(title = "Tutte le combinazioni: Energy -> CO2", x = "Orizzonte (mesi)", y = "IRF modello VAR") +
  
  theme_bw(base_size = 11) +
  theme(plot.title = element_text(face = "bold", size = 15, hjust = 0.5), plot.subtitle = element_text(size = 11, hjust = 0.5),
    strip.text.y.left = element_text(size = 10, face = "bold"), strip.text.x = element_text(size = 10, face = "bold", angle = 0),
    strip.background = element_rect(fill = "grey95", color = "grey60"), strip.placement  = "outside",
    panel.spacing    = unit(0.25, "lines"), axis.text.x = element_text(size = 6), axis.text.y = element_text(size = 6),
    panel.grid.minor = element_blank())
p_all








