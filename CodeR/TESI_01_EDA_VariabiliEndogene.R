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




###################################################
#### 1. Importazione e pre-processing dei dati ####
###################################################

### Importo il dataset 
dataUSA <- read.table(
  "https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/refs/heads/main/Data/ConsumiEnergeticiEmissioniSettori_USA_1973-2025.txt",
  header=TRUE, sep = "", stringsAsFactors = FALSE)
dataUSA <- dataUSA %>% dplyr::select(Sector, Source, Month, EmissCO2_MillMetricTons, EnerCons_TrillBTU)
lapply(dataUSA, unique)


### Rinomino le variabili del dataset 
colnames(dataUSA)[colnames(dataUSA) == "EmissCO2_MillMetricTons"] <- "CO2"     # Emissioni di CO2 misurate in milioni di tonnellate metriche
colnames(dataUSA)[colnames(dataUSA) == "EnerCons_TrillBTU"] <- "Energy"        # Energia consumata misurata in Trillion BTU


### Ricostruzione del dataset: rimozione delle fonti energetiche non rilevanti ai fini dell'analisi multivariata
fonti_da_rimuovere <- c("Total Energy",
                        "Total Fossil sources",
                        "Total Renewable Energy")
dataUSA <- dataUSA %>% filter(!Source %in% fonti_da_rimuovere)    # elimino dal dataset le fonti non utilizzate
lapply(dataUSA, unique)


### Rinominazione delle fonti energetiche 
dataUSA <- dataUSA %>%
  mutate(Source = recode(Source, 
                         "Natural Gas Excluding Supplemental Gaseous Fuels" = "NaturalGas",
                         "Motor Gasoline Excluding Ethanol" = "MotorGasoline",
                         "Distillate Fuel Oil" = "FuelOil",
                         "Biomass Energy" = "BiomassEnergy",
                         "Conventional Hydroelectric Power" = "HydroelectricPower",
                         "Wind Energy" = "WindEnergy",
                         "Solar Energy" = "SolarEnergy",
                         "Geothermal Energy" = "GeoEnergy")) %>%
  mutate(Sector = recode(Sector, "Electric Power" = "ElectricPower"))


### Analisi dei valori mancanti per settore e fonte energetica
sum(is.na(dataUSA$CO2))     
sum(is.na(dataUSA$Energy))

# Pivot long del dataset iniziale
data_long <- dataUSA %>% pivot_longer(cols = c(CO2, Energy), names_to = "Metric", values_to = "Value")

# Analisi valori mancanti
valori_mancanti <- data_long %>% filter(is.na(Value))

# Numero mesi unici (per il calcolo in %)
n_mesi <- n_distinct(data_long$Month)
# Percentuale di NA per combinazione Sector + Source + Metric
summary_missing <- valori_mancanti %>%
  group_by(Sector, Source, Metric) %>%
  summarise(MissingCount = n(), .groups = 'drop') %>%
  mutate(MissingPercent = (MissingCount / n_mesi) * 100)

# Separazione: tutti NA vs NA parziali 
solo_na <- summary_missing %>% filter(MissingPercent == 100)
da_imputare <- summary_missing %>% filter(MissingPercent < 100)
# Visualizzo 
print("Combinazioni con TUTTI i valori mancanti (da NON imputare):")
print(solo_na)
# Visualizzo 
print("Combinazioni con valori PARZIALMENTE mancanti (da imputare):")
print(da_imputare)

# Grafico per ogni settore separati per CO2 ed Energy
ggplot(summary_missing, aes(x = Metric, y = reorder(Source, MissingPercent), fill = MissingPercent)) +
  geom_tile(color = "white") +
  facet_wrap(~Sector, scales = "free_y") +
  scale_fill_gradient(low = "white", high = "red", name = "% NA") +
  labs(title = "Heatmap dei valori mancanti per fonte energetica e settore",
    x = "CO2 e Energy",
    y = "Fonte Energetica") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.text.y = element_text(size = 8),
        strip.text = element_text(face = "bold"))




### EMISSIONI C02 & ENERGIA CONSUMATA grafici aggregati per settore ###
#-----------------------------------------------------------------------
# Elenco settori
settori <- unique(dataUSA$Sector)
dataUSA$Month <- ymd(dataUSA$Month)

# Boxplot CO2 per settore
ggplot(na.omit(dataUSA), aes(x = Sector, y = CO2)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Distribuzione delle emissioni di CO2 per settore")

# Boxplot ENERGY per settore
ggplot(na.omit(dataUSA), aes(x = Sector, y = Energy)) +
  geom_boxplot(fill = "lightblue") +
  theme_minimal() +
  labs(title = "Distribuzione dell'energia consumata per settore")



### Correlazione aggregata
cor.test(dataUSA$CO2, dataUSA$Energy) 
# Risultato globale sensato perchè quando aumenta il consumo di energia aumentano anche le emissioni di CO2



### Ciclo sui settori per creare i grafici delle serie storiche sulle emissioni CO2 ed Energia consumata
output_dir <- "Graphs"    # Percorso locale della cartella  dove salvo i grafici
if (!dir.exists(output_dir)) dir.create(output_dir)

for (settore in settori) {
  df_settore <- dataUSA %>% filter(Sector == settore)
  
  # GRAFICO EMISSIONI CO2
  p_CO2 <- ggplot(df_settore, aes(x = Month, y = CO2, color = Source)) +
    geom_line() +
    labs(title = paste("Emissioni di CO2 nel settore:", settore),
         x = "Anno", y = "CO2 (Milioni di tonnellate metriche)") +
    theme_minimal() + theme(legend.position = "bottom")
  
  # GRAFICO ENERGIA CONSUMATA
  p_Energy <- ggplot(df_settore, aes(x = Month, y = Energy, color = Source)) +
    geom_line() +
    labs(title = paste("Energia consumata nel settore:", settore),
         x = "Anno", y = "Energia (Trillion BTU)") +
    theme_minimal() + theme(legend.position = "bottom")
  
  # Salvataggio dei grafici nella cartella "Graphs" 
  ggsave(file.path(output_dir, paste0("CO2_", settore, ".jpg")),
         plot = p_CO2, width = 12, height = 8, dpi = 300)
  ggsave(file.path(output_dir, paste0("Energy_", settore, ".jpg")),
         plot = p_Energy, width = 12, height = 8, dpi = 300)
  
  # Visualizzo i grafici
  print(p_CO2)
  print(p_Energy)}




#####################################################
#### 2. Analisi esplorativa delle serie storiche ####
#####################################################

### Rotazione/pivoting del dataset iniziale
datapivot <- dataUSA %>%
  pivot_wider(names_from = c(Sector, Source),     
              values_from = c(CO2, Energy))  

### Eliminazione delle colonne che hanno valori mancanti NA per tutta la serie storica (per tutti i mesi considerati)
CO2_cols <- grep("^CO2_", names(datapivot), value = TRUE)       # tutte le colonne che iniziano con "CO2_"
datapivot <- datapivot %>%
  dplyr::select(-any_of(CO2_cols[colSums(!is.na(datapivot[CO2_cols])) == 0]))  # rimuovo colonne CO2 completamente NA

ENERGY_cols <- grep("^Energy_", names(datapivot), value = TRUE) # tutte le colonne che iniziano con "Energy_"
datapivot <- datapivot %>%
  dplyr::select(-any_of(ENERGY_cols[colSums(!is.na(datapivot[ENERGY_cols])) == 0]))  # rimuovo colonne Energy completamente NA

cat("Colonne rimaste dopo pulizia:\n")
print(names(datapivot))



### EMISSIONI DI CO2 PER OGNI SETTTORE ###
#------------------------------------------------------
### Data handling/management con Tidyverse
dataCO2 <- datapivot %>%
  mutate(Month = ymd(Month), Date_idx = yearmonth(Month)) %>%
  # Seleziono le colonne di interesse
  dplyr::select(Month, Date_idx, starts_with("CO2")) %>%
  as_tsibble(index = Date_idx)

# Grafico disaggregato per fonte e settore
dataCO2 %>%
  pivot_longer(cols = starts_with("CO2"),
               names_to = "Settore",
               values_to = "CO2") %>%
  ggplot(aes(x = Month, y = CO2, color = Settore)) +
  geom_line() +
  labs(title = "Emissioni di CO2 per settore",
       subtitle = "Fonte: US EIA",
       x = "Anno", y = "Milioni di tonnellate metriche") +
  theme_minimal() +
  theme(legend.position = "bottom")


### Trasformazione in oggetti classe time series per ogni settore: Converto tutte le colonne in un unico oggetto mts
CO2_ts <- dataCO2 %>%
  dplyr::select(starts_with("CO2")) %>%
  ts_ts()



### ENERGIA CONSUMATA PER OGNI SETTTORE ###
#------------------------------------------------------
### Data handling/management con Tidyverse
dataENERGY <- datapivot %>%
  mutate(Month = ymd(Month), Date_idx = yearmonth(Month)) %>%
  # Seleziono le colonne di interesse
  dplyr::select(Month, Date_idx, starts_with("Energy")) %>%
  as_tsibble(index = Date_idx)

# Grafico disaggregato per fonte e settore
dataENERGY %>%
  pivot_longer(cols = starts_with("Energy"),
               names_to = "Settore",
               values_to = "Energy") %>%
  ggplot(aes(x = Month, y = Energy, color = Settore)) +
  geom_line() +
  labs(title = "Emissioni di Energy per settore",
       subtitle = "Fonte: US EIA",
       x = "Anno", y = "Trillion BTU") +
  theme_minimal() +
  theme(legend.position = "bottom")


### Trasformazione in oggetti classe time series per ogni settore: Converto tutte le colonne in un unico oggetto mts
ENERGY_ts <- dataENERGY %>%
  dplyr::select(starts_with("Energy")) %>%
  ts_ts()




###################################################
#### 3. Analisi distribuzione + Test normalità ####  
###################################################

# Importo la funzione
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/Normality.R")

# Porto i dati in formato "long" (una colonna Metric con CO2/Energy e una colonna Value)
data_long <- dataUSA %>%
  dplyr::select(Month, Sector, Source, CO2, Energy) %>%   # seleziono variabili di interesse
  tidyr::pivot_longer(cols = c(CO2, Energy), names_to = "Metric", values_to = "Value") %>%
  dplyr::filter(!is.na(Value))   

settori <- unique(data_long$Sector)   # elenco settori
fonti <- unique(data_long$Source)     # elenco fonti
metriche <- c("CO2", "Energy")        # metriche analizzate

# Salvo grafici istogramma e QQ-plot per ogni combinazione settore / fonte / metrica
for (s in settori) {
  for (f in fonti) {
    for (m in metriche) {
      salva_grafico_distribuzione(data_long, s, f, m)}}
  }

# Eseguo i test di normalità
risultati_normalita <- test_normalita(data_long)
print(risultati_normalita, n = Inf)   # stampo tutti i risultati ottenuti




#####################################################
#### 4. Test stazionarietà per le serie storiche ####
#####################################################

# Importo la funzione per analisi serie storiche (ADF, Ljung-Box, ACF, PACF) 
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/TestStazionarieta.R", encoding = "UTF-8")

# ----- Analisi iniziale CO2 -----
risultati_CO2 <- analisi_serie_ts(tsibble::as_tsibble(dataCO2, index = Date_idx))
tabella_CO2 <- sintesi_risultati_ts(risultati_CO2, "CO2")
print(tabella_CO2, n = Inf)


# ----- Analisi iniziale ENERGY -----
risultati_ENERGY <- analisi_serie_ts(tsibble::as_tsibble(dataENERGY, index = Date_idx))
tabella_ENERGY <- sintesi_risultati_ts(risultati_ENERGY, "ENERGY")
print(tabella_ENERGY, n = Inf)




######################################
#### 5. Stagionalità + filtro HPJ ####
######################################
# serie storiche originali -> TRAMO-SEATS per rimuovere stagionalità -> restituisco il ciclo (serie - trend HPJ)

plot(tramoseats(CO2_ts[,2])$final$series[,"sa"])  # esempio grafico


## Funzione per rimuovere la stagionalità tramite TRAMO-SEATS 
remove_seasonality <- function(y, freq = 12) {     # y: vettore numerico, freq: frequenza della serie (12 = mensile)
  # Trasformo in oggetto ts
  y_ts <- ts(y, frequency = freq)
  # Applico TRAMO-SEATS
  modello <- tryCatch(
    RJDemetra::tramoseats(y_ts, spec = "RSA5c"),  # specifica standard Eurostat
    error = function(e) NULL)                      # se fallisce restituisco NULL
  if (is.null(modello)) {
    # Se TRAMO-SEATS non converge / va in errore,
    # per sicurezza restituisco la serie originale
    return(as.numeric(y))}
  # Estraggo la serie destagionalizzata (colonna "sa")
  sa <- tryCatch(
    modello$final$series[,"sa"],
    error = function(e) y)
  return(as.numeric(sa))
}


## Funzione per calcolare la componente ciclica con HPJ 
## Applico HPJ -> ottengo il ciclo economico
apply_hpj <- function(y){
  v <- as.numeric(y)
  v <- zoo::na.approx(v, na.rm = FALSE)  # interpolo eventuali NA
  fit <- hpj(v)
  cycle <- v - fit$hpj                   # ciclo = y_deseasonal - trend HPJ
  return(cycle)}


## Funzione per calcolare la componente ciclica con TRAMO-SEATS + HPJ
make_hpj_cycle <- function(tsib) {
  tmp <- as_tibble(tsib)
  
  # Colonne da filtrare (escludo l'indice temporale)
  cols <- tmp %>% dplyr::select(-Month, -Date_idx) %>% names()
  tmp[cols] <- lapply(tmp[cols], function(x) {
    v <- as.numeric(x)
    if (all(is.na(v))) return(v)
    # 1) interpolo NA
    v <- zoo::na.approx(v, na.rm = FALSE)
    # 2) destagionalizzo con TRAMO-SEATS (RJDemetra)
    y_deseasonal <- remove_seasonality(v, freq = 12)  # freq=12 perché serie mensili
    # 3) HPJ sulle serie destagionalizzate -> ciclo
    cycle <- apply_hpj(y_deseasonal)
    cycle[is.na(x)] <- NA   # Rimetto NA dove erano NA in origine
    return(cycle)
  })
  tmp %>% as_tsibble(index = Date_idx)
}



# --- Componenti cicliche HPJ per CO2 ---
dataCO2_cycle <- make_hpj_cycle(dataCO2)

dataCO2_cycle %>%
  pivot_longer(cols = starts_with("CO2"),
               names_to = "Serie", values_to = "Valore") %>%  
  #filter( Month >= "2005-01-01" & Month < "2006-01-01") %>%
  ggplot(aes(x = Month, y = Valore, color = Serie)) +
  geom_line() +
  labs(title = "Serie stazionarie - CO2",
       x = "Anno", y = "Deviazione dal trend HPJ") +
  theme_minimal() + theme(legend.position = "bottom")



# --- Componenti cicliche HPJ per ENERGY ---
dataENERGY_cycle <- make_hpj_cycle(dataENERGY)

dataENERGY_cycle %>%
  pivot_longer(cols = starts_with("Energy"),
               names_to = "Serie", values_to = "Valore") %>%
  #filter( Month >= "2007-01-01" & Month < "2010-01-01") %>%
  ggplot(aes(x = Month, y = Valore, color = Serie)) +
  geom_line() +
  labs(title = "Serie stazionarie - ENERGY",
       x = "Anno", y = "Deviazione dal trend HPJ") +
  theme_minimal() + theme(legend.position = "bottom")




##########################################################
#### 5bis. Test stazionarietà sulle serie trasformate ####
##########################################################

# CO2: componenti cicliche
risultati_CO2_cycle <- analisi_serie_ts(tsibble::as_tsibble(dataCO2_cycle, index = Date_idx))
tabella_CO2_cycle <- sintesi_risultati_ts(risultati_CO2_cycle, "CO2_cycle")
print(tabella_CO2_cycle, n = Inf)

# ENERGY: componenti cicliche
risultati_ENERGY_cycle <- analisi_serie_ts(tsibble::as_tsibble(dataENERGY_cycle, index = Date_idx))
tabella_ENERGY_cycle <- sintesi_risultati_ts(risultati_ENERGY_cycle, "ENERGY_cycle")
print(tabella_ENERGY_cycle, n = Inf)




################################################
#### 6. Salvataggio dei dati pre-processati ####
################################################

dataCO2_final <- as.data.frame(dataCO2_cycle)
dataENERGY_final <- as.data.frame(dataENERGY_cycle)


saveRDS(dataCO2_final,   "dataCO2_final.rds")
saveRDS(dataENERGY_final, "dataENERGY_final.rds")

write.csv(dataCO2_final, "dataCO2_final.csv", row.names = FALSE)
write.csv(dataENERGY_final, "dataENERGY_final.csv", row.names = FALSE)






