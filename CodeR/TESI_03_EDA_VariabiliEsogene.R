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
## Importo funzione per i grafici
source_url("https://raw.githubusercontent.com/AuroraMusitelli/Masters---Thesis/main/Functions/PlotGrafici.R")


### Indice Produzione Industriale USA 
# (IPI di un certo mese è 110 e l'anno base è 2017=100, significa che l'IPI è aumentata del 10% rispetto al livello medio del 2017)
# -> Seasonally Adjusted
url <- "https://github.com/AuroraMusitelli/Masters---Thesis/raw/main/Data/IndiceProduzioneIndustrialeUSA.xlsx"  
tmp_file <- tempfile(fileext = ".xlsx")
httr::GET(url, httr::write_disk(tmp_file, overwrite = TRUE))
IPI <- read_excel(tmp_file, sheet = "Monthly")

# Tengo i mesi utili 
IPI <- IPI %>% mutate(Month = as.Date(Month, format = "%Y-%m-%d"))  # formato anno/mese/giorno
IPI <- IPI %>% filter(Month >= as.Date("1973-01-01") & Month <= as.Date("2025-07-01"))
# Grafico
plot_time_series(IPI, "Month", "IndexProd", "Indice di Produzione Industriale USA", "Indice")


### Popolazione USA (La popolazione include la popolazione residente)
# NOT Seasonally Adjusted
url <- "https://github.com/AuroraMusitelli/Masters---Thesis/raw/main/Data/PopolazioneUSA.xlsx"  
tmp_file <- tempfile(fileext = ".xlsx")
httr::GET(url, httr::write_disk(tmp_file, overwrite = TRUE))
popUSA <- read_excel(tmp_file, sheet = "Monthly")
# converto in milioni la popolazione
popUSA$PopMil <- popUSA$POPTHM / 1000
popUSA <- popUSA %>% dplyr::select(-POPTHM)

# Tengo i mesi utili 
popUSA <- popUSA %>% mutate(Month = as.Date(Month, format = "%Y-%m-%d"))  # formato anno/mese/giorno
popUSA <- popUSA %>% filter(Month >= as.Date("1973-01-01") & Month <= as.Date("2025-07-01"))
# Grafico
plot_time_series(popUSA, "Month", "PopMil", "Popolazione USA", "Popolazione (milioni)")


### Fornitura mensile di nuove case negli USA MSACSR=nuove case disponibili(invendute) / vendite mensili di nuove case
# Se MSACSR aumenta -> più case rimangono invendute.
# Se MSACSR diminuisce -> le case si vendono più velocemente.
# -> Seasonally Adjusted
url <- "https://github.com/AuroraMusitelli/Masters---Thesis/raw/main/Data/FornituraNuoveCaseUSA.xlsx"  
tmp_file <- tempfile(fileext = ".xlsx")
httr::GET(url, httr::write_disk(tmp_file, overwrite = TRUE))
newHUSA <- read_excel(tmp_file, sheet = "Monthly")

# Tengo i mesi utili 
newHUSA <- newHUSA %>% mutate(Month = as.Date(Month, format = "%Y-%m-%d"))  # formato anno/mese/giorno
newHUSA <- newHUSA %>% filter(Month >= as.Date("1973-01-01") & Month <= as.Date("2025-07-01"))
# Grafico
plot_time_series(newHUSA, "Month", "NewHouses", "Fornitura Mensile di Nuove Case USA", "Rapporto case invendute / vendite mensili")


### Livello di occupazione USA (Persone di età pari o superiore a 16 anni residenti nei 50 stati)
# -> Seasonally Adjusted
url <- "https://github.com/AuroraMusitelli/Masters---Thesis/raw/main/Data/LivelloOccupazioneUSA.xlsx"  
tmp_file <- tempfile(fileext = ".xlsx")
httr::GET(url, httr::write_disk(tmp_file, overwrite = TRUE))
occupationUSA <- read_excel(tmp_file, sheet = "Monthly")
# converto in milioni le persone
occupationUSA$occupationMil <- occupationUSA$LevelOccupation / 1000
occupationUSA <- occupationUSA %>% dplyr::select(-LevelOccupation)

# Tengo i mesi utili 
occupationUSA <- occupationUSA %>% mutate(Month = as.Date(Month, format = "%Y-%m-%d"))  # formato anno/mese/giorno
occupationUSA <- occupationUSA %>% filter(Month >= as.Date("1973-01-01") & Month <= as.Date("2025-07-01"))
# Grafico
plot_time_series(occupationUSA, "Month", "occupationMil", "Livello di Occupazione USA", "Occupati (milioni)")


### GDP USA (Miliardi di dollari): Prodotto interno lordo, tasso annuo trimestrale -> Seasonally Adjusted
url <- "https://github.com/AuroraMusitelli/Masters---Thesis/raw/main/Data/GDPtrimestraleUSA.xlsx"  
tmp_file <- tempfile(fileext = ".xlsx")
httr::GET(url, httr::write_disk(tmp_file, overwrite = TRUE))
GDPUSA <- read_excel(tmp_file, sheet = "Quarterly")

# Converto Month in formato Date
GDPUSA <- GDPUSA %>% mutate(Month = as.Date(Month))
# Creo sequenza di date mensili
monthly_dates <- seq(from = min(GDPUSA$Month), to = as.Date("2025-07-01"), by = "month")
# Interpolazione lineare dei valori mensili
GDP_monthly <- approx(
  x = as.numeric(GDPUSA$Month),
  y = GDPUSA$GDP,
  xout = as.numeric(monthly_dates)) 
# Trasformo in dataframe
GDPUSA <- data.frame(Month = as.Date(GDP_monthly$x, origin = "1970-01-01"), GDP = GDP_monthly$y) 
GDPUSA <- GDPUSA %>% filter(Month >= as.Date("1973-01-01") & Month <= as.Date("2025-07-01"))

# Grafico
plot_time_series(GDPUSA, "Month", "GDP", "PIL USA (mensile interpolato)", "PIL (mld di USD)")



## Devo rendere le variabili esogene stazionarie ##
## Dataset grezzo con le esogene 
data_exog_raw <- GDPUSA %>% dplyr::select(Month, GDP) %>%
  left_join(IPI %>% dplyr::select(Month, IndexProd), by = "Month") %>%
  left_join(newHUSA %>% dplyr::select(Month, NewHouses), by = "Month") %>%
  left_join(occupationUSA %>% dplyr::select(Month, occupationMil), by = "Month") %>%
  left_join(popUSA %>% dplyr::select(Month, PopMil), by = "Month") %>%
  arrange(Month) %>% as_tsibble(index = Month)


## Funzione per ottenere le componenti cicliche HPJ delle esogene
make_hpj_cycle_exog <- function(tsib, seasonal_vars = c("PopMil"), # solo PopolazioneUSA è NOT Seasonally Adjusted
                                freq = 12) {                     # serie mensili
  tmp  <- as_tibble(tsib)
  cols <- setdiff(names(tmp), "Month")   # tutte le colonne tranne l'indice temporale
  
  for (col in cols) {
    v <- as.numeric(tmp[[col]])
    if (all(is.na(v))) next
    v <- zoo::na.approx(v, na.rm = FALSE)
    # Destagionalizzo SOLO se la variabile è in seasonal_vars
    if (col %in% seasonal_vars) {
      y_sa <- remove_seasonality(v, freq = freq)   # TRAMO-SEATS
    } else {
      y_sa <- v                                   # nessuna destagionalizzazione
    }
    # Applico HPJ per rimuovere il trend e ottenere il ciclo
    cycle <- apply_hpj(y_sa)
    cycle[is.na(tmp[[col]])] <- NA   # rimetto NA dove erano NA
    tmp[[col]] <- cycle}
  tsibble::as_tsibble(tmp, index = Month)}


## Componenti cicliche HPJ delle esogene
data_exog_cycle <- make_hpj_cycle_exog(data_exog_raw)

## Rinomino le colonne per chiarezza (tutte ora sono "cicli")
data_exog_cycle <- data_exog_cycle %>%
  rename(
    GDP_cycle        = GDP,
    IPI_cycle        = IndexProd,
    NewHouses_cycle  = NewHouses,
    Occupation_cycle = occupationMil,
    Pop_cycle        = PopMil)

## Aggiungo le dummy macro 
data_exog_final <- data_exog_cycle %>%
  mutate(
    OilCrisis_80s      = ifelse(Month >= as.Date("1979-10-01") & Month <= as.Date("1983-12-01"), 1, 0),
    Liberalization_97_01 = ifelse(Month >= as.Date("1997-01-01") & Month <= as.Date("2001-12-01"), 1, 0),
    Crisis_2008        = ifelse(Month >= as.Date("2008-09-01") & Month <= as.Date("2009-06-01"), 1, 0),
    COVID_2020         = ifelse(Month >= as.Date("2020-03-01") & Month <= as.Date("2020-06-01"), 1, 0))


## Standardizzo i cicli esogeni per rappresentarlo nel grafico
data_exog_plot <- data_exog_final %>%
  mutate(across(c(GDP_cycle, IPI_cycle, NewHouses_cycle, Occupation_cycle, Pop_cycle), ~ as.numeric(scale(.x)), .names = "{.col}_std"))

## Long format per le serie standardizzate
data_exog_long <- data_exog_plot %>%
  dplyr::select(Month, GDP_cycle_std, IPI_cycle_std, NewHouses_cycle_std, Occupation_cycle_std, Pop_cycle_std,
                OilCrisis_80s, Liberalization_97_01, Crisis_2008, COVID_2020) %>%
  pivot_longer(cols = c(GDP_cycle_std, IPI_cycle_std, NewHouses_cycle_std, Occupation_cycle_std, Pop_cycle_std),
               names_to = "Variabile", values_to = "Valore")


## Grafico con linee verticali per le dummy
ggplot(data_exog_long, aes(x = Month, y = Valore, color = Variabile)) +
  geom_line(size = 0.8, alpha = 0.9) +
  # Dummy come linee verticali 
  geom_vline(xintercept = as.Date("1979-10-01"), linetype = "dashed", color = "brown", size = 0.7) +
  geom_vline(xintercept = as.Date("1997-01-01"), linetype = "dashed", color = "purple", size = 0.7) +
  geom_vline(xintercept = as.Date("2008-09-01"), linetype = "dashed", color = "orange", size = 0.7) +
  geom_vline(xintercept = as.Date("2020-03-01"), linetype = "dashed", color = "red", size = 0.7) +
  labs(title = "Componenti Cicliche Standardizzate delle Variabili Esogene USA (1973-2025)", subtitle = "Serie stazionarie",
       x = "Anno", y = "Valore standardizzato (z-score dei cicli HPJ)", color = "Serie") + theme_minimal(base_size = 13) +
  theme(legend.position = "bottom", plot.title = element_text(face = "bold", hjust = 0.5), panel.grid.minor = element_blank())



## Unisco endogene (cicliche/stazionarie) ed esogene cicliche 
dataCO2_varx <- dataCO2_agg %>%
  left_join(data_exog_plot %>%
              dplyr::select(-GDP_cycle_std, -IPI_cycle_std, -NewHouses_cycle_std, -Occupation_cycle_std, -Pop_cycle_std,
                            -OilCrisis_80s, -Liberalization_97_01, -Crisis_2008, -COVID_2020),   
            by = "Month") %>% drop_na() %>% arrange(Month)

dataENERGY_varx <- dataENERGY_agg %>%
  left_join(data_exog_plot %>%
              dplyr::select(-GDP_cycle_std, -IPI_cycle_std, -NewHouses_cycle_std, -Occupation_cycle_std, -Pop_cycle_std,
                            -OilCrisis_80s, -Liberalization_97_01, -Crisis_2008, -COVID_2020),   
            by = "Month") %>% drop_na() %>% arrange(Month)




################################################
#### 2. Salvataggio dei dati pre-processati ####
################################################

dataCO2_varx <- as.data.frame(dataCO2_varx)
dataENERGY_varx <- as.data.frame(dataENERGY_varx)


saveRDS(dataCO2_varx,   "dataCO2_varx.rds")
saveRDS(dataENERGY_varx, "dataENERGY_varx.rds")



