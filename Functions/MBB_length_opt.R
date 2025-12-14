###############################################################################         
########## Estimate the optimal blocks length based on AR(1) dynamic ##########      GITHUB blocklength 
##########                See Mudelsee's book                        ##########   
############################################################################### 
# Questa funzione sceglie la lunghezza ottimale dei blocchi per il moving block bootstrap, lunghezza ottimale serve a generare
# bootstrap coerenti con la dipendenza dei residui HVAR

MBB_length_opt <- function(series) {
  series <- as.numeric(na.omit(series))   # prendo la serie tolgo i NA e la trasformo in numerica
  n   <- length(series)   # lunghezza serie
  fit_ar1 <- Arima(    # stimo un modello AR(1) sulla serie
    y = series, order = c(1, 0, 0),   # AR(1)
    include.mean = FALSE,   # no intercetta
    method = "ML") # stimo con max likelihood 
  phi <- coef(fit_ar1)[["ar1"]]   # estraggo il coefficiente Phi AR(1) 
  l_opt <- ceiling((sqrt(6) * phi / (1 - phi^2))^(2/3) * n^(1/3))    # uso la formula di Mudelsee per la lunghezza ottimale del blocco 
  return(l_opt)
}