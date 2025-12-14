#######################################
#### Funzione ausiliaria per i lag ####     GITHUB zeitreihe 
#######################################
# Riferimento teorico Lütkepohl (2005, New Introduction to Multiple Time Series Analysis)
# Questa funzione per ogni t prendo i K valori a lag 1, lag 2, . lag p e li concateno in un vettore riga; questo ricostruisce la tipica 
# matrice dei regressori.

# BIGVAR NON RESTITUISCE la matrice dei regressori Z_t usata nella stima, restituisce solo la matrice di coefficienti B^
# Questa funzione serve per:
# ricostruire la matrice dei regressori Z_t = (Y_{t-1},...,Y_{t-p})
# calcolare i fitted values:   Y_hat_t = c + B * Z_t
# ricostruire i residui:       e_t = Y_t - Y_hat_t
# applicare bootstrap ai residui (moving block / wild bootstrap)
# calcolare IRF tramite companion matrix + Cholesky

make_lag_matrices <- function(Y, p_lag) {
  Y    <- as.matrix(Y)
  Tobs <- nrow(Y)   # numero totale di osservazioni T
  K    <- ncol(Y)   # numero di variabili
  T_eff <- Tobs - p_lag    # Numero di osservazioni effettive
  
  Z      <- matrix(0, T_eff, K * p_lag)
  Y_used <- matrix(0, T_eff, K)
  
  #Ciclo per costruire lag 
  for (t in 1:T_eff) {
    t_star <- p_lag + t              # indice temporale "reale"
    vec <- c()                       # Inizializzo un vettore vuoto che conterrà i (K x p) lag in ordine
    for (L in 1:p_lag) {
      vec <- c(vec, Y[t_star - L, ]) # aggiungo i K valori al lag L
    }
    Z[t, ]      <- vec              #  regressori laggati per l'osservazione t
    Y_used[t, ] <- Y[t_star, ]
  }
  # Ritorno una lista 
  return(list(Z = Z, Y_used = Y_used))
}
# !!Ottengo una lista con!!
# Z      = matrice T_eff × (K*p) dei regressori laggati
# Y_used = matrice T_eff × K delle osservazioni effettive

