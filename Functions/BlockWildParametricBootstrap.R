#######################################################################################
########## Residuals block wild parametric bootstrap (versione multivariata) ##########    GITHUB Class-MovingBlocks
#######################################################################################
# Questa funzione prende la matrice dei residui del modello HVAR e i fitted yhat e genera nsim serie bootstrap multivariate 
# moving block bootstrap (MBB) sui residui per preservare autocorrelazione nel tempo,
# wild bootstrap (moltiplica blocchi per +-1) per gestire eteroschedasticità

# e : matrice T x K dei residui
# yhat : matrice T x K con i fitted 
# nsim : numero di serie bootstrap da generare
# H    : orizzonte di previsione 
# L    : lunghezza del blocco 
# Wild : TRUE = wild bootstrap (moltiplico i blocchi per +-1), FALSE = semplice block bootstrap

Res_Block_Wild_BootGenerator <- function(e, nsim = 1, yhat = NULL, H = NULL, L = 1, Wild = FALSE, Seed = 1234) {
  e <- as.matrix(e) # !!residui e, trasformo in matrice T x K!!
  Tobs <- nrow(e)   # numero osservazioni
  K    <- ncol(e)   # numero di variabili
  
  # !!Centro ogni colonna dei residui (media zero per ogni variabile)!!
  e <- scale(e, center = TRUE, scale = FALSE)
  
  # Converto yhat fitted in matrice 
  if (!is.null(yhat)) {
    yhat <- as.matrix(yhat)}
  
  # Fisso il seme per replicabilità
  if (!is.null(Seed)) set.seed(Seed)
  
  # !!Array 3D che conterranno residui e serie bootstrap!!
  e_b <- array(0, dim = c(Tobs, K, nsim))    # Dimensioni: Tobs x K x nsim per i residui
  y_b <- array(0, dim = c(Tobs, K, nsim))    # Dimensioni: Tobs x K x nsim per le serie bootstrap
  
  # Scelgo la lunghezza dei blocchi 
  if (!is.null(L)) {
    blocks_lng <- L    # Se L non è NULL, la uso come lunghezza dei blocchi 
  } else {
    blocks_lng <- MBB_length_opt(series = e[, 1])    # Se L è NULL, chiamo MBB_length_opt sul primo residuo per scegliere la lunghezza ottimale
  }
  
  # Calcolo il numero di blocchi da concatenare per coprire tutta la serie
  if (!is.null(H)) {
    blocks_num <- ceiling((Tobs - H) / blocks_lng)   # se H non è NULL con forecast adjustment
  } else {
    blocks_num <- ceiling(Tobs / blocks_lng)}    # se H è NULL senza forecast adjustment
  
  # Applico un moving block bootstrap multivariato sui residui
  # Loop sulle repliche bootstrap b = 1,...,nsim
  for (b in 1:nsim) {
    # Loop sui blocchi i = 1,...,blocks_num per costruire le serie bootstrap
    for (i in 1:blocks_num) {
      # Calcolo gli indici temporali da riempire nel bootstrap
      if (i < blocks_num) {
        idx_ts  <- ((i - 1) * blocks_lng + 1):(i * blocks_lng)
        idx_len <- blocks_lng
      } else {
        # per l'ultimo blocco, posso avere un pezzo più corto per arrivare fino a Tobs
        idx_ts  <- ((i - 1) * blocks_lng + 1):Tobs
        idx_len <- length(idx_ts)
      }
      # Scelgo casualmente l'endpoint del blocco di residui da copiare
      endpoint <- sample(blocks_lng:Tobs, size = 1)
      idx_src  <- (endpoint - idx_len + 1):endpoint
      # !!Estraggo il blocco di residui su tutte le K variabili del sistema!!
      blc <- e[idx_src, , drop = FALSE]
      # Wild bootstrap (moltiplico tutto il blocco per +-1) per trattare eteroschedasticità
      if (Wild) {
        w <- sample(c(-1, 1), 1)
        blc <- blc * w
      }
      # Inserisco il blocco nella serie bootstrap dei residui per la replica b
      e_b[idx_ts, , b] <- blc
    }
    
    # 2) Costruisco la pseudo-serie UNA VOLTA: Y* = Yhat + e*
    if (!is.null(yhat)) {
      y_b[,,b] <- yhat + e_b[,,b]
    } else {
      y_b[,,b] <- e_b[,,b]
    }  # fine loop sui blocchi i
  }
  # !!Ottengo una lista con!!
  #  y_b: le serie bootstrap (Tobs x K x nsim)
  #  e_b: i residui bootstrap (Tobs x K x nsim)
  return(list(y_b = y_b, e_b = e_b))
}




