## Funzione per generare IRF da un modello HVAR (BigVAR)  

## Phi   : matrice k x (k*p) dei coefficienti senza intercetta
## Sigma : matrice k x k di covarianza dei residui
## n     : orizzonte (numero di passi avanti)
## k     : numero di serie
## p     : ordine del VAR
## Y0    : vettore k-dimensionale con lo shock iniziale (es. 1 sulla variabile ENERGY, 0 sulle altre)

suppressMessages(library(expm))
generateIRF <- function(Phi, Sigma, n, k, p, Y0) {
  # Se p > 1, passo a forma VAR(1) con companion matrix
  if (p > 1) {
    A <- VarptoVar1MC(Phi, p, k)   # funzione di BigVAR
  } else {
    A <- Phi
  }
  # Matrice di selezione J per riportare da stato "companion" alle k serie originali
  J <- matrix(0, nrow = k, ncol = k * p)
  diag(J) <- 1
  
  # Cholesky della matrice di covarianza (identificazione strutturale)
  P <- t(chol(Sigma))
  
  # Matrice per salvare le IRF: righe = serie, colonne = orizzonte (0,...,n)
  IRF <- matrix(0, nrow = k, ncol = n + 1)
  
  for (i in 0:n) {
    # Matrice delle MA coefficient (phi_i)
    phi_i   <- J %*% (A %^% i) %*% t(J)
    theta_i <- phi_i %*% P
    IRF[, i + 1] <- theta_i %*% Y0
  }
  
  return(IRF)
}



