#############################################
#### IRF via companion matrix + Cholesky ####   GITHUB bvartools e irf.bvar
#############################################
# Riferimento teorico Lütkepohl (2005, New Introduction to Multiple Time Series Analysis)
# mostra che le IRF possono ottenersi con matrice companion + matrice di selezione J --> PHI_h = JA^(h-1)J'P

IRF_from_Phi <- function(Phi, Sigma, H, k, p) {
  # Matrice Companion A: 
  if (p > 1) {       # Se p > 1 costruisco la companion matrix con BigVAR::VarptoVar1MC
    A_comp <- VarptoVar1MC(Phi, p, k)  # K*p x K*p
  } else {     # altrimenti è un semplice VAR(1) e uso Phi 
    A_comp <- Phi                       # caso VAR(1)
  }
  Kp <- nrow(A_comp)
  
  # Matrice di selezione J: estrae y_t dal vettore stato z_t = [y_t', y_{t-1}', ..., y_{t-p+1}']'
  J <- cbind(diag(k), matrix(0, k, Kp - k))  # K x (K*p)
  
  # Decomposizione di Cholesky di Sigma e ottengo P (shock ortogonali)
  P <- t(chol(Sigma))  # K x K
  
  # Array per le IRF: H x K (variabile risposta) x K (shock)
  IRF <- array(0, dim = c(H, k, k))
  # A_pow = A_comp^0 = I matrice identità
  A_pow <- diag(Kp)
  
  for (h in 1:H) {
    # Matrice di risposta all'orizzonte h: PHI_h = JA^(h-1)J'P
    Theta_h   <- J %*% A_pow %*% t(J) %*% P
    IRF[h,, ] <- Theta_h
    # Aggiorno A_pow = A_comp^h
    A_pow <- A_comp %*% A_pow}
  return(IRF)
}
# !!Ottengo un array IRF[h,i,j] che rappresenta l'effetto di uno shock strutturale nella variabile j sulla variabile i all'orizzonte h!!

