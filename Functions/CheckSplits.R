## Controllo esplicito delle split temporali 
check_splits <- function(Y_mat, T1, T2) {
  T_tot <- nrow(Y_mat)
  tibble(
    Fase = c("Training", "CV (BigVAR)", "Test finale"),
    Start = c(1,        T1 + 1,        T2 + 1),
    End   = c(T1,       T2,            T_tot),
    N_osservazioni = End - Start + 1,
    Percentuale = round(100 * N_osservazioni / T_tot, 1))}

check_splits(Y_mat_CO2_std,    T1_CO2_std,    T2_CO2_std)
check_splits(Y_mat_ENERGY_std, T1_ENERGY_std, T2_ENERGY_std)