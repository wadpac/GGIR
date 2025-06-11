# NOTE ABOUT DOCUMENTATION:
# GGIR does not use Roxygen. The documentation below is not used. 
# All function documentation can be found in the man/*.Rd files.
# Please edit documentation there.
#
#- @title Detrended Fluctuation Analysis
#- 
#- @description ...
#- @usage DFA(data, scale = 2^(1/8), box_size = 4, m = 1)
#- @param data Univariate time series (must be a vector or data frame)
#- @param scale Specifies the ratio between successive box sizes (by default scale = 2^(1/8))
#- @param box_size Vector of box sizes (must be used in conjunction with scale = "F")
#- @param m An integer of the polynomial order for the detrending (by default m=1)
#- 
#- @details The DFA fluctuation can be computed in a geometric scale or for different choices of boxes sizes.
#- 
#- @return Estimated alpha is a real number between zero and two.
#- 
#- @note It is not possible estimating alpha for multiple time series at once.
#- 
#- @author Ian Meneghel Danilevicz  and Victor Mesquita
#- 
#- @references C.-K. Peng, S.V. Buldyrev, S. Havlin, M. Simons, H.E. Stanley, A.L. Goldberger Phys. Rev. E, 49 (1994), p. 1685
#- Mesquita, Victor & Filho, Florencio & Rodrigues, Paulo. (2020). Detection of crossover points in detrended fluctuation analysis: An application to EEG signals of patients with epilepsy. Bioinformatics. 10.1093/bioinformatics/btaa955.
#- 
#- @examples
#- # Estimate self-similarity of a very known time series available on R base: the sunspot.year.
#- # Then the spend time with each method is compared.
#- 

DFA = function(data, scale = 2^(1/8), box_size = 4, m = 1){
  if (inherits(x = data, "data.frame")) {
    data = data[, 1]
  }
  N = length(data)
  if (scale != "F") {
    box_size <- NULL
    n = 1
    n_aux <- 0
    box_size[1] <- 4
    for (n in 1:N) {
      while (n_aux < round(N/4)) {
        n_aux <- box_size[1]
        n = n + 1
        box_size[n] <- ceiling(scale * box_size[n - 1])
        n_aux <- box_size[n] + 4
      }
    }
  }
  ninbox2 <- NULL
  for (j in 1:length(box_size)) {
    ninbox2[j] <- N %/% box_size[j]
  }
  aux_seq = seq_along(ninbox2)  
  aux_length = aux_seq[length(aux_seq)]  
  y_k = cumsum(data) - mean(data)
  rm(data, n_aux, scale)
  aux_mat = matrix(nrow = aux_length, ncol = 2)
  for (j in seq_along(ninbox2)) {
    W = box_size[j] * ninbox2[j]
    aux_j = numeric(W)
    fit = y_k
    for (i in seq_len(W)) {
      if (i == 1) {
        aux_j[1] = box_size[j]
        tmp1 = i:aux_j[i]
      } else if (i >= 2) {
        aux_j[i] = aux_j[i - 1] + box_size[j]
        tmp1 = (aux_j[i - 1] + 1):(aux_j[i])
      }
      if (m == 1) {
        fit[tmp1] = lm.fit(x = cbind(1, tmp1), y = y_k[tmp1])$fitted.values
      } else {
        fit[tmp1] = lm(y_k[tmp1] ~ poly(tmp1, m, raw = TRUE))$fitted.values
      }
      if (i >= ninbox2[j]) {
        aux_j[i] <- 0
      }
    }
    aux_mat[j,] = c(round(box_size[j], digits = 0),
                    sqrt((1 / N) * sum((y_k[1:W] - fit[1:W]) ^ 2)))
  }
  colnames(aux_mat) <- c("box", "DFA")
  aux_list = aux_mat
  return(aux_list)
}