# NOTE ABOUT DOCUMENTATION:
# GGIR does not use Roxygen. The documentation below is not used. 
# All function documentation can be found in the man/*.Rd files.
# Please edit documentation there.
#
#- @title Estimated self-similarity parameter
#- 
#- @description This function estimates the self-similarity parameter (SSP), also known as scaling exponent or alpha.  
#- @usage alpha_hat(data,scale = 2^(1/8),box_size = 4,m=1) 
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
#- @author Ian Meneghel Danilevicz and Victor Mesquita 
#- 
#- @references C.-K. Peng, S.V. Buldyrev, S. Havlin, M. Simons, H.E. Stanley, A.L. Goldberger Phys. Rev. E, 49 (1994), p. 1685
#- Mesquita, Victor & Filho, Florencio & Rodrigues, Paulo. (2020). Detection of crossover points in detrended fluctuation analysis: An application to EEG signals of patients with epilepsy. Bioinformatics. 10.1093/bioinformatics/btaa955. 
#-
#- @examples 
#- # Estimate self-similarity of a very known time series available on R base: the sunspot.year.
#- # Then the spend time with each method is compared.
#- 
#- SSP(sunspot.year, scale = 2)
#- SSP(sunspot.year, scale = 1.2)

SSP = function(data, scale = 2^(1/8), box_size = 4, m = 1){
  if (inherits(x = data, "data.frame")) {
    data = data[, 1]
  }
  if (length(data) <= box_size || any(is.na(data))) {
    alpha_hat = NA
  } else {
    dfa_hat = DFA(data, scale = scale, box_size = box_size, m = m)    
    est_ols = lm(log(dfa_hat[,2]) ~ log(dfa_hat[,1]))
    alpha_hat = est_ols$coefficients[[2]]    
  }
  return(alpha_hat)
} 