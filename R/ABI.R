# NOTE ABOUT DOCUMENTATION:
# GGIR does not use Roxygen. The documentation below is not used. 
# All function documentation can be found in the man/*.Rd files.
# Please edit documentation there.
#
#- @title Activity balance index (ABI)
#- 
#- @description This function estimates the Activity balance index (ABI), which is a transformation of the self-similarity parameter (SSP), also known as scaling exponent or alpha.  
#- @param x the estimated self-similarity parameter (SSP)
#-
#- @details ABI = exp(-abs(SSP-1)/exp(-2))
#-
#- @return The estimated Activity balance index (ABI) is a real number between zero and one.
#-
#- @author Ian Meneghel Danilevicz  
#- 
#- @references C.-K. Peng, S.V. Buldyrev, S. Havlin, M. Simons, H.E. Stanley, A.L. Goldberger Phys. Rev. E, 49 (1994), p. 1685
#- Mesquita, Victor & Filho, Florencio & Rodrigues, Paulo. (2020). Detection of crossover points in detrended fluctuation analysis: An application to EEG signals of patients with epilepsy. Bioinformatics. 10.1093/bioinformatics/btaa955. 
#-
#- @examples 
#- # Estimate Activity balance index of a very known time series available on R base: the sunspot.year.
#- 
#- ssp = SSP(sunspot.year, scale = 1.2)
#- abi = ABI(ssp)
#-
ABI = function(x){
  if (is.na(x)) {
    y = NA
  } else {
    y = exp(-abs(x - 1) * exp(2))  
  }
  return(y)  
}