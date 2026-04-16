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
#- @param epochSize An integer with epoch length in seconds
#-
#- @details The DFA fluctuation can be computed in a geometric scale or for different choices of boxes sizes.
#-
#- @return Estimated alpha, alpha_1, and alpha_2: real number between zero and two.
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

SSP = function(data, scale = 2^(1/8), box_size = 4, m = 1, epochSize){
  
  # Helper to replicate the fluctuation analysis using sapply later
  calc_fluctuation = function(n, y, N_total) {
    num_boxes = N_total %/% n
    W = n * num_boxes
    y_truncated = y[1:W]
    
    # Reshape y into a matrix: each column is one box
    y_matrix = matrix(y_truncated, nrow = n)
    
    # Create the x-vector for a single box: 1:n
    x = 1:n
    # Slope (beta) for each box: beta = cov(x,y) / var(x)
    x_detrend = x - mean(x)
    sum_x2 = sum(x_detrend^2)
    
    # betas is a vector of slopes (one for each box)
    betas = colSums(y_matrix * x_detrend) / sum_x2
    
    # intercepts is a vector of intercepts (one for each box)
    y_means = colMeans(y_matrix)
    intercepts = y_means - (betas * mean(x))
    
    # Fits for every point
    # fit_matrix[i, j] = intercepts[j] + betas[j] * x[i]
    betas_mtx = matrix(betas, nrow = n, ncol = num_boxes, byrow = TRUE)
    intercepts_mtx = matrix(intercepts, nrow = n, ncol = num_boxes, byrow = TRUE)
    fit_matrix = intercepts_mtx + betas_mtx * x
    
    # RMS calculation as in DFA (average fluctuation)
    return(sqrt(sum((y_matrix - fit_matrix)^2) / N_total))
  }
  
  # MAIN SCRIPT ------
  if (inherits(x = data, "data.frame")) {
    data = data[, 1]
  }
  if (length(data) <= box_size || any(is.na(data))) {
    return(list(alpha_overall = NA, alpha_1 = NA, alpha_2 = NA))
  } else {
    # Box size calculation (imitating DFA() function)
    N = length(data)
    if (scale != "F") {
      box_sizes <- 4
      while (tail(box_sizes, 1) + 4 < round(N/4)) {
        box_sizes <- c(box_sizes, ceiling(scale * tail(box_sizes, 1)))
      }
    } else {
      box_sizes = box_size
    }

    # y_k calculation (equal to DFA() function)
    y_k = cumsum(data - mean(data))

    # Apply to all box sizes
    dfa_values = sapply(box_sizes, calc_fluctuation, y = y_k, N_total = N)
    
    # Convert box sizes to minutes to apply boundaries for alpha_1 and alpha_2
    box_sizes_min = box_sizes * epochSize / 60
    
    # OUTPUT ----
    
    # 1. Overal Alpha (SSP)
    model = lm(log(dfa_values) ~ log(box_sizes))
    alpha_hat = as.numeric(coef(model)[2])
    
    # 2. alpha_1 (<= 90 minutes)
    idx_1 = which(box_sizes_min <= 90)
    if (length(idx_1) >= 3) { # required at least 3 values to fit the lm
      model_1 = lm(log(dfa_values[idx_1]) ~ log(box_sizes[idx_1]))
      alpha_1 = as.numeric(coef(model_1)[2])
    } else {
      alpha_1 = NA
    }
    
    # 3. Final Alpha_2 Calculation (Between 120 and 600 minutes)
    idx_2 = which(box_sizes_min >= 120 & box_sizes_min <= 600)
    if (length(idx_2) >= 3) {
      model_2 = lm(log(dfa_values[idx_2]) ~ log(box_sizes[idx_2]))
      alpha_2 = as.numeric(coef(model_2)[2])
    } else {
      alpha_2 = NA
    }
  }
  return(list(alpha_overall = alpha_hat, alpha_1 = alpha_1, alpha_2 = alpha_2))
} 