DEV_ <- function(raw_ts,
                          E,
                          tau,
                          theta,
                          winsize,
                          stepsize
    ){
  
  
  window_indices <- seq(winsize, length(raw_ts), stepsize)
  matrix_result <- matrix(NaN, nrow = length(window_indices), ncol = 4)
  index <- 0
  
  # In every sliding window
  for(j in window_indices){

    index <- index + 1
    # Set the window
    sliding_window <- raw_ts[(j - winsize + 1):j]
    
    # Normalise the window
    norm_sliding_window <- (sliding_window - mean(sliding_window, na.rm = T))/sd(sliding_window, na.rm = T)
    
    # Calculate univariate S-map coefficients
    smap <- s_map(norm_sliding_window, E = E, tau = tau, theta = theta)
    
    # Choose the theta that gives the best predictability
    best <- which.max(smap$rho)
    theta_best <- smap$theta[best]
    
    # Calculate the S-map coefficients for the best theta
    smap <- s_map(norm_sliding_window, E = E, tau = tau, theta = theta_best,
                  save_smap_coefficients = T)
    
    smap_co <- smap$smap_coefficients[[1]]
    
    # Matrix for storing the results
    matrix_eigen <- matrix(NA, nrow = nrow(smap_co), ncol = 3)
    
    # Calculate the DEV
    for(k in 2:nrow(smap_co))
    {
      if(!is.na(smap_co[k,1]))
      {
        M <- rbind(as.numeric(smap_co[k, 3:ncol(smap_co)]), cbind(diag(E - 1), rep(0, E - 1)))
        M_eigen <- eigen(M)$values
        lambda1 <- M_eigen[order(abs(M_eigen))[E]]
        
        matrix_eigen[k,1] <- abs(lambda1)
        matrix_eigen[k,2] <- Re(lambda1)
        matrix_eigen[k,3] <- Im(lambda1)
      }
    }
    
    # Save the results
    matrix_result[index,1] <- j
    matrix_result[index,2] <- mean(matrix_eigen[,1],na.rm=TRUE)
    matrix_result[index,3] <- mean(matrix_eigen[,2],na.rm=TRUE)
    matrix_result[index,4] <- mean(matrix_eigen[,3],na.rm=TRUE)
  }
  
  colnames(matrix_result) <- c('t', '|DEV|', 'Re(DEV)', 'Im(DEV)')
  
  return(matrix_result)

}

DEV <- cmpfun(DEV_)
