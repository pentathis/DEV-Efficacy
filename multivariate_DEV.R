multivariate_DEV_ <- function(raw_ts, # Data frame with first column as time indices
                          E,
                          tau,
                          theta,
                          winsize,
                          stepsize
    ){
  
  
  window_indices <- seq(winsize, nrow(raw_ts), stepsize)
  matrix_result <- matrix(NaN, nrow = length(window_indices), ncol = 4)
  index <- 0
  
  
  # In every sliding window
  for(j in window_indices){
    
    index <- index + 1
    
    # Set the window
    sliding_window <- raw_ts[(j - winsize + 1):j, -1]
    
    sliding_window_time <- raw_ts[(j - winsize + 1):j, 1]
    
    for(targ_col in 1:ncol(sliding_window)){
      
      # Adjust the window
      sliding_window_adj <- cbind(sliding_window[(1-tau):nrow(sliding_window),targ_col], 
                              sliding_window[1:(nrow(sliding_window)+tau),])
      
      # Normalise the window
      norm_sliding_window <- as.data.frame(apply(sliding_window_adj[,1:ncol(sliding_window_adj)], 
                                                 2, function(x) (x-mean(x))/sd(x)))
      
      norm_sliding_window <- cbind(sliding_window_time[1:(length(sliding_window_time)+tau)]
                                   , norm_sliding_window) 
      
      # Set column names for SMap function
      col_names <- c('time', 'targ')
      
      for(col in 3:ncol(norm_sliding_window)){
        col_names <- append(col_names, col)
      }
      
      colnames(norm_sliding_window) <- col_names
      
      
      # Create lib names for SMap function
      lib <- paste(1, nrow(norm_sliding_window))
      pred <- paste(1, nrow(norm_sliding_window))
      
      
      rho_values <- array(NA, dim = length(theta))

      # Choose the best theta
      for(tht in 1:length(theta)){
        
        smap <- SMap(dataFrame = norm_sliding_window, 
                     lib = lib,
                     pred = pred, 
                     theta = theta[tht],
                     E = E, embedded = TRUE, 
                     columns = col_names[3:length(col_names)], target = 'targ')
        
        rho_values[tht] <- cor(na.omit(smap$predictions)$Predictions,
                               na.omit(smap$predictions)$Observations)
        
      }
      
      best.theta <- theta[which.max(rho_values)]
      
      
      # Calculate multivariate S-map coefficients
      assign(paste0('smap', targ_col), SMap(dataFrame = norm_sliding_window, 
                                            lib = lib,
                                     pred = pred, 
                   theta = best.theta,
                   E = E, embedded = TRUE, 
                   columns = col_names[3:length(col_names)], target = 'targ')$coefficients)
      
    }
    
    
      
    # Matrix for storing the results
    matrix_eigen <- matrix(NA, nrow = nrow(smap1), ncol = 3)
    
    M <- matrix(NA, nrow = E, ncol = E)
    
    
    # Calculate the DEV
  
    for(k in 2:nrow(smap1)){
      
      for(targ_col in 1:E){
        
        M[targ_col,] <- as.numeric(get(paste0('smap', targ_col))[k, 3:ncol(get(paste0('smap', targ_col)))])
      }
      
      M_eigen <- eigen(M)$values
      lambda1 <- M_eigen[order(abs(M_eigen))[E]]
      
      matrix_eigen[k,1] <- abs(lambda1)
      matrix_eigen[k,2] <- Re(lambda1)
      matrix_eigen[k,3] <- Im(lambda1)
      
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

multivariate_DEV <- cmpfun(multivariate_DEV_)
