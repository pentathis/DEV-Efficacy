# This file contains the functions to generate time series for the continuous models we considered. 
# The function is named according the type of system it generates.
# In case of univariate models, just the time series vector is returned. In the case of multivariate  models,
# a data frame with the time indices in the first column and variable values in subsequent columns is returned.


# Fold bifurcation
fold <- function(t_end = 1e6, K = 10, r = 1, c_min = 1, c_max = 3, 
                 b = 1, sigma = 0.5, dt = 0.01){
  
  fold_ts <- rep(NaN, t_end)
  c <- c_min
  dc <- (c_max - c_min)/t_end
  
  
  fold_ts[1] <- 8.83
  
  for(i in 2:t_end){
    
    N = fold_ts[i-1]
    
    fold_ts[i] <- N + (r*N*(1-N/K) - (c*N^2/(b^2 + N^2)))*dt + sigma*rnorm(1)*dt
    
    c <- c + dc
  }
  
  return(fold_ts[seq(1, t_end, length.out = 1e4)])
  
}


# Transcritical bifurcation
transcritical <- function(t_end = 1e6, K = 10, r = 1, c_min = 2, c_max = 0, 
               sigma = 0.1, dt = 0.01){
  
  tcts <- rep(NaN, t_end)
  tcts[1] <- 0.1
  
  c <- c_min
  dc <- (c_max - c_min)/t_end
  
  for(i in 2:t_end){
    
    N <- tcts[i-1]
    tcts[i] <- N + (r*N*(1-N/K) - c*N)*dt + sigma*rnorm(1)*dt
    
    c <- c + dc
    
  }
  
  return(tcts[seq(1, t_end, length.out = 1e4)])
}


# Hopf bifurcation
hopf <- function(t_end = 1e6, K_min = 2, K_max = 4, r = 0.5, a = 0.4, b = 0.6, e = 0.6, 
                 d = 0.15, sigma = 0.05, dt = 0.01){
  
  n_values <-  rep(NaN, t_end)
  p_values <-  rep(NaN, t_end)
  
  # Initial conditions
  n_values[1] <- 1
  p_values[1] <- 1
  
  K <- K_min
  dK <- (K_max -  K_min)/t_end
  
  for(i in 2:t_end){
    
    N = n_values[i-1]
    P = p_values[i-1]
    
    n_values[i] <- N + (r*N*(1-N/K) - (a*N*P)/(b+N))*dt + sigma*rnorm(1)*dt
    p_values[i] <- P + ((e*a*N*P)/(b+N) - d*P)*dt + sigma*rnorm(1)*dt
    
    K <- K + dK
    
  }
  
  
  return(cbind(seq(1, 1e4), n_values[seq(1, t_end, length.out = 1e4)], 
               p_values[seq(1, t_end, length.out = 1e4)]))
}


pitchfork <- function(t_end = 1e6, K = 10, c = 0.8, N_c = 5, 
                      I = 4, r_min = 0, r_max = 1, sigma = 0.5, dt = 0.01){
  
  pfts <- rep(NaN, t_end)
  r <-  r_min
  dr <- (r_max - r_min)/t_end
  
  pfts[1] <- 5
  
  for(i in 2:t_end){
    
    N = pfts[i-1]
    
    pfts[i] <- N + (r*N*(1-N/K)*(N-N_c) - c*N + I)*dt + sigma*rnorm(1)*dt
    
    r <- r + dr
  }
  
  return(pfts[seq(1, t_end, length.out = 1e4)])
}

# Bifurcation without CSD
wo_csd <- function(t_end = 1e6, r = 1, d = 0.1, a_J = 0.01, a_A = 0.01, 
                                    e = 0.4, K_J_min = 20, K_J_max = 150,
                                    K_A = 150, sigma = 10,
                                    dt = 0.01){
  
  R_J_values <- rep(NaN, t_end)
  R_A_values <- rep(NaN, t_end)
  C_J_values <- rep(NaN, t_end)
  C_A_values <- rep(NaN, t_end)
  
  R_J_values[1] <- 4
  R_A_values[1] <- 127
  C_J_values[1] <- 81
  C_A_values[1] <- 15
  
  K_J = K_J_min
  dK_J = (K_J_max - K_J_min)/t_end
  
  
  for(i in 2:t_end){
    
    R_J = R_J_values[i-1]
    R_A = R_A_values[i-1]
    C_J = C_J_values[i-1]
    C_A = C_A_values[i-1]
    
    R_J_values[i] <- R_J + (r*R_J*(1-R_J/K_J) - a_J*R_J*C_J)*dt 
    R_A_values[i] <- R_A + (r*R_A*(1-R_A/K_A) - a_A*R_A*C_A)*dt 
    C_J_values[i] <- C_J + (e*a_A*R_A*C_A - d*C_J - e*a_J*R_J*C_J)*dt + sigma*rnorm(1)*dt
    C_A_values[i] <- C_A + (e*a_J*R_J*C_J - d*C_A)*dt 
    
    
    K_J <- K_J + dK_J
  }
  
  pts <- seq(1, t_end, length.out = 1e4)
  
  return(cbind(1:1e4, R_J_values[pts], R_A_values[pts], C_J_values[pts], C_A_values[pts]))
  
}


# No transition
no_trans <- function(t_end = 1e6, K = 1.9, r = 1, c_min = 1, c_max = 3, 
                     b = 1, sigma = 0.1, dt = 0.01){
  
  no_trans_ts <- rep(NaN, t_end)
  c <- c_min
  dc <- (c_max - c_min)/t_end
  
  
  no_trans_ts[1] <- 1
  
  for(i in 2:t_end){
    
    N = no_trans_ts[i-1]
    
    no_trans_ts[i] <- N + (r*N*(1-N/K) - (c*N^2/(b^2 + N^2)))*dt + sigma*rnorm(1)*dt
    c <- c + dc
  }
  
  return(no_trans_ts[seq(1, t_end, length.out = 1e4)])
  
}


# Transition without bifurcation
wo_bif <- function(t_end = 1e6, K = 5.2, r = 1, c_min = 1, c_max = 3, b = 1,
                   sigma = 0.5, dt = 0.01){
  
  wo_bif_ts <- rep(NaN, t_end)
  c <- c_min
  dc <- (c_max - c_min)/t_end
  
  
  wo_bif_ts[1] <- 4
  
  for(i in 2:t_end){
    
    N = wo_bif_ts[i-1]
    
    wo_bif_ts[i] <- N + (r*N*(1-N/K) - (c*N^2/(b^2 + N^2)))*dt + sigma*rnorm(1)*dt
    
    c <- c + dc
  }
  
  return(wo_bif_ts[seq(1, t_end, length.out = 1e4)])
  
}
