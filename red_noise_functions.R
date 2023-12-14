# Functions to generate time series with red noise


# Noy-Meir Model - Fold bifurcation
nm_fold <- function(t_end = 1e4, f_min = 0, f_max = 2, sigma = 0.01, corr = 0.3){
  
  N_values <- rep(NA, t_end)
  
  f <- f_min
  df <- (f_max - f_min)/t_end
  
  N_values[1] <- 7.5
  
  noise <- rnorm(1)
  
  for(i in 2:t_end){
    N <- N_values[i-1]
    
    N_values[i] <- N*exp(0.75 - 0.1*N) - (f*(N^2/(N^2 + 0.75^2))) + N*sigma*noise
    
    noise = corr*noise + sqrt(1-corr^2)*rnorm(1)
    
    f <- f + df
  }
  
  return(N_values)
}


# Henon map - Period-doubling bifurcation
henon_pd <- function(t_end = 1e4, a_min = 0.1, a_max = 0.4, sigma = 0.01, corr = 0){
  
  x_values <- rep(NA, t_end)
  y_values <- rep(NA, t_end)
  
  
  a <- a_min
  da <- (a_max - a_min)/t_end
  
  x_values[1] <- 1.21
  y_values[1] <- 1.21
  
  noise <-  rnorm(1)
  
  for(i in 2:t_end){
    
    x <- x_values[i-1]
    y <- y_values[i-1]
    
    x_values[i] <- 1 - a*x^2 + y + x*sigma*noise
    y_values[i] <- 0.3*x
    
    noise = corr*noise + sqrt(1-corr^2)*rnorm(1)
    
    a <- a + da
  }
  
  return(x_values)
}


# Rosenzweig- MacArthur Model - Neimark-Sacker bifurcation
rma_neimark_sacker <- function(t_end = 1e4, l_min = 3.48, l_max = 3.78, sigma = 1e-3, corr = 0){
  
  x_values <- rep(NA, t_end)
  y_values <- rep(NA, t_end)
  
  
  l <- l_min
  dl <- (l_max - l_min)/t_end
  
  x_values[1] <- 0.62
  y_values[1] <- 1.4
  
  noise <-  rnorm(1)
  
  for(i in 2:t_end){
    
    x <- x_values[i-1]
    y <- y_values[i-1]
    
    x_values[i] <- (1+l)*x - 4*x^2 - (x*y/(1+0.5*x)) + x*sigma*noise
    y_values[i] <- -2*y + (6*x*y/(1+0.5*x))
    
    noise = corr*noise + sqrt(1-corr^2)*rnorm(1)
    
    l <- l + dl
  }
  
  return(x_values)
  
}
