# Functions to generate time series for the considered discrete models
# Each function returns the time series as an array when the model is univariate
# In the case of multivariate  models, a data frame with the time indices in the 
# first column and variable values in subsequent columns is returned.


# Noy-Meir Model - Fold bifurcation
nm_fold <- function(t_end = 1e4, f_min = 0, f_max = 2, sigma = 0.05){
  
  N_values <- rep(NA, t_end)
  
  f <- f_min
  df <- (f_max - f_min)/t_end
  
  N_values[1] <- 7.5
  
  for(i in 2:t_end){
    N <- N_values[i-1]
    
    N_values[i] <- N*exp(0.75 - 0.1*N) - (f*(N^2/(N^2 + 0.75^2))) + sigma*rnorm(1)
    
    f <- f + df
  }
  
  return(N_values)
}


# Lotka-Volterra Model - Transcritical bifurcation
lv_transcritical <- function(t_end = 1e4, r = 0.5, c_min = 0.5, c_max = 1.5,
                             sigma = 8e-4){
  
  x_values <- rep(NA, t_end)
  y_values <- rep(NA, t_end)
  
  x_values[1] <- 0.67
  y_values[1] <- 0.11
  
  c <- c_max
  dc <- -(c_max - c_min)/t_end
  
  for(i in 2:t_end){
    
    x <- x_values[i-1]
    y <- y_values[i-1]
    
    x_values[i] <- (r+1)*x - r*x^2 - c*x*y + sigma*rnorm(1)
    y_values[i] <- c*x*y + sigma*rnorm(1)
    
    c <- c + dc
  }
  
  return(cbind(1:t_end, x_values, y_values))
}


# Reduced Lorenz Model - Pitchfork bifurcation
lorenz_pitchfork <- function(t_end = 1e4, h = 0.5, a_min = -1, a_max = 1, sigma = 0.005){
  
  x_values <- rep(NA, t_end)
  y_values <- rep(NA, t_end)
  
  x_values[1] <- 0
  y_values[1] <- 0
  
  a <- a_min
  da <- (a_max - a_min)/t_end
  
  for(i in 2:t_end){
    
    x <- x_values[i-1]
    y <- y_values[i-1]
    
    x_values[i] <- (1+a*h)*x - h*x*y + sigma*rnorm(1)
    y_values[i] <- (1 - h)*y + h*x^2 + sigma*rnorm(1)
    
    a <- a + da
  }
  
  return(cbind(1:t_end, x_values, y_values))
}