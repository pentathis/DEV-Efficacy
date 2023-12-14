# Functions to obtain time series for the considered non-smooth (piecewise smooth) models


# A general function for the four kinds of piecewise smooth transitions
# except Neimark-Sacker bifurcation
non_smooth_general <- function(t_end = 1e4, x0, a, b, mu_min = -1, mu_max = 1, sigma = 0.03){
  
  x <- rep(NA, t_end)
  x[1] <- x0
  
  mu <- mu_min
  dmu <- (mu_max - mu_min)/t_end
  
  for(i in 2:t_end){
    
    if(x[i-1] < 0){
      x[i] <- a*x[i-1] + mu + sigma*rnorm(1)
    }else{
      x[i] <- b*x[i-1] + mu + sigma*rnorm(1)
    }
    
    mu <- mu + dmu
  }
  
  return(x)
}


# Piecewise smooth Neimark-Sacker bifurcation
ns_neimark_sacker <- function(t_end = 1e4, x0 = -0.0125, y0 = -0.00375,
                              tau_l = -0.1, delta_l = -0.3, tau_r = -0.685, delta_r = 1.4,
                              mu_min = -0.01, mu_max = 0.01, sigma = 2e-4){
  mu <- mu_min
  dmu <- (mu_max - mu_min)/t_end
  
  x_values <- rep(NA, t_end)
  y_values <- rep(NA, t_end)
  
  x_values[1] <- x0
  y_values[1] <- y0
  
  for(i in 2:t_end){
    
    x <- x_values[i-1]
    y <- y_values[i-1]
    
    if(x<0){
      x_values[i] <- tau_l*x + y + mu + sigma*rnorm(1)
      y_values[i] <- -delta_l*x + sigma*rnorm(1)
    }else{
      x_values[i] <- tau_r*x + y + mu + sigma*rnorm(1)
      y_values[i] <- -delta_r*x + sigma*rnorm(1)
    }
    
    mu <- mu + dmu
  }
  
  return(cbind(1:t_end, x_values, y_values))
}