# Noy-Meir Model - Fold bifurcation
nm_fold_eigenval <- function(t_end = 10000, f_min = 0, f_max = 2){
  
  N_values <- rep(NA, t_end)
  eigenvalues <- rep(NA, t_end)
  
  f <- f_min
  df <- (f_max - f_min)/t_end
  
  N_values[1] <- 7.5
  
  for(i in 2:t_end){
    N <- N_values[i-1]
    
    N_values[i] <- N*exp(0.75 - 0.1*N) - (f*(N^2/(N^2 + 0.75^2)))
    
    eigenvalues[i-1] <- exp(0.75- N/10)*(1-N/10) - 2*f*0.75^2/((N^2+0.75^2)^2)
    
    f <- f + df
  }
  
  return(eigenvalues)
}


# Lotka-Volterra Model - Transcritical bifurcation
lv_transcritical_eigenval <- function(t_end = 1e4, r = 0.5, c_min = 0.5, c_max = 1.5){
  
  x_values <- rep(NA, t_end)
  y_values <- rep(NA, t_end)
  eigenvalues <- rep(NA, t_end)
  
  x_values[1] <- 0.67
  y_values[1] <- 0.11
  
  c <- c_max
  dc <- -(c_max - c_min)/t_end
  
  for(i in 2:t_end){
    
    x <- x_values[i-1]
    y <- y_values[i-1]
    
    jacobian <- matrix(c(r + 1 - 2*r*x - c*y, -c*x, c*y, c*x),
                       nrow=2, ncol = 2, byrow = T)
    
    eigenvalues[i] <- Mod(eigen(jacobian)$values[1])
    
    x_values[i] <- (r+1)*x - r*x^2 - c*x*y
    y_values[i] <- c*x*y
    
    c <- c + dc
  }
  
  return(eigenvalues)
}


# Reduced Lorenz Model - Pitchfork bifurcation
lorenz_pitchfork_eigenval <- function(t_end = 1e4, h = 0.5, a_min = -1, a_max = 1){
  
  x_values <- rep(NA, t_end)
  y_values <- rep(NA, t_end)
  eigenvalues <- rep(NA, t_end)
  
  x_values[1] <- 0
  y_values[1] <- 0
  
  a <- a_min
  da <- (a_max - a_min)/t_end
  
  for(i in 2:t_end){
    
    x <- x_values[i-1]
    y <- y_values[i-1]
    
    jacobian <- matrix(c(1+a*h-h*y, -h*x, 2*h*x, 1-h),
                       nrow=2, ncol = 2, byrow = T)
    
    eigenvalues[i] <- Re(eigen(jacobian)$values[1])
    
    x_values[i] <- (1+a*h)*x - h*x*y
    y_values[i] <- (1 - h)*y + h*x^2 
    
    a <- a + da
  }
  
  return(eigenvalues)
}

