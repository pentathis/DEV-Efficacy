library(rEDM)
library(compiler)


source('univariate_DEV.R')
source('discrete_model_functions.R')


roundto50<- function(x){
  if(x%%50 > 25){
    return(x + 50 - x%%50)
  }else{
    return(x - x%%50 )
  }
}

replicates <- 100


# Fold bifurcation
f_max_values = seq(2, 11)

# generate time series
for(j in f_max_values){
  for(i in 1:replicates){
    
    assign(paste0('fold_ts_', j, '_', i), nm_fold(f_max = j))
  }
}


# Calculate and save DEV
for(j in f_max_values){
  for(i in 1:replicates){
    print(c(j, i))
    
    fold_DEV <-  DEV(get(paste0('fold_ts_', j, '_', i))[1:roundto50(18400/j)],
                                               E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                               winsize = 250, stepsize = 50)
    
    filename <- paste0('data/rate_anal/fold/fold_DEV_', j, '_', i, '.csv')
    
    write.csv(fold_DEV, filename)
 
  }
}


# Transcritical bifurcation
c_min_values <- seq(0.5, 0.05, by = -0.05)

# Generate time series
for(j in c_min_values){
  for(i in 1:replicates){
    
    print(c(j, i))
    
    usable <- F
    
    while(usable == F){
      
      ts <-  lv_transcritical(c_min = j)[,2]
      
      if(any(is.na(ts)) == T || any(is.na(ts)) > 1.5){
        usable = F
      }else{usable = T}
    }
    
    assign(paste0('transcritical_ts_', j, '_', i), ts)
    
  }
}


# Calculate and save DEV
for(j in c_min_values){
  for(i in 1:replicates){

    print(c(j, i))

    transcritical_DEV <-  DEV(get(paste0('transcritical_ts_', j, '_', i))[1:roundto50(5000/(1.5 - j))],
                     E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                     winsize = 250, stepsize = 50)

    filename <- paste0('data/rate_anal/transcritical/transcritical_DEV_', j, '_', i, '.csv')

    write.csv(transcritical_DEV, filename)

  }
}


# Pitchfork bifurcation
a_max_values <- seq(0.5, 5, by = 0.5)

# Generate time series
for(j in a_max_values){
  for(i in 1:replicates){
    
    assign(paste0('pitchfork_ts_', j, '_', i), lorenz_pitchfork(a_max = j)[,2])
  }
}

# Calculate and save DEV
for(j in a_max_values){
  for(i in 1:replicates){

    print(c(j, i))

    pitchfork_DEV <-  DEV(get(paste0('pitchfork_ts_', j, '_', i))[1:roundto50(1e4/(j+1))],
                     E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                     winsize = 250, stepsize = 50)

    filename <- paste0('data/rate_anal/pitchfork/pitchfork_DEV_', j, '_', i, '.csv')

    write.csv(pitchfork_DEV, filename)
  }
}
     