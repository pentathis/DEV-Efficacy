library(rEDM)
library(compiler)


source('discrete_model_functions.R')
source('univariate_DEV.R')
source('multivariate_DEV.R')


replicates <- 100
# generate the time series
for(i in 1:replicates){
  assign(paste0('discrete_fold_ts', i), nm_fold())
  
  # The below block ensures that there are no NaN elements in the time series
  done <- FALSE
  while(done == FALSE){
    discrete_transcritical_ts <- lv_transcritical()
    if(NaN %in% discrete_transcritical_ts[, 2] == FALSE){
      assign(paste0('discrete_transcritical_ts', i), discrete_transcritical_ts)
      done <-  TRUE
    }
  }
  
  assign(paste0('discrete_pf_ts', i), lorenz_pitchfork())
}


# Calculate DEV trends
for(i in 1:replicates){
  print(i)
  
  assign(paste0('discrete_fold_DEV', i), DEV(get(paste0('discrete_fold_ts', i))[seq(1, 9200, length.out=100)],
                                             E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                             winsize = 20, stepsize = 4))
  
  assign(paste0('discrete_transcritical_DEV', i), DEV(get(paste0('discrete_transcritical_ts', i))[seq(1, 5000, length.out=100), 2],
                                                      E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                                      winsize = 20, stepsize = 4))
  
  assign(paste0('discrete_pf_DEV', i), DEV(get(paste0('discrete_pf_ts', i))[seq(1, 5000, length.out=100), 2],
                                           E = 2, tau = -1, theta = seq(0, 2.5, 0.5),
                                           winsize = 20, stepsize = 4))
  
  
}


# Calculate the average over 100 time series
discrete_fold_DEV_sum <- 0
discrete_transcritical_DEV_sum <- 0
discrete_pitchfork_DEV_sum <- 0


for(i in 1:replicates){
  
  discrete_fold_DEV_sum <- discrete_fold_DEV_sum + get(paste0('discrete_fold_DEV', i))
  discrete_transcritical_DEV_sum <- discrete_transcritical_DEV_sum + get(paste0('discrete_transcritical_DEV', i))
  discrete_pitchfork_DEV_sum <- discrete_pitchfork_DEV_sum + get(paste0('discrete_pf_DEV', i))
}


discrete_fold_mean_DEV <- discrete_fold_DEV_sum/replicates
discrete_transcritical_mean_DEV <- discrete_transcritical_DEV_sum/replicates
discrete_pitchfork_mean_DEV <- discrete_pitchfork_DEV_sum/replicates


# Save the average values
write.csv(discrete_fold_mean_DEV, 'data/avg_DEV_sparse_discrete/sparse_discrete_fold.csv')
write.csv(discrete_transcritical_mean_DEV, 'data/avg_DEV_sparse_discrete/sparse_discrete_transcritical.csv')
write.csv(discrete_pitchfork_mean_DEV, 'data/avg_DEV_sparse_discrete/sparse_discrete_pitchfork.csv')


# Calculate the standard deviation
discrete_fold_DEV_square_sum <- 0
discrete_transcritical_DEV_square_sum <- 0
discrete_pitchfork_DEV_square_sum <- 0

for(i in 1:replicates){
  discrete_fold_DEV_square_sum <- discrete_fold_DEV_square_sum + get(paste0('discrete_fold_DEV', i))^2
  discrete_transcritical_DEV_square_sum <- discrete_transcritical_DEV_square_sum + get(paste0('discrete_transcritical_DEV', i))^2
  discrete_pitchfork_DEV_square_sum <- discrete_pitchfork_DEV_square_sum + get(paste0('discrete_pf_DEV', i))^2
}

discrete_fold_DEV_sd <- sqrt(discrete_fold_DEV_square_sum/replicates - discrete_fold_mean_DEV^2)
discrete_transcritical_DEV_sd <- sqrt(discrete_transcritical_DEV_square_sum/replicates - discrete_transcritical_mean_DEV^2)
discrete_pitchfork_DEV_sd <- sqrt(discrete_pitchfork_DEV_square_sum/replicates - discrete_pitchfork_mean_DEV^2)


# Save the standard deviation values
write.csv(discrete_fold_DEV_sd, 'data/sd_DEV_sparse_discrete/sparse_discrete_fold.csv')
write.csv(discrete_transcritical_DEV_sd, 'data/sd_DEV_sparse_discrete/sparse_discrete_transcritical.csv')
write.csv(discrete_pitchfork_DEV_sd, 'data/sd_DEV_sparse_discrete/sparse_discrete_pitchfork.csv')
