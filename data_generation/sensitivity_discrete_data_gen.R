library(rEDM)
library(compiler)


source('univariate_DEV.R')
source('discrete_model_functions.R')

replicates <- 100


# The commented code below is meant to generate the time series and save them.
# It need not be run again as it has been run once and the time series have been saved.


# # Generate time series
# for(i in 1:replicates){
#   assign(paste0('discrete_fold', i), nm_fold()[1:9200])
# 
# 
#   usable <- F
# 
#   while(usable == F){
# 
#     ts <- lv_transcritical()[1:5000, 2]
# 
#     if(any(is.na(ts)) == T || any(is.na(ts)) > 1.5){
#       usable = F
#     }else{usable = T}
#   }
# 
# 
#   assign(paste0('discrete_transcritical', i), ts)
# 
#   assign(paste0('discrete_pitchfork', i), lorenz_pitchfork()[1:5000, 2])
# }
# 
# # # Save the time series
# discrete_fold <- matrix(NA, nrow = 9200, ncol = 100)
# discrete_transcritical <- matrix(NA, nrow = 5000, ncol = 100)
# discrete_pitchfork <- matrix(NA, nrow = 5000, ncol = 100)
# 
# for(i in 1:replicates){
#   discrete_fold[,i] <-get(paste0('discrete_fold', i))
#   discrete_transcritical[,i] <- get(paste0('discrete_transcritical', i))
#   discrete_pitchfork[,i] <- get(paste0('discrete_pitchfork', i))
# }
# 
# write.csv(discrete_fold, 'data/sensitivity_anal/discrete_sensitivity_anal/time_series/discrete_fold')
# write.csv(discrete_pitchfork, 'data/sensitivity_anal/discrete_sensitivity_anal/time_series/discrete_pitchfork')
# write.csv(discrete_transcritical, 'data/sensitivity_anal/discrete_sensitivity_anal/time_series/discrete_transcritical')

# Retrieve the saved time series
discrete_fold <- read.csv('data/sensitivity_anal/discrete_sensitivity_anal/time_series/discrete_fold', row.names = 1)
discrete_transcritical <- read.csv('data/sensitivity_anal/discrete_sensitivity_anal/time_series/discrete_transcritical', row.names = 1)
discrete_pitchfork <- read.csv('data/sensitivity_anal/discrete_sensitivity_anal/time_series/discrete_pitchfork', row.names = 1)

for(i in 1:replicates){
  assign(paste0('discrete_fold', i), discrete_fold[,i])
  assign(paste0('discrete_transcritical', i), discrete_transcritical[,i])
  assign(paste0('discrete_pitchfork', i), discrete_pitchfork[,i])
}


# Perform DEV analysis
for(E in 1:10){
  for(tau in -1:-10){
    print(c(E, tau))
    
   
      tryCatch({
        for(i in 1:replicates){
        
          assign(paste0('discrete_fold_DEV', '_', i, '_', E, tau), DEV(get(paste0('discrete_fold', i)),
                                                          E = E, tau = tau,
                                                          theta = seq(0, 2.5, 0.5),
                                                          winsize = 250, stepsize = 50))
          
          assign(paste0('discrete_transcritical_DEV', '_', i, '_',  E, tau), DEV(get(paste0('discrete_transcritical', i)),
                                                                   E = E, tau = tau,
                                                                   theta = seq(0, 2.5, 0.5),
                                                                   winsize = 250, stepsize = 50))
          
          assign(paste0('discrete_pitchfork_DEV', '_', i, '_',  E, tau), DEV(get(paste0('discrete_pitchfork', i)),
                                                               E = E, tau = tau,
                                                               theta = seq(0, 2.5, 0.5),
                                                               winsize = 250, stepsize = 50))
        }
        
        fold_DEV_sum <- 0
        transcritical_DEV_sum <- 0
        pitchfork_DEV_sum <- 0
        
        for(i in 1:replicates){
          fold_DEV_sum <- fold_DEV_sum + get(paste0('discrete_fold_DEV', '_', i, '_',  E, tau))
          transcritical_DEV_sum <- transcritical_DEV_sum + get(paste0('discrete_transcritical_DEV', '_', i, '_',  E, tau))
          pitchfork_DEV_sum <- pitchfork_DEV_sum + get(paste0('discrete_pitchfork_DEV', '_', i, '_',  E, tau))
        }
        
        assign(paste0('discrete_fold_DEV', E, tau), fold_DEV_sum/replicates)
        assign(paste0('discrete_transcritical_DEV', E, tau), transcritical_DEV_sum/replicates)
        assign(paste0('discrete_pitchfork_DEV', E, tau), pitchfork_DEV_sum/replicates)
        
        
      }, error = function(e){})
    
    
    
    tryCatch({
      
      
      
      write.csv(get(paste0('discrete_fold_DEV', E, tau)), paste0('data/sensitivity_anal/discrete_sensitivity_anal/discrete_fold/discrete_fold', E, tau, '.csv'))
      write.csv(get(paste0('discrete_transcritical_DEV', E, tau)), paste0('data/sensitivity_anal/discrete_sensitivity_anal/discrete_transcritical/discrete_transcritical', E, tau, '.csv'))
      write.csv(get(paste0('discrete_pitchfork_DEV', E, tau)), paste0('data/sensitivity_anal/discrete_sensitivity_anal/discrete_pitchfork/discrete_pitchfork', E, tau, '.csv'))
      
      }, error = function(e){})
    
    
  }
}
      