library(gplots)


source('sensitivity_heatmap.R')


# Read all the files
for(E in 1:10){
  for(tau in -1:-10){
    
    tryCatch({
      assign(paste0('discrete_fold', E, -tau), read.csv(paste0('data/sensitivity_anal/discrete_sensitivity_anal/discrete_fold/discrete_fold', E, tau, '.csv'), row.names = 1))
      assign(paste0('discrete_transcritical', E, -tau), read.csv(paste0('data/sensitivity_anal/discrete_sensitivity_anal/discrete_transcritical/discrete_transcritical', E, tau, '.csv'), row.names = 1))
      assign(paste0('discrete_pitchfork', E, -tau), read.csv(paste0('data/sensitivity_anal/discrete_sensitivity_anal/discrete_pitchfork/discrete_pitchfork', E, tau, '.csv'), row.names = 1))
      
    },
    error = function(e){#If an error is there, carry on
    })
  }
}

transition_point_heatmap('discrete_fold')
transition_point_heatmap('discrete_transcritical')
transition_point_heatmap('discrete_pitchfork')

delta_DEV_heatmap('discrete_fold')
delta_DEV_heatmap('discrete_transcritical')
delta_DEV_heatmap('discrete_pitchfork')

