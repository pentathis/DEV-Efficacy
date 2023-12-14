correlations <- c(0, 0.2, 0.4, 0.6, 0.8)

# Read the saved files 

for(correlation in correlations){
  
  assign(paste0('fold_DEV', correlation), 
         read.csv(paste0('data/avg_DEV_red_noise/rn_fold/rn_fold', correlation, '.csv'), row.names = 1))
  assign(paste0('pd_DEV', correlation), 
         read.csv(paste0('data/avg_DEV_red_noise/rn_pd/rn_pd', correlation, '.csv'), row.names = 1))
  assign(paste0('ns_DEV', correlation), 
         read.csv(paste0('data/avg_DEV_red_noise/rn_ns/rn_ns', correlation, '.csv'), row.names = 1))
}


# Plots
par(mfrow=c(5, 3), mai = c(0.6, 0.6, 0.2, 0.3), cex.lab = 1.5)

for(correlation in correlations){
  
  if(correlation == max(correlations)){
    xlabel = 'Time'
  }else{
    xlabel = NA
  }

  plot(get(paste0('fold_DEV', correlation))[,1], 
       get(paste0('fold_DEV', correlation))[,2], 
       type = 'l', 
       ylab = '|DEV|',
       xlab = xlabel, 
       ylim = c(0, 1.1),
       lwd = 3,
       col = 'blue')
  abline(h = 1, lty = 2, col = 'gray')
    
  plot(get(paste0('pd_DEV', correlation))[,1], 
       get(paste0('pd_DEV', correlation))[,2], 
       type = 'l', 
       ylab = NA,
       xlab = xlabel, 
       ylim = c(0, 1.1),
       lwd = 3,
       col = 'red')
  abline(h = 1, lty = 2, col = 'gray')
  
  plot(get(paste0('ns_DEV', correlation))[,1], 
       get(paste0('ns_DEV', correlation))[,2], 
       type = 'l', 
       ylab = NA,
       xlab = xlabel, 
       ylim = c(0, 1.1),
       lwd = 3,
       col = 'purple')
  abline(h = 1, lty = 2, col = 'gray')
  mtext(paste0('corr = ', correlation), side = 4, line = 0.8)

}
