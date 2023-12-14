library(vioplot)


# Throughout this file, correlations refer to noise correlation

rate_anal <- function(bifurcation, correlations = seq(0, 0.9, by = 0.1)){
  
  
  DEV_begin <- matrix(NA, length(correlations), 100, dimnames = list(paste0('corr', correlations),
                                                                   paste0('ts', seq(1, 100))))
  DEV_end <- matrix(NA, length(correlations), 100, dimnames = list(paste0('corr', correlations),
                                                                 paste0('ts', seq(1, 100))))
  DEV_trend <- matrix(NA, length(correlations), 100, dimnames = list(paste0('corr', correlations),
                                                                   paste0('ts', seq(1, 100))))
  
  for(m in correlations){
    for(i in 1:100){
      
      DEV_vals <- read.csv(paste0('data/red_noise/rn_',bifurcation,'/rn_', bifurcation, 
                                  m, '-', i, '.csv'), row.names = 1)
      
      DEV_trend[paste0('corr', m), i] <- cor(DEV_vals[,1], DEV_vals[,2], method = 'kendall')
      
      DEV_begin[paste0('corr', m), i] <- DEV_vals[1, 2]
      
      DEV_end[paste0('corr', m), i] <- DEV_vals[nrow(DEV_vals), 2]
    }
  }
  
  delta_DEV <- DEV_end - DEV_begin
  
  
  return(list(DEV_end, delta_DEV, DEV_trend))
}

violins <- function(DEV_data, letters, xlabel = NA, bif_name, axislabels = seq(0, 0.9, by = 0.1) 
                      ,  col1 = 'lightpink', col2 = 'red'){
  
  colors <- colorRampPalette(c(col1, col2))(10)
  
  # DEV and transition point
  vioplot(t(DEV_data[[1]]), 
          col = 'cornsilk', 
          lwd = 2, 
          colMed = colors[1:nrow(DEV_data[[1]])], 
          ylab = '|DEV| at transition point', 
          border = colors[1:nrow(DEV_data[[1]])],
          ylim = c(0, 1.01),
          xaxt = 'n',
          drawRect = F)
  
  axis(side = 1,
       at = 1:nrow(DEV_data[[1]]),
       labels = axislabels)
  mtext(xlabel,
        side = 1, 
        line = 3, 
        cex = 1.3)
  abline(h = 1, lty = 2)
  mtext(letters[1], at = -0.4, line = 1.7, cex = 1.5)
  
  # Delta DEV 
  vioplot(t(DEV_data[[2]]), 
          col = 'cornsilk', 
          lwd = 2, 
          colMed = colors[1:nrow(DEV_data[[1]])],
          ylab = 'Δ |DEV|',
          border = colors[1:nrow(DEV_data[[1]])], 
          ylim = c(0, 1.1),  
          xaxt = 'n', 
          drawRect = F)
  axis(side = 1, 
       at = 1:nrow(DEV_data[[1]]), 
       labels = axislabels)
  mtext(xlabel, side = 1, line = 3, cex = 1.3)
  mtext(letters[2], at = -0.4, line = 1.7, cex = 1.5)
  
  # Add bifurcation name on right side
  corners = par('usr')
  par(xpd = T)
  text(x = corners[2]+0.6, y = mean(corners[3:4]), bif_name, srt = 270, cex = 2)
  par(xpd = F)
  
  
  abline(h = 0, lty = 2)
  
  #Kendall tau
  # vioplot(t(DEV_data[[3]]), col = 'cornsilk', lwd = 2, colMed = colors[1:nrow(DEV_data[[1]])], ylab = 'Kendall\'s tau',
  #         border = colors[1:nrow(DEV_data[[1]])], ylim = c(-1, 1), cex.axis = 1.3, xaxt = 'n', cex.lab = 3)
  # axis(side = 1, at = 1:nrow(DEV_data[[1]]), labels = substring(rownames(DEV_data[[1]]), first = 6))
  # mtext(expression(sigma), side = 1, line = 2.5, cex = 1)
  # 
  # abline(h = 0, lty = 2)
  
}


fold_rate_anal <- rate_anal('fold')
transcritical_rate_anal <- rate_anal('pd')
pitchfork_rate_anal <- rate_anal('ns')

par(mfrow = c(3, 2), cex.lab = 1.7, cex.axis = 1.3,  mai = c(0.6, 0.7, 0.6, 0.4))

violins(fold_rate_anal, c('A', 'B'), bif_name = 'Fold', col1 = 'lightpink', col2 = 'red')
violins(transcritical_rate_anal, c('C', 'D'), bif_name = 'Period-doubling', col1 = 'plum1', col2 = 'purple')
violins(pitchfork_rate_anal, c('E', 'F'), xlabel = 'Noise correlation', bif_name = 'Neimark-Sacker', col1 = 'lightblue', col2 = 'blue')

# Save with size 1084 px width by 916 px height
