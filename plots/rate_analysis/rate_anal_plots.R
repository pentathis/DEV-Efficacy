library(vioplot)

rate_anal <- function(bifurcation, max_values){
  
  
  DEV_begin <- matrix(NA, length(max_values), 100, dimnames = list(paste0('maxval', max_values),
                                                                     paste0('ts', seq(1, 100))))
  DEV_end <- matrix(NA, length(max_values), 100, dimnames = list(paste0('maxval', max_values),
                                                                   paste0('ts', seq(1, 100))))
  DEV_trend <- matrix(NA, length(max_values), 100, dimnames = list(paste0('maxval', max_values),
                                                                     paste0('ts', seq(1, 100))))
  
  for(m in max_values){
    for(i in 1:100){
      
      DEV_vals <- read.csv(paste0('data/rate_anal/',bifurcation,'/', bifurcation, '_DEV_',
                                  m, '_', i, '.csv'), row.names = 1)
      
      DEV_trend[paste0('maxval', m), i] <- cor(DEV_vals[,1], DEV_vals[,2], method = 'kendall')
      
      DEV_begin[paste0('maxval', m), i] <- DEV_vals[1, 2]
      
      DEV_end[paste0('maxval', m), i] <- DEV_vals[nrow(DEV_vals), 2]
    }
  }
  
  delta_DEV <- DEV_end - DEV_begin
  
  
  return(list(DEV_end, delta_DEV, DEV_trend))
}

violins <- function(DEV_data, letters, xlabel, bif_name, axislabels,  col1 = 'lightpink', col2 = 'red'){

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
  text(x = corners[2]+0.5, y = mean(corners[3:4]), bif_name, srt = 270, cex = 2)
  par(xpd = F)
  
  
  abline(h = 0, lty = 2)
  
  # Kendall tau 
  # vioplot(t(DEV_data[[3]]), col = 'cornsilk', lwd = 2, colMed = colors[1:nrow(DEV_data[[1]])], ylab = 'Kendall\'s tau',
  #         border = colors[1:nrow(DEV_data[[1]])], ylim = c(-1, 1), cex.axis = 1.3, xaxt = 'n', cex.lab = 3)
  # axis(side = 1, at = 1:nrow(DEV_data[[1]]), labels = substring(rownames(DEV_data[[1]]), first = 6))
  # mtext(expression(sigma), side = 1, line = 2.5, cex = 1)
  # 
  # abline(h = 0, lty = 2)
  
}


fold_rate_anal <- rate_anal('fold', 2:11)
transcritical_rate_anal <- rate_anal('transcritical', seq(0.5, 0.05, by = -0.05))
pitchfork_rate_anal <- rate_anal('pitchfork', seq(0.5, 5, by = 0.5))

par(mfrow = c(3, 2), cex.lab = 1.7, cex.axis = 1.3,  mai = c(0.6, 0.7, 0.6, 0.4))

violins(fold_rate_anal, c('A', 'B'), xlabel = expression(F[max]), bif_name = 'Fold', axislabels = 2:11, col1 = 'lightpink', col2 = 'red')
violins(transcritical_rate_anal,c('C', 'D'), xlabel = expression(c[min]), bif_name = 'Transcritical', axislabels = seq(0.5, 0.05, by = -0.05), col1 = 'plum1', col2 = 'purple')
violins(pitchfork_rate_anal, c('E', 'F'), xlabel = expression(a[max]), bif_name = 'Pitchfork', axislabels = seq(0.5, 5, by = 0.5), col1 = 'lightblue', col2 = 'blue')

# Save with size 1084 px width by 916 px height
