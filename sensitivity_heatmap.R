# Functions to draw heatmaps for sensitivity analysis


transition_point_heatmap <- function(bifurcation, breaks =  seq(0.5, 1.5, length.out = 145)){
  
  DEV_at_transition <- matrix(NA, nrow = 10, ncol = 10, dimnames = list(paste0('E',1:10), paste0('tau', -1:-10)))
  
  for(E in 1:10){
    for(tau in 1:10){
      
      tryCatch({
        DEV_values <- get(paste0(bifurcation, E, tau))
        DEV_at_transition[E, tau] <- DEV_values[nrow(DEV_values), 2]
      }, error = function(e){
        DEV_at_transition[E, tau] <- NA
      })
    }
  }
  
  heatmap.2(DEV_at_transition, 
            scale = 'none',
            margins = c(4, 4),
            trace = 'none', 
            col = colorpanel(144, '#fde725', '#21918c', '#440154'),
            breaks = breaks,
            density.info = 'none', 
            Rowv = F, Colv = F, 
            dendrogram = 'none', 
            xlab = NA, 
            ylab = NA, 
            labRow = 1:10,
            labCol = 1:10, 
            cexRow = 2,
            cexCol = 2,
            cex.lab = 1.6,
            srtCol = 0,
            na.color = 'gray', 
            key.title = '|DEV| at transition point',
            key.xlab = NA,
            keysize = 1, 
            lhei = c(1, 5), 
            lwid = c(1, 10, 1), 
            lmat = rbind(c(5, 4, 2), c(6, 1, 3)),
            key.par = list(mar=c(3.5, 0, 3, 3.5), cex.main = 2))
  mtext('E', side = 4, line = -1.5, cex = 2, at = 0.4)
  mtext(expression(tau), side = 1, line = 4.2, cex = 2, at = 0.44)
}


kendall_tau_heatmap <- function(bifurcation, breaks =  seq(-1, 1, length.out = 145)){
  
  kendalltau<- matrix(NA, nrow = 10, ncol = 10, dimnames = list(paste0('E',1:10), paste0('tau', -1:-10)))
  
  for(E in 1:10){
    for(tau in 1:10){
      
      tryCatch({
        DEV_values <- get(paste0(bifurcation, E, tau))
        kendalltau[E, tau] <- cor(DEV_values[,1], DEV_values[, 2])
      }, error = function(e){
        kendalltau[E, tau] <- NA
      })
    }
  }
  
  heatmap.2(kendalltau, 
            scale = 'none',
            margins = c(4, 4),
            trace = 'none', 
            col = colorpanel(144, '#fde725', '#21918c', '#440154'),
            breaks = breaks,
            density.info = 'none', 
            Rowv = F, Colv = F, 
            dendrogram = 'none', 
            xlab = NA, 
            ylab = NA, 
            labRow = 1:10,
            labCol = 1:10, 
            cexCol = 1.6,
            cexRow = 1.6,
            cex.lab = 1.6,
            srtCol = 0,
            na.color = 'gray', 
            key.title = expression('Kendall\'s '*tau*''),
            key.xlab = NA,
            keysize = 1, 
            lhei = c(1, 5), 
            lwid = c(1, 10, 1), 
            lmat = rbind(c(5, 4, 2), c(6, 1, 3)),
            key.par = list(mar=c(3.5, 0, 3, 3.5), cex.main = 2),
            symkey = F)
  mtext('E', side = 4, line = -1.5, cex = 2, at = 0.4)
  mtext(expression(tau), side = 1, line = 4.2, cex = 2, at = 0.44)
}


delta_DEV_heatmap <- function(bifurcation, breaks =  seq(0, 1, length.out = 145)){
  
  DEV_at_transition <- matrix(NA, nrow = 10, ncol = 10, dimnames = list(paste0('E',1:10), paste0('tau', -1:-10)))
  
  for(E in 1:10){
    for(tau in 1:10){
      
      tryCatch({
        DEV_values <- get(paste0(bifurcation, E, tau))
        DEV_at_transition[E, tau] <- DEV_values[nrow(DEV_values), 2] - DEV_values[1, 2] 
      }, error = function(e){
        DEV_at_transition[E, tau] <- NA
      })
    }
  }
  
  heatmap.2(DEV_at_transition, 
            scale = 'none',
            margins = c(4, 4),
            trace = 'none', 
            col = colorpanel(144, '#fde725', '#21918c', '#440154'),
            breaks = breaks,
            density.info = 'none', 
            Rowv = F, Colv = F, 
            dendrogram = 'none', 
            xlab = NA, 
            ylab = NA, 
            labRow = 1:10,
            labCol = 1:10, 
            cexRow = 1.6,
            cexCol = 1.6,
            cex.lab = 1.6,
            srtCol = 0,
            na.color = 'gray', 
            key.title = 'Δ |DEV|',
            key.xlab = NA,
            keysize = 1, 
            lhei = c(1, 5), 
            lwid = c(1, 10, 1), 
            lmat = rbind(c(5, 4, 2), c(6, 1, 3)),
            key.par = list(mar=c(3.5, 0, 3, 3.5), cex.main = 2))
  mtext('E', side = 4, line = -1.5, cex = 2, at = 0.4)
  mtext(expression(tau), side = 1, line = 4.2, cex = 2, at = 0.44)
}
