require(gplots)

# Calculate average DEV data 
replicates <- 100

fold_DEV_end_sum <- 0
fold_delta_DEV_sum <- 0
transcritical_DEV_end_sum <- 0
transcritical_delta_DEV_sum <- 0
pitchfork_DEV_end_sum <- 0
pitchfork_delta_DEV_sum <- 0

for(i in 1:replicates){
  
  DEV_end <- read.csv(paste0('data/rate_vs_ts_len/fold/fold_DEV_end', i, '.csv'), row.names=1)
  DEV_start <- read.csv(paste0('data/rate_vs_ts_len/fold/fold_DEV_start', i, '.csv'), row.names=1)
  fold_DEV_end_sum <- fold_DEV_end_sum + DEV_end
  fold_delta_DEV_sum <- fold_delta_DEV_sum + DEV_end - DEV_start
  
  DEV_end <- read.csv(paste0('data/rate_vs_ts_len/transcritical/transcritical_DEV_end', i, '.csv'), row.names=1)
  DEV_start <- read.csv(paste0('data/rate_vs_ts_len/transcritical/transcritical_DEV_start', i, '.csv'), row.names=1)
  transcritical_DEV_end_sum <- transcritical_DEV_end_sum + DEV_end
  transcritical_delta_DEV_sum <- transcritical_delta_DEV_sum + DEV_end - DEV_start

  DEV_end <- read.csv(paste0('data/rate_vs_ts_len/pitchfork/pitchfork_DEV_end', i, '.csv'), row.names=1)
  DEV_start <- read.csv(paste0('data/rate_vs_ts_len/pitchfork/pitchfork_DEV_start', i, '.csv'), row.names=1)
  pitchfork_DEV_end_sum <- pitchfork_DEV_end_sum + DEV_end
  pitchfork_delta_DEV_sum <- pitchfork_delta_DEV_sum + DEV_end - DEV_start


  
}

fold_delta_DEV_mean <- fold_delta_DEV_sum/replicates
fold_DEV_end_mean <- fold_DEV_end_sum/replicates

transcritical_delta_DEV_mean <- transcritical_delta_DEV_sum/replicates
transcritical_DEV_end_mean <- transcritical_DEV_end_sum/replicates

pitchfork_delta_DEV_mean <- pitchfork_delta_DEV_sum/replicates
pitchfork_DEV_end_mean <- pitchfork_DEV_end_sum/replicates

# Function to plot heatmap
DEV_heatmap <- function(data, breaks, labrow, labcol, showkey,
                            keytitle){
  
  heatmap.2(data.matrix(data),
            scale = 'none',
            margins = c(5, 5),
            trace = 'none', 
            col = colorpanel(144, '#fde725', '#21918c', '#440154'),
            breaks =  breaks,
            density.info = 'none', 
            Rowv = F, Colv = F, 
            dendrogram = 'none', 
            xlab = NA, 
            ylab = NA, 
            labRow = labrow,
            labCol = labcol, 
            cexRow = 2,
            cexCol = 2,
            cex.lab = 1.6,
            srtCol = 0,
            na.color = 'gray', 
            key=showkey,
            key.title = keytitle,
            key.xlab = NA,
            keysize = 1, 
            lhei = c(1, 5), 
            lwid = c(1, 10, 1), 
            lmat = rbind(c(5, 4, 2), c(6, 1, 3)),
            key.par = list(mar=c(3.5, 0, 3, 3.5), cex.main = 2))
}


# Fold bifurcation
DEV_heatmap(fold_DEV_end_mean, breaks = seq(0.5, 1.5, length.out = 145),
                labrow = c(seq(100, 900, 100), seq(1000, 10000, 1000)),
                labcol = seq(2, 11, 1), showkey = T,
            keytitle = '|DEV| at transition point')

mtext('Length of pre-transition time series', side = 4, line = -0.5, cex = 2, at = 0.4)
mtext(expression(F[max]), side = 1, line = 4.5, cex = 2, at = 0.44)
mtext('Fold bifurcation', side = 2, line = 1.5, cex = 2, at = 0.4)
mtext('A', side = 3, line = 2, at = -0.05, cex=2)

DEV_heatmap(fold_delta_DEV_mean, breaks = seq(0, 1, length.out = 145),
            labrow = c(seq(100, 900, 100), seq(1000, 10000, 1000)),
            labcol = seq(2, 11, 1),  showkey = T,
            keytitle = 'Δ |DEV|')

mtext('Length of pre-transition time series', side = 4, line = -0.5, cex = 2, at = 0.4)
mtext(expression(F[max]), side = 1, line = 4.5, cex = 2, at = 0.44)
mtext('B', side = 3, line = 2, at = -0.05, cex=2)


# transcritical bifurcation
DEV_heatmap(transcritical_DEV_end_mean, breaks = seq(0.5, 1.5, length.out = 145),
            labrow = c(seq(100, 900, 100), seq(1000, 10000, 1000)),
            labcol = seq(0.5, 0.05, by = -0.05),  showkey = F,
            keytitle = NA)

mtext('Length of pre-transition time series', side = 4, line = -0.5, cex = 2, at = 0.4)
mtext(expression(c[min]), side = 1, line = 4.5, cex = 2, at = 0.44)
mtext('Transcritical bifurcation', side = 2, line = 1.5, cex = 2, at = 0.4)
mtext('C', side = 3, line = 2, at = -0.05, cex=2)

DEV_heatmap(transcritical_delta_DEV_mean, breaks = seq(0, 1, length.out = 145),
            labrow = c(seq(100, 900, 100), seq(1000, 10000, 1000)),
            labcol = seq(0.5, 0.05, by = -0.05),  showkey = F,
            keytitle = NA)

mtext('Length of pre-transition time series', side = 4, line = -0.5, cex = 2, at = 0.4)
mtext(expression(c[min]), side = 1, line = 4.5, cex = 2, at = 0.44)
mtext('D', side = 3, line = 2, at = -0.05, cex=2)


# pitchfork bifurcation
DEV_heatmap(pitchfork_DEV_end_mean, breaks = seq(0.5, 1.5, length.out = 145),
            labrow = c(seq(100, 900, 100), seq(1000, 10000, 1000)),
            labcol = seq(0.5, 5, by = 0.5),  showkey = F,
            keytitle = NA)

mtext('Length of pre-transition time series', side = 4, line = -0.5, cex = 2, at = 0.4)
mtext(expression(a[max]), side = 1, line = 4.1, cex = 2, at = 0.44)
mtext('Pitchfork bifurcation', side = 2, line = 1.5, cex = 2, at = 0.4)
mtext('E', side = 3, line = 2, at = -0.05, cex=2)


DEV_heatmap(pitchfork_delta_DEV_mean, breaks = seq(0, 1, length.out = 145),
            labrow = c(seq(100, 900, 100), seq(1000, 10000, 1000)),
            labcol =seq(0.5, 5, by = 0.5),  showkey = F,
            keytitle = NA)


mtext('Length of pre-transition time series', side = 4, line = -0.5, cex = 2, at = 0.4)
mtext(expression(a[max]), side = 1, line = 4.1, cex = 2, at = 0.44)
mtext('F', side = 3, line = 2, at = -0.05, cex=2)




