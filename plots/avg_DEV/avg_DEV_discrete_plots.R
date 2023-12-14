source('discrete_model_functions.R')


fold_DEV_mean <- read.csv('data/avg_DEV_discrete/discrete_fold.csv', row.names = 1)
transcritical_DEV_mean <- read.csv('data/avg_DEV_discrete/discrete_transcritical.csv', row.names = 1)
pitchfork_DEV_mean <- read.csv('data/avg_DEV_discrete/discrete_pitchfork.csv', row.names = 1)


# Make the plots
par(mfrow=c(2, 2), cex.axis = 1.4, cex.lab = 2, mai = c(0.8, 1.2, 0.28, 0.5))

nmts <- nm_fold()
lvts <- (lv_transcritical()[,2] + 1.2)*(10/2.4)
rlts <- (lorenz_pitchfork()[,2] + 1.2)*(10/2.4)

nmcol <- colorRampPalette(c('lightpink', 'red'))(1e4)
lvcol <- colorRampPalette(c('plum1', 'purple'))(1e4)
rlcol <- colorRampPalette(c('lightblue', 'blue'))(1e4)

# Make a dummy plot
plot(-1, -1, ylim = c(0, 10), xlim = c(0, 1e4), ylab = 'N', xlab = NA)

# Plot with color gradient
for(i in 2:1e4){
        points(c(i-1, i), c(nmts[i-1], nmts[i]), type = 'l', col = nmcol[i])
        points(c(i-1, i), c (lvts[i-1], lvts[i]), type = 'l', col = lvcol[i])
        points(c(i-1, i), c(rlts[i-1], rlts[i]), type = 'l', col = rlcol[i])
        
}
axis(side = 4, labels = seq(-1.2, 1.2, length.out = 6), at = seq(0, 10, length.out = 6))
mtext('x', side = 4, line = 2.5, cex = 1.3)
mtext('A', side = 3, at = -2000, line = 0, cex = 2)


legend(x = 'bottomleft', col = c('red', 'purple', 'blue'), legend = c('Noy-Meir Model',
                                                    'Lotka-Volterra Model',
                                                    'Reduced Lorenz Model'),
       lty = 1, lwd = 4, cex = 1.5)

nmcol <- colorRampPalette(c('lightpink', 'red'))(nrow(fold_DEV_mean))
lvcol <- colorRampPalette(c('plum1', 'purple'))(nrow(transcritical_DEV_mean))
rlcol <- colorRampPalette(c('lightblue', 'blue'))(nrow(pitchfork_DEV_mean))

# Magnitude of DEV

# Make a dummy plot
plot(-1, -1, ylim = c(0, 1.2), xlim = c(0, 1e4), ylab = '|DEV|', xlab = NA)

# Plot with color gradient
for(i in 2:nrow(fold_DEV_mean)){
        points( c(fold_DEV_mean[i-1, 1], fold_DEV_mean[i, 1]),
                c(fold_DEV_mean[i-1, 2], fold_DEV_mean[i, 2]), type = 'l', col = nmcol[i], lwd = 4)
}

for(i in 2:nrow(transcritical_DEV_mean)){
        
        points(c(transcritical_DEV_mean[i-1, 1], transcritical_DEV_mean[i, 1]),
               c(transcritical_DEV_mean[i-1, 2], transcritical_DEV_mean[i, 2]), type = 'l', col = lvcol[i], lwd = 4)
        
        points(c(pitchfork_DEV_mean[i-1, 1], pitchfork_DEV_mean[i, 1]), 
               c(pitchfork_DEV_mean[i-1, 2], pitchfork_DEV_mean[i, 2]), type = 'l', col = rlcol[i], lwd = 4)
}
abline(h = 1, col = 'gray', lty = 2, lwd = 2)
mtext('B', side = 3, at = -2000, line = 0, cex = 2)


# Real part of DEV

# Make a dummy plot
plot(-1, -1, ylim = c(0, 1.2), xlim = c(0, 1e4), ylab = 'Re(DEV)', xlab = 'Time')

# Plot with color gradient
for(i in 2:nrow(fold_DEV_mean)){
        points( c(fold_DEV_mean[i-1, 1], fold_DEV_mean[i, 1]),
                c(fold_DEV_mean[i-1, 3], fold_DEV_mean[i, 3]), type = 'l', col = nmcol[i], lwd = 4)
}

for(i in 2:nrow(transcritical_DEV_mean)){
        
        points(c(transcritical_DEV_mean[i-1, 1], transcritical_DEV_mean[i, 1]),
               c(transcritical_DEV_mean[i-1, 3], transcritical_DEV_mean[i, 3]), type = 'l', col = lvcol[i], lwd = 4)
        
        points(c(pitchfork_DEV_mean[i-1, 1], pitchfork_DEV_mean[i, 1]), 
               c(pitchfork_DEV_mean[i-1, 3], pitchfork_DEV_mean[i, 3]), type = 'l', col = rlcol[i], lwd = 4)
}
abline(h = 1, col = 'gray', lty = 2, lwd = 2)
mtext('C', side = 3, at = -2000, line = 0, cex = 2)


# Imaginary part of DEV

# Make a dummy plot
plot(-1, -1, ylim = c(-0.5, 0.5), xlim = c(0, 1e4), ylab = 'Im(DEV)', xlab = 'Time')

# Plot with color gradient
for(i in 2:nrow(fold_DEV_mean)){
        points( c(fold_DEV_mean[i-1, 1], fold_DEV_mean[i, 1]),
                c(fold_DEV_mean[i-1, 4], fold_DEV_mean[i, 4]), type = 'l', col = nmcol[i], lwd = 4)
}

for(i in 2:5000){
        
        points(c(transcritical_DEV_mean[i-1, 1], transcritical_DEV_mean[i, 1]),
               c(transcritical_DEV_mean[i-1, 4], transcritical_DEV_mean[i, 4]), type = 'l', col = lvcol[i], lwd = 4)
        
        points(c(pitchfork_DEV_mean[i-1, 1], pitchfork_DEV_mean[i, 1]), 
               c(pitchfork_DEV_mean[i-1, 4], pitchfork_DEV_mean[i, 4]), type = 'l', col = rlcol[i], lwd = 4)
        
}
abline(h= 0, col = 'gray', lty = 2, lwd = 2)
mtext('D', side = 3, at = -2000, line = 0, cex = 2)
