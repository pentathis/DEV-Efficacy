source('discrete_model_functions.R')
source('analytical_eigenvalues.R')


fold_DEV_mean <- read.csv('data/avg_DEV_discrete/discrete_fold.csv', row.names = 1)
transcritical_DEV_mean <- read.csv('data/avg_DEV_discrete/discrete_transcritical.csv', row.names = 1)
pitchfork_DEV_mean <- read.csv('data/avg_DEV_discrete/discrete_pitchfork.csv', row.names = 1)


# make the plots
par(mfrow=c(1, 4), cex.axis = 1.7, cex.lab = 2.2, mai = c(0.6, 0.65, 0.5, 0.15))

nmts <- nm_fold()
lvts <- (lv_transcritical()[,2] + 1.2)*(10/2.4)
rlts <- (lorenz_pitchfork()[,2] + 1.2)*(10/2.4)


# Make a dummy plot
plot(-1, -1, ylim = c(0, 10), xlim = c(0, 1e4), ylab = 'N', xlab = 'Time')

# Plot the time series
lines(1:10000, nmts, col = 'red')
lines(1:10000, lvts, col = 'purple')
lines(1:10000, rlts, col = 'blue')

axis(side = 4, labels = seq(-1.2, 1.2, length.out = 6), at = seq(0, 10, length.out = 6))
mtext('x', side = 4, line = 1.8, cex = 1.3)
mtext('A', side = 3, at = -2000, line = 0.5, cex = 2)
mtext('Time series', side = 3, at = 5000, line = 1, cex=1.5)

legend(x ='bottomleft', col = c('red', 'purple', 'blue'), legend = c('Noy-Meir Model',
                                                                      'Lotka-Volterra Model',
                                                                     'Reduced Lorenz Model'),
                                                                      lty = 1, lwd = 4, cex = 1.3)

# Fold bifurcation
plot(1:10000, nm_fold_eigenval(), type = 'l', ylim = c(0, 1.5), 
     col='black', lwd=3, ylab = NA, xlab = 'F', xaxt='n')
axis(1, at=c(0, 5000, 9999), labels = c(0, 1, 2))

lines(fold_DEV_mean[, 1], fold_DEV_mean[,2], type = 'l', col = 'red', lwd = 3)
abline(h = 1, col = 'gray', lty = 2, lwd = 3)
mtext('B', side = 3, at = -2000, line = 0.5, cex = 2)
mtext('Fold', side = 3, at = 5000, line = 1, cex=1.5)
legend(x ='bottomleft', col=c('red', 'black'), legend = c('|DEV|', '|Dominant eigenvalue|'), lty = 1, lwd=4, cex=1.5)

# Transcritical bifurcation
plot(lv_transcritical_eigenval(), type = 'l', ylim = c(0, 1.5), 
     col = 'black', lwd=3, ylab = NA, xlab = 'c', xaxt='n')
axis(1, at=c(0, 5000, 10000), labels = c(1.5, 1, 0))

lines(transcritical_DEV_mean[, 1], transcritical_DEV_mean[,2], type = 'l', col = 'purple', lwd = 3)
abline(h = 1, col = 'gray', lty = 2, lwd = 3)
mtext('C', side = 3, at = -2000, line = 0.5, cex = 2)
mtext('Transcritical', side = 3, at = 5000, line = 1, cex=1.5)
legend(x ='bottomleft', col=c('purple', 'black'), legend = c('|DEV|', '|Dominant eigenvalue|'), lty = 1, lwd=4, cex=1.5)


# Pitchfork bifurcation
plot(lorenz_pitchfork_eigenval(), type = 'l', ylim = c(0, 1.5), 
     col='black', lwd=3, ylab = NA, xlab = 'h', xaxt='n')
axis(1, at=c(0, 5000, 10000), labels = c(-1, 0, 1))

lines(pitchfork_DEV_mean[, 1], pitchfork_DEV_mean[,2], type = 'l', col = 'blue', lwd = 3)
abline(h = 1, col = 'gray', lty = 2, lwd = 3)
mtext('D', side = 3, at = -2000, line = 0.5, cex = 2)
mtext('Pitchfork', side = 3, at = 5000, line = 1, cex=1.5)
legend(x ='bottomleft', col=c('blue', 'black'), legend = c('|DEV|', '|Dominant eigenvalue|'), lty = 1, lwd=4, cex=1.5)

