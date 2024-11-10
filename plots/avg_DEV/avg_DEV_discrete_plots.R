source('discrete_model_functions.R')


fold_DEV_mean <- read.csv('data/avg_DEV_discrete/discrete_fold.csv', row.names = 1)
transcritical_DEV_mean <- read.csv('data/avg_DEV_discrete/discrete_transcritical.csv', row.names = 1)
pitchfork_DEV_mean <- read.csv('data/avg_DEV_discrete/discrete_pitchfork.csv', row.names = 1)

fold_DEV_sd <- read.csv('data/sd_DEV_discrete/discrete_fold.csv', row.names = 1)
transcritical_DEV_sd <- read.csv('data/sd_DEV_discrete/discrete_transcritical.csv', row.names = 1)
pitchfork_DEV_sd <- read.csv('data/sd_DEV_discrete/discrete_pitchfork.csv', row.names = 1)


# Make the plots
par(mfrow=c(2, 2), cex.axis = 1.4, cex.lab = 2, mai = c(0.8, 1.2, 0.28, 0.5))

nmts <- nm_fold()
lvts <- (lv_transcritical()[,2] + 1.2)*(10/2.4)
rlts <- (lorenz_pitchfork()[,2] + 1.2)*(10/2.4)



# Make a dummy plot
plot(-1, -1, ylim = c(0, 10), xlim = c(0, 1e4), ylab = 'N', xlab = NA)

# Plot the time series
lines(1:10000, nmts, col = 'red')
lines(1:10000, lvts, col = 'purple')
lines(1:10000, rlts, col = 'blue')

axis(side = 4, labels = seq(-1.2, 1.2, length.out = 6), at = seq(0, 10, length.out = 6))
mtext('x', side = 4, line = 2.5, cex = 1.3)
mtext('A', side = 3, at = -2000, line = 0, cex = 2)


legend(x = 'bottomleft', col = c('red', 'purple', 'blue'), legend = c('Noy-Meir Model',
                                                    'Lotka-Volterra Model',
                                                    'Reduced Lorenz Model'),
       lty = 1, lwd = 4, cex = 1.5)

# Magnitude of DEV

# Make a dummy plot
plot(-1, -1, ylim = c(0, 1.2), xlim = c(0, 1e4), ylab = 'Re(DEV)', xlab = 'Time')

# Draw mean +- sd
polygon(x=c(fold_DEV_mean[, 1], rev(fold_DEV_mean[, 1])),
        y=c(fold_DEV_mean[, 2] + fold_DEV_sd[,2], rev(fold_DEV_mean[, 2] - fold_DEV_sd[,2])),
        col = 'lightpink', density = NA)

polygon(x=c(transcritical_DEV_mean[, 1], rev(transcritical_DEV_mean[, 1])),
        y=c(transcritical_DEV_mean[, 2] + transcritical_DEV_sd[,2], rev(transcritical_DEV_mean[, 2] - transcritical_DEV_sd[,2])),
        col = 'plum1', density = NA)

polygon(x=c(pitchfork_DEV_mean[, 1], rev(pitchfork_DEV_mean[, 1])),
        y=c(pitchfork_DEV_mean[, 2] + pitchfork_DEV_sd[,2], rev(pitchfork_DEV_mean[, 2] - pitchfork_DEV_sd[,2])),
        col = 'lightblue', density = NA)

# Draw |DEV| vs time
lines(fold_DEV_mean[, 1], fold_DEV_mean[,2], type = 'l', col = 'red', lwd = 3)
lines(transcritical_DEV_mean[, 1], transcritical_DEV_mean[,2], type = 'l', col = 'purple', lwd = 3)
lines(pitchfork_DEV_mean[, 1], pitchfork_DEV_mean[,2], type = 'l', col = 'blue', lwd = 3)


abline(h = 1, col = 'gray', lty = 2, lwd = 2)
mtext('B', side = 3, at = -2000, line = 0, cex = 2)


# Real part of DEV

# Make a dummy plot
plot(-1, -1, ylim = c(0, 1.2), xlim = c(0, 1e4), ylab = 'Re(DEV)', xlab = 'Time')

# Draw mean +- sd
polygon(x=c(fold_DEV_mean[, 1], rev(fold_DEV_mean[, 1])),
        y=c(fold_DEV_mean[, 3] + fold_DEV_sd[,3], rev(fold_DEV_mean[, 3] - fold_DEV_sd[,3])),
        col = 'lightpink', density = NA)

polygon(x=c(transcritical_DEV_mean[, 1], rev(transcritical_DEV_mean[, 1])),
        y=c(transcritical_DEV_mean[, 3] + transcritical_DEV_sd[,3], rev(transcritical_DEV_mean[, 3] - transcritical_DEV_sd[,3])),
        col = 'plum1', density = NA)

polygon(x=c(pitchfork_DEV_mean[, 1], rev(pitchfork_DEV_mean[, 1])),
        y=c(pitchfork_DEV_mean[, 3] + pitchfork_DEV_sd[,3], rev(pitchfork_DEV_mean[, 3] - pitchfork_DEV_sd[,3])),
        col = 'lightblue', density = NA)

# Draw |DEV| vs time
lines(fold_DEV_mean[, 1], fold_DEV_mean[,3], type = 'l', col = 'red', lwd = 3)
lines(transcritical_DEV_mean[, 1], transcritical_DEV_mean[,3], type = 'l', col = 'purple', lwd = 3)
lines(pitchfork_DEV_mean[, 1], pitchfork_DEV_mean[,3], type = 'l', col = 'blue', lwd = 3)



abline(h = 1, col = 'gray', lty = 2, lwd = 2)
mtext('C', side = 3, at = -2000, line = 0, cex = 2)


# Imaginary part of DEV

# Make a dummy plot
plot(-1, -1, ylim = c(-0.5, 0.5), xlim = c(0, 1e4), ylab = 'Im(DEV)', xlab = 'Time')
abline(h= 0, col = 'gray', lty = 2, lwd = 2)

# Draw mean +- sd
polygon(x=c(fold_DEV_mean[, 1], rev(fold_DEV_mean[, 1])),
        y=c(fold_DEV_mean[, 4] + fold_DEV_sd[,4], rev(fold_DEV_mean[, 4] - fold_DEV_sd[,4])),
        col = 'lightpink', density = NA)

polygon(x=c(transcritical_DEV_mean[, 1], rev(transcritical_DEV_mean[, 1])),
        y=c(transcritical_DEV_mean[, 4] + transcritical_DEV_sd[,4], rev(transcritical_DEV_mean[, 4] - transcritical_DEV_sd[,4])),
        col = 'plum1', density = NA)

polygon(x=c(pitchfork_DEV_mean[, 1], rev(pitchfork_DEV_mean[, 1])),
        y=c(pitchfork_DEV_mean[, 4] + pitchfork_DEV_sd[,4], rev(pitchfork_DEV_mean[, 4] - pitchfork_DEV_sd[,4])),
        col = 'lightblue', density = NA)

# Draw |DEV| vs time
lines(fold_DEV_mean[, 1], fold_DEV_mean[,4], type = 'l', col = 'red', lwd = 3)
lines(transcritical_DEV_mean[, 1], transcritical_DEV_mean[,4], type = 'l', col = 'purple', lwd = 3)
lines(pitchfork_DEV_mean[, 1], pitchfork_DEV_mean[,4], type = 'l', col = 'blue', lwd = 3)


mtext('D', side = 3, at = -2000, line = 0, cex = 2)
