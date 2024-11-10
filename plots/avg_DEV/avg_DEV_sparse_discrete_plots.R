fold_DEV_mean <- read.csv('data/avg_DEV_sparse_discrete/sparse_discrete_fold.csv', row.names = 1)
transcritical_DEV_mean <- read.csv('data/avg_DEV_sparse_discrete/sparse_discrete_transcritical.csv', row.names = 1)
pitchfork_DEV_mean <- read.csv('data/avg_DEV_sparse_discrete/sparse_discrete_pitchfork.csv', row.names = 1)

fold_DEV_sd <- read.csv('data/sd_DEV_sparse_discrete/sparse_discrete_fold.csv', row.names = 1)
transcritical_DEV_sd <- read.csv('data/sd_DEV_sparse_discrete/sparse_discrete_transcritical.csv', row.names = 1)
pitchfork_DEV_sd <- read.csv('data/sd_DEV_sparse_discrete/sparse_discrete_pitchfork.csv', row.names = 1)

par(mfrow=c(3, 3), cex.axis = 1.4, cex.lab = 2, mai = c(0.8, 0.8, 0.28, 0.4))


# Fold
plot(-1, -1, ylim = c(0, 1.5), xlim = c(0, 105), ylab = '|DEV|', xlab = 'Time')
polygon(x=c(fold_DEV_mean[, 1], rev(fold_DEV_mean[, 1])),
        y=c(fold_DEV_mean[, 2] + fold_DEV_sd[,2], rev(fold_DEV_mean[, 2] - fold_DEV_sd[,2])),
        col = 'lightpink', density = NA)
lines(fold_DEV_mean[, 1], fold_DEV_mean[,2], type = 'l', col = 'red', lwd = 3)
abline(h = 1, col = 'gray', lty = 2, lwd = 2)

plot(-1, -1, ylim = c(0, 1.5), xlim = c(0, 105), ylab = 'Re(DEV)', xlab = 'Time')
polygon(x=c(fold_DEV_mean[, 1], rev(fold_DEV_mean[, 1])),
        y=c(fold_DEV_mean[, 3] + fold_DEV_sd[,3], rev(fold_DEV_mean[, 3] - fold_DEV_sd[,3])),
        col = 'lightpink', density = NA)
lines(fold_DEV_mean[, 1], fold_DEV_mean[,3], type = 'l', col = 'red', lwd = 3)
abline(h = 1, col = 'gray', lty = 2, lwd = 2)

plot(-1, -1, ylim = c(-0.5, 0.5), xlim = c(0, 105), ylab = 'Im(DEV)', xlab = 'Time')
polygon(x=c(fold_DEV_mean[, 1], rev(fold_DEV_mean[, 1])),
        y=c(fold_DEV_mean[, 4] + fold_DEV_sd[,4], rev(fold_DEV_mean[, 4] - fold_DEV_sd[,4])),
        col = 'lightpink', density = NA)
lines(fold_DEV_mean[, 1], fold_DEV_mean[,4], type = 'l', col = 'red', lwd = 3)
abline(h = 0, col = 'gray', lty = 2, lwd = 2)


# transcritical
plot(-1, -1, ylim = c(0, 1.5), xlim = c(0, 105), ylab = '|DEV|', xlab = 'Time')
polygon(x=c(transcritical_DEV_mean[, 1], rev(transcritical_DEV_mean[, 1])),
        y=c(transcritical_DEV_mean[, 2] + transcritical_DEV_sd[,2], rev(transcritical_DEV_mean[, 2] - transcritical_DEV_sd[,2])),
        col = 'plum1', density = NA)
lines(transcritical_DEV_mean[, 1], transcritical_DEV_mean[,2], type = 'l', col = 'purple', lwd = 3)
abline(h = 1, col = 'gray', lty = 2, lwd = 2)

plot(-1, -1, ylim = c(0, 1.5), xlim = c(0, 105), ylab = 'Re(DEV)', xlab = 'Time')
polygon(x=c(transcritical_DEV_mean[, 1], rev(transcritical_DEV_mean[, 1])),
        y=c(transcritical_DEV_mean[, 3] + transcritical_DEV_sd[,3], rev(transcritical_DEV_mean[, 3] - transcritical_DEV_sd[,3])),
        col = 'plum1', density = NA)
lines(transcritical_DEV_mean[, 1], transcritical_DEV_mean[,3], type = 'l', col = 'purple', lwd = 3)
abline(h = 1, col = 'gray', lty = 2, lwd = 2)

plot(-1, -1, ylim = c(-0.5, 0.5), xlim = c(0, 105), ylab = 'Im(DEV)', xlab = 'Time')
polygon(x=c(transcritical_DEV_mean[, 1], rev(transcritical_DEV_mean[, 1])),
        y=c(transcritical_DEV_mean[, 4] + transcritical_DEV_sd[,4], rev(transcritical_DEV_mean[, 4] - transcritical_DEV_sd[,4])),
        col = 'plum1', density = NA)
lines(transcritical_DEV_mean[, 1], transcritical_DEV_mean[,4], type = 'l', col = 'purple', lwd = 3)
abline(h = 0, col = 'gray', lty = 2, lwd = 2)


# pitchfork
plot(-1, -1, ylim = c(0, 1.5), xlim = c(0, 105), ylab = '|DEV|', xlab = 'Time')
polygon(x=c(pitchfork_DEV_mean[, 1], rev(pitchfork_DEV_mean[, 1])),
        y=c(pitchfork_DEV_mean[, 2] + pitchfork_DEV_sd[,2], rev(pitchfork_DEV_mean[, 2] - pitchfork_DEV_sd[,2])),
        col = 'lightblue', density = NA)
lines(pitchfork_DEV_mean[, 1], pitchfork_DEV_mean[,2], type = 'l', col = 'blue', lwd = 3)
abline(h = 1, col = 'gray', lty = 2, lwd = 2)

plot(-1, -1, ylim = c(-0.5, 1), xlim = c(0, 105), ylab = 'Re(DEV)', xlab = 'Time')
polygon(x=c(pitchfork_DEV_mean[, 1], rev(pitchfork_DEV_mean[, 1])),
        y=c(pitchfork_DEV_mean[, 3] + pitchfork_DEV_sd[,3], rev(pitchfork_DEV_mean[, 3] - pitchfork_DEV_sd[,3])),
        col = 'lightblue', density = NA)
lines(pitchfork_DEV_mean[, 1], pitchfork_DEV_mean[,3], type = 'l', col = 'blue', lwd = 3)
abline(h = 1, col = 'gray', lty = 2, lwd = 2)

plot(-1, -1, ylim = c(-0.6, 0.6), xlim = c(0, 105), ylab = 'Im(DEV)', xlab = 'Time')
polygon(x=c(pitchfork_DEV_mean[, 1], rev(pitchfork_DEV_mean[, 1])),
        y=c(pitchfork_DEV_mean[, 4] + pitchfork_DEV_sd[,4], rev(pitchfork_DEV_mean[, 4] - pitchfork_DEV_sd[,4])),
        col = 'lightblue', density = NA)
lines(pitchfork_DEV_mean[, 1], pitchfork_DEV_mean[,4], type = 'l', col = 'blue', lwd = 3)
abline(h = 0, col = 'gray', lty = 2, lwd = 2)
