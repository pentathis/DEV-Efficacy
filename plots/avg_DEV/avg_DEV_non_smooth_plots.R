source('non_smooth_model_functions.R')

no_bif_DEV <- read.csv('data/avg_DEV_non_smooth/no_bif.csv', row.names = 1)
ns_fold_DEV <- read.csv('data/avg_DEV_non_smooth/ns_fold.csv', row.names = 1)
ns_pd_DEV <- read.csv('data/avg_DEV_non_smooth/ns_pd.csv', row.names = 1)
ns_chaos_DEV <- read.csv('data/avg_DEV_non_smooth/ns_chaos.csv', row.names = 1)
ns_nm_DEV <- read.csv('data/avg_DEV_non_smooth/ns_nm.csv', row.names = 1)
ns_nm_mult_DEV <- read.csv('data/avg_DEV_non_smooth/ns_nm_mult.csv', row.names = 1)


# Draw the plots
par(mfrow=c(5, 4), mai = c(0.55, 0.6, 0.47, 0.15), cex.lab = 1.5)

#No bifurcation
plot(non_smooth_general(x0 = -2.5, a = 0.6, b = 0.2), 
     type = 'l', 
     ylab = 'x', 
     xlab = NA, 
     lwd = 2, 
     col = '#b2173c')

axis(side = 3, labels = seq(-1, 1,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext(expression('Control Parameter ('*mu*')'), side = 3, line = 2, cex = 1)
mtext('A', at = -2400, line = 1.8, cex = 1.6)
abline(v = 5000, col = 'gray', lwd = 2)

plot(no_bif_DEV[,1], no_bif_DEV[,2], 
     type = 'l', 
     ylab = '|DEV|',
     xlab = NA, 
ylim = c(0.5, 1.1),
     lwd = 3,
     col = '#b2173c')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 1, lty = 2, col = 'gray')

plot(no_bif_DEV[,1], no_bif_DEV[,3], 
     type = 'l',  
     ylab = 'Re(DEV)', 
     xlab = NA, 
     ylim = c(-1, 1),
     lwd = 3, 
     col = '#b2173c')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 1, lty = 2, col = 'gray')
abline(h = -1, lty = 2, col = 'gray')

plot(no_bif_DEV[,1], no_bif_DEV[,4], 
     type = 'l',  
     ylab = 'Im(DEV)', 
     xlab = NA, 
     ylim = c(-0.2, 0.1),
     lwd = 3, 
     col = '#b2173c')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 0, lty = 2, col = 'gray')


# Fold
plot(100, 100, type = 'l',
     ylab = 'x', 
     xlab = NA, 
     ylim = c(-2, 2),
     xlim = c(0, 1e4))
abline(v = 5000, col = 'gray', lwd = 2)

lines(non_smooth_general(x0 = 1.25, a = 1.5, b = 0.2, mu_min = 1, mu_max = -1),
      lwd = 2,
      col = '#8b3591')

axis(side = 3, labels = seq(1, -1,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext(expression('Control Parameter ('*mu*')'), side = 3, line = 2, cex = 1)
mtext('B', at = -2400, line = 1.8, cex = 1.6)

plot(ns_fold_DEV[,1], ns_fold_DEV[,2], 
     type = 'l',  
     ylab = '|DEV|', 
     xlab = NA, 
     ylim = c(0.5, 1.1),
     xlim = c(0, 1e4),
     lwd = 3, 
     col = '#8b3591')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 1, lty = 2, col = 'gray')

plot(ns_fold_DEV[,1], ns_fold_DEV[,3], 
     type = 'l',  
     ylab = 'Re(DEV)',
     xlab = NA, 
     ylim = c(-1, 1),
     xlim = c(0, 1e4),
     lwd = 3,
     col = '#8b3591')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 1, lty = 2, col = 'gray')
abline(h = -1, lty = 2, col = 'gray')

plot(ns_fold_DEV[,1], ns_fold_DEV[,4], 
     type = 'l',  
     ylab = 'Im(DEV)', 
     xlab = NA,
     ylim = c(-0.2, 0.1),
     xlim = c(0, 1e4),
     lwd = 3, 
     col = '#8b3591')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 0, lty = 2, col = 'gray')


# Period doubling
plot(non_smooth_general(x0 = -0.67, a = -0.5, b = -1.5), 
     type = 'l',
     ylab = 'x', 
     xlab = NA, 
     lwd = 2, 
     col = '#72b322')

axis(side = 3, labels = seq(-1, 1,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext(expression('Control Parameter ('*mu*')'), side = 3, line = 2, cex = 1)
mtext('C', at = -2400, line = 1.8, cex = 1.6)
abline(v = 5000, col = 'gray', lwd = 2)

plot(ns_pd_DEV[,1], ns_pd_DEV[,2], 
     type = 'l',  
     ylab = '|DEV|', 
     xlab = NA,
ylim = c(0.5, 1.1),
     lwd = 3, 
     col = '#72b322')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 1, lty = 2, col = 'gray')

plot(ns_pd_DEV[,1], ns_pd_DEV[,3], 
     type = 'l',  
     ylab = 'Re(DEV)', 
     xlab = NA, 
     ylim = c(-1, 1),
     lwd = 3, 
     col = '#72b322')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = -1, lty = 2, col = 'gray')
abline(h = 1, lty = 2, col = 'gray')

plot(ns_pd_DEV[,1], ns_pd_DEV[,4], 
     type = 'l',  
     ylab = 'Im(DEV)',
     xlab = NA, 
     ylim = c(-0.2, 0.1),
     lwd = 3, 
     col = '#72b322')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 0, lty = 2, col = 'gray')


# Period1-chaos
plot(non_smooth_general(x0 = -2.5, a = 0.6, b = -4), 
     type = 'l',
     ylab = 'x', 
     xlab = NA, 
     lwd = 2, 
     col = '#1480bf')

axis(side = 3, labels = seq(-1, 1,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext(expression('Control Parameter ('*mu*')'), side = 3, line = 2, cex = 1)
mtext('D', at = -2400, line = 1.8, cex = 1.6)
abline(v = 5000, col = 'gray', lwd = 2)

plot(ns_chaos_DEV[,1], ns_chaos_DEV[,2], 
     type = 'l',  
     ylab = '|DEV|', 
     xlab = NA,
     ylim = c(0.5, 1.8),
     lwd = 3, 
     col = '#1480bf')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 1, lty = 2, col = 'gray')

plot(ns_chaos_DEV[,1], ns_chaos_DEV[,3], 
     type = 'l',  
     ylab = 'Re(DEV)', 
     xlab = NA, 
     ylim = c(-1.2, 1),
     lwd = 3, 
     col = '#1480bf')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = -1, lty = 2, col = 'gray')
abline(h = 1, lty = 2, col = 'gray')

plot(ns_chaos_DEV[,1], ns_chaos_DEV[,4], 
     type = 'l',  
     ylab = 'Im(DEV)', 
     xlab = NA, 
     ylim = c(-0.2, 0.1),
     lwd = 3, 
     col = '#1480bf')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 0, lty = 2, col = 'gray')


# Neimark-Sacker
plot(ns_neimark_sacker(), 
     type = 'l',
     ylab = 'x', 
     xlab = 'Time', 
     lwd = 2, 
     col = '#656364')

axis(side = 3, labels = seq(-1, 1,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext(expression('Control Parameter ('*mu*')'), side = 3, line = 2, cex = 1)
mtext('E', at = -2400, line = 1.8, cex = 1.6)
abline(v = 5000, col = 'gray', lwd = 2)

plot(ns_nm_DEV[,1], ns_nm_DEV[,2], 
     type = 'l', 
     ylab = '|DEV|',
     xlab = 'Time', 
     ylim = c(0.5, 1.2),
     lwd = 3, 
     col = '#656364')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 1, lty = 2, col = 'gray')

plot(ns_nm_DEV[,1], ns_nm_DEV[,3], 
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = 'Time', 
     ylim = c(-1, 1),
     lwd = 3, 
     col = '#656364')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 1, lty = 2, col = 'gray')
abline(h = -1, lty = 2, col = 'gray')

plot(ns_nm_DEV[,1], ns_nm_DEV[,4], 
     type = 'l',  
     ylab = 'Im(DEV)', 
     xlab = 'Time', 
     ylim = c(-0.9, 0.1),
     lwd = 3, 
     col = '#656364')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 0, lty = 2, col = 'gray')


# Neimark-Sacker multivariate_DEV
par(mfrow=c(1, 3), cex.lab=1.3)
plot(ns_nm_mult_DEV[,1], ns_nm_mult_DEV[,2], 
     type = 'l',  
     ylab = '|DEV|',
     xlab = 'Time', 
     lwd = 3, 
     col = '#656364')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 1, lty = 2, col = 'gray')

plot(ns_nm_mult_DEV[,1], ns_nm_mult_DEV[,3], 
     type = 'l',  
     ylab = 'Re(DEV)', 
     xlab = 'Time', 
     lwd = 3, 
     col = '#656364')
abline(v = 5000,  col = 'gray', lwd = 2)
abline(h = 1, lty = 2, col = 'gray')

plot(ns_nm_mult_DEV[,1], ns_nm_mult_DEV[,4], 
     type = 'l',  
     ylab = 'Im(DEV)', 
     xlab = 'Time', 
     lwd = 3, 
     col = '#656364')
abline(v = 5000, col = 'gray', lwd = 2)
abline(h = 0, lty = 2, col = 'gray')
abline(v = 5000,  col = 'gray', lwd = 2)

