source('continuous_model_functions.R')


# Read the saved files
fold_DEV <- read.csv('data/avg_DEV_continuous/fold.csv', row.names = 1)
transcritical_DEV <- read.csv('data/avg_DEV_continuous/transcritical.csv', row.names = 1)
hopf_DEV <- read.csv('data/avg_DEV_continuous/hopf.csv', row.names = 1)
pitchfork_DEV <- read.csv('data/avg_DEV_continuous/pitchfork.csv', row.names = 1)
wo_csd_DEV <- read.csv('data/avg_DEV_continuous/wo_csd.csv', row.names = 1)
no_trans_DEV <- read.csv('data/avg_DEV_continuous/no_trans.csv', row.names = 1)
wo_bif_DEV <- read.csv('data/avg_DEV_continuous/wo_bif.csv', row.names = 1)

hopf_P_DEV <- read.csv('data/avg_DEV_continuous/hopf_P.csv', row.names = 1)
wo_csd_R_J_DEV <- read.csv('data/avg_DEV_continuous/wo_csd_R_J.csv', row.names = 1)
wo_csd_R_A_DEV <- read.csv('data/avg_DEV_continuous/wo_csd_R_A.csv', row.names = 1)
wo_csd_C_A_DEV <- read.csv('data/avg_DEV_continuous/wo_csd_C_A.csv', row.names = 1)

hopf_mult_DEV <- read.csv('data/avg_DEV_continuous/hopf_mult.csv', row.names = 1)
wo_csd_mult_DEV <- read.csv('data/avg_DEV_continuous/wo_csd_mult.csv', row.names = 1)


#Plots

# The main plots for the 7 systems
# Save with size 1000 px width by 1250 px height
par(mfrow=c(7, 4), mai = c(0.54, 0.6, 0.47, 0.15), cex.lab = 1.5)

plot(fold(), 
     type = 'l', 
     ylab = 'N', 
     xlab = NA,
     ylim = c(0, 10), 
     lwd = 3, 
     col = '#b2173c')
abline(v = 8100, col = 'gray', lwd = 2)
axis(side = 3, labels = seq(1, 3,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext('Control parameter (c)', side = 3, line = 2.3, cex = 1)
mtext('A', at = -2400, line = 1.8, cex = 1.6)

plot(fold_DEV[,1], fold_DEV[,2], 
     type = 'l', 
     ylab = '|DEV|', 
     xlab = NA,
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#b2173c')
abline(h = 1, lty = 2, col = 'gray')

plot(fold_DEV[,1], fold_DEV[,3], 
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = NA,
     xlim = c(0, 1e4), 
     ylim = c(0, 1.1), 
     lwd = 3, 
     col = '#b2173c')
abline(h = 1, lty = 2, col = 'gray')
plot(fold_DEV[,1], fold_DEV[,4], type = 'l', ylab = 'Im(DEV)', xlab = 'Time',ylim = c(-1, 1), xlim = c(0, 1e4), lwd = 3, col = '#b2173c')
abline(h = 0, lty = 2, col = 'gray')


# Transcritical
usable <- F

  while(usable == F){

    tcts <- transcritical()

    if(any(is.na(tcts)) == T || any(is.na(tcts)) < 0){
      usable = F
    }else{usable = T}
  }
plot(tcts, 
     type = 'l', 
     ylab = 'N', 
     xlab = NA,
     ylim = c(0, 11), 
     lwd = 3, 
     col = '#8b3591')
abline(v = 5000, col = 'gray', lwd = 2)
axis(side = 3, labels = seq(2, 0,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext('Control parameter (c)', side = 3, line = 2.3, cex = 1)
mtext('B', at = -2400, line = 1.8, cex = 1.6)

plot(transcritical_DEV[,1], transcritical_DEV[,2], 
     type = 'l', 
     ylab = '|DEV|', 
     xlab = NA,
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#8b3591')
abline(h = 1, lty = 2, col = 'gray')

plot(transcritical_DEV[,1], transcritical_DEV[,3], 
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = NA,
     xlim = c(0, 1e4), 
     ylim = c(0, 1.1), 
     lwd = 3, 
     col = '#8b3591')
abline(h = 1, lty = 2, col = 'gray')

plot(transcritical_DEV[,1], transcritical_DEV[,4], 
     type = 'l', 
     ylab = 'Im(DEV)', 
     xlab = NA,
     ylim = c(-1, 1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#8b3591')
abline(h = 0, lty = 2, col = 'gray')


# hopf
plot(hopf(), 
     type = 'l', 
     ylab = 'N', 
     xlab = NA,
     ylim = c(0, 4), 
     lwd = 3, 
     col = '#72b322')
abline(v = 3520, col = 'gray', lwd = 2)
axis(side = 3, labels = seq(2, 4, length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext('Control parameter (K)', side = 3, line = 2.3, cex = 1)
mtext('C', at = -2400, line = 1.8, cex = 1.6)

plot(hopf_DEV[,1], hopf_DEV[,2], 
     type = 'l', 
     ylab = '|DEV|', 
     xlab = NA,
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#72b322')
abline(h = 1, lty = 2, col = 'gray')

plot(hopf_DEV[,1], hopf_DEV[,3], 
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = NA,
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#72b322')
abline(h = 1, lty = 2, col = 'gray')

plot(hopf_DEV[,1], hopf_DEV[,4], 
     type = 'l',
     ylab = 'Im(DEV)', 
     xlab = NA,
     ylim = c(-1, 1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#72b322')
abline(h = 0, lty = 2, col = 'gray')

# Pitchfork
plot(pitchfork(), 
     type = 'l', 
     ylab = 'N', 
     xlab = NA,
     lwd = 3, 
     col = '#1480bf')
abline(v = 3250, col = 'gray', lwd = 2)
axis(side = 3, labels = seq(0, 1, length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext('Control parameter (r)', side = 3, line = 2.3, cex = 1)
mtext('D', at = -2400, line = 1.8, cex = 1.6)

plot(pitchfork_DEV[,1], pitchfork_DEV[,2],
     type = 'l', 
     ylab = '|DEV|', 
     xlab = NA,
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#1480bf')
abline(h = 1, lty = 2, col = 'gray')

plot(pitchfork_DEV[,1], pitchfork_DEV[,3], 
     type = 'l',
     ylab = 'Re(DEV)', 
     xlab = NA,
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#1480bf')
abline(h = 1, lty = 2, col = 'gray')

plot(pitchfork_DEV[,1], pitchfork_DEV[,4], 
     type = 'l', 
     ylab = 'Im(DEV)', 
     xlab = NA,
     ylim = c(-1, 1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#1480bf')
abline(h = 0, lty = 2, col = 'gray')


# wo_csd
plot(wo_csd()[,4], 
     type = 'l', 
     ylab = expression(C[J]), 
     xlab = NA,
     ylim = c(0, 110), 
     lwd = 3, 
     col = '#656364')
abline(v = 5000, col = 'gray', lwd = 2)
axis(side = 3, labels = seq(20, 150,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext(expression('Control parameter ('*K[J]*')'), side = 3, line = 2, cex = 0.9)
mtext('E', at = -2400, line = 1.8, cex = 1.6)

plot(wo_csd_DEV[,1], wo_csd_DEV[,2], 
     type = 'l', 
     ylab = '|DEV|', 
     xlab = NA,
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#656364')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_csd_DEV[,1], wo_csd_DEV[,3],
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = NA,
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#656364')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_csd_DEV[,1], wo_csd_DEV[,4], 
     type = 'l', 
     ylab = 'Im(DEV)', 
     xlab = NA,
     ylim = c(-1, 1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#656364')
abline(h = 0, lty = 2, col = 'gray')


# no_trans
plot(no_trans(), 
     type = 'l', 
     ylab = 'N', 
     xlab = NA,
     ylim = c(0, 1.1), 
     lwd = 3, 
     col = '#f0b816')
abline(v = 5000, col = 'gray', lwd = 2)
axis(side = 3, labels = seq(1, 3,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext('Control parameter (c)', side = 3, line = 2.3, cex = 1)
mtext('F', at = -2400, line = 1.8, cex = 1.6)

plot(no_trans_DEV[,1], no_trans_DEV[,2],
     type = 'l',
     ylab = '|DEV|', 
     xlab = NA,
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#f0b816')
abline(h = 1, lty = 2, col = 'gray')

plot(no_trans_DEV[,1], no_trans_DEV[,3], 
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = NA, 
     ylim = c(0, 1.1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#f0b816')
abline(h = 1, lty = 2, col = 'gray')

plot(no_trans_DEV[,1], no_trans_DEV[,4], 
     type = 'l',
     ylab = 'Im(DEV)',
     xlab = NA,
     ylim = c(-1, 1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#f0b816')
abline(h = 0, lty = 2, col = 'gray')


# wo_bif
plot(wo_bif(), 
     type = 'l', 
     ylab = 'N', 
     xlab = 'Time',
     ylim = c(0, 5), 
     lwd = 3, 
     col = '#a0686c')
abline(v = 2750, col = 'gray', lwd = 2)
axis(side = 3, labels = seq(1, 3,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext('Control parameter (c)', side = 3, line = 2.3, cex = 1)
mtext('G', at = -2400, line = 1.8, cex = 1.6)

plot(wo_bif_DEV[,1], wo_bif_DEV[,2], 
     type = 'l', 
     ylab = '|DEV|', 
     xlab = 'Time', 
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#a0686c')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_bif_DEV[,1], wo_bif_DEV[,3], 
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = 'Time', 
     ylim = c(0, 1.1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#a0686c')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_bif_DEV[,1], wo_bif_DEV[,4], 
     type = 'l', 
     ylab = 'Im(DEV)', 
     xlab = 'Time', 
     ylim = c(-1, 1),
     xlim = c(0, 1e4), 
     lwd = 3, col = '#a0686c')
abline(h = 0, lty = 2, col = 'gray')



# DEV plots for other variables in Hopf bifurcation and multivariate DEV

# Save with size 900 px width by 400 px height

par(mfrow=c(2, 4), mai = c(0.55, 0.6, 0.47, 0.15), cex.lab = 1.5)


plot(hopf()[,3], 
     type = 'l', 
     ylab = 'P', 
     xlab = 'Time', 
     ylim = c(0, 4), 
     lwd = 3, 
     col = '#72b322')
abline(v = 3520, col = 'gray', lwd = 2)
axis(side = 3, labels = seq(2, 4, length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext('Control parameter (K)', side = 3, line = 2.3, cex = 1)
mtext('A', at = -2400, line = 1.8, cex = 1.6)

plot(hopf_P_DEV[,1], hopf_P_DEV[,2], 
     type = 'l',
     ylab = '|DEV|', 
     xlab = 'Time', 
     ylim = c(0, 1.1),
     xlim = c(0, 1e4), 
     lwd = 3,
     col = '#72b322')
abline(h = 1, lty = 2, col = 'gray')

plot(hopf_P_DEV[,1], hopf_P_DEV[,3], 
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = 'Time', 
     ylim = c(0, 1.1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#72b322')
abline(h = 1, lty = 2, col = 'gray')

plot(hopf_P_DEV[,1], hopf_P_DEV[,4], 
     type = 'l',
     ylab = 'Im(DEV)', 
     xlab = 'Time',
     ylim = c(-1, 1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#72b322')
abline(h = 0, lty = 2, col = 'gray')


plot.new()
mtext('B', at = -0.25, line = 1.8, cex = 1.6)
plot(hopf_mult_DEV[,1], hopf_mult_DEV[,2],
     type = 'l', 
     ylab = '|DEV|', 
     xlab = 'Time', 
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#72b322')
abline(h = 1, lty = 2, col = 'gray')

plot(hopf_mult_DEV[,1], hopf_mult_DEV[,3], 
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = 'Time', 
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3,
     col = '#72b322')
abline(h = 1, lty = 2, col = 'gray')

plot(hopf_mult_DEV[,1], hopf_mult_DEV[,4], 
     type = 'l', 
     ylab = 'Im(DEV)', 
     xlab = 'Time', 
     ylim = c(-1, 1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#72b322')
abline(h = 0, lty = 2, col = 'gray')



# DEV plots for other variables in bifurcation in without case and multivariate DEV

# Save with size 984 px width by 800 px height
wo_csd_ts <- wo_csd()

par(mfrow=c(4, 4), mai = c(0.55, 0.6, 0.47, 0.15), cex.lab = 1.5)

plot(wo_csd_ts[,1], wo_csd_ts[,2], 
     type = 'l', 
     ylab = expression(R[J]), 
     xlab = 'Time', 
     ylim = c(0, 150), 
     lwd = 3, 
     col = '#656364')
abline(v = 5000, col = 'gray', lwd = 2)
axis(side = 3, labels = seq(20, 150,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext(expression('Control parameter ('*K[J]*')'), side = 3, line = 2, cex = 1)
mtext('A', at = -2400, line = 1.8, cex = 1.6)

plot(wo_csd_R_J_DEV[,1], wo_csd_R_J_DEV[,2],
     type = 'l', 
     ylab = '|DEV|', 
     xlab = 'Time', 
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3,
     col = '#656364')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_csd_R_J_DEV[,1], wo_csd_R_J_DEV[,3],
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = 'Time', 
     xlim = c(0, 1e4), 
     ylim = c(0, 1.1), 
     lwd = 3, 
     col = '#656364')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_csd_R_J_DEV[,1], wo_csd_R_J_DEV[,4],
     type = 'l', 
     ylab = 'Im(DEV)', 
     xlab = 'Time',
     ylim = c(-1, 1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#656364')
abline(h = 0, lty = 2, col = 'gray')



plot(wo_csd_ts[,1], wo_csd_ts[,3], 
     type = 'l',
     ylab =  expression(R[A]), 
     xlab = 'Time',
     ylim = c(0, 150), 
     lwd = 3, 
     col = '#656364')
abline(v = 5000, col = 'gray', lwd = 2)
axis(side = 3, labels = seq(20, 150,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext(expression('Control parameter ('*K[J]*')'), side = 3, line = 2, cex = 0.9)
mtext('B', at = -2400, line = 1.8, cex = 1.6)

plot(wo_csd_R_A_DEV[,1], wo_csd_R_A_DEV[,2],
     type = 'l', 
     ylab = '|DEV|', 
     xlab = 'Time', 
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#656364')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_csd_R_A_DEV[,1], wo_csd_R_A_DEV[,3], 
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = 'Time', 
     xlim = c(0, 1e4), 
     ylim = c(0, 1.1), 
     lwd = 3, 
     col = '#656364')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_csd_R_A_DEV[,1], wo_csd_R_A_DEV[,4], 
     type = 'l', 
     ylab = 'Im(DEV)', 
     xlab = 'Time',
     ylim = c(-1, 1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#656364')
abline(h = 0, lty = 2, col = 'gray')



plot(wo_csd_ts[,1], wo_csd_ts[,5], 
     type = 'l', 
     ylab =  expression(C[A]), 
     xlab = 'Time', 
     ylim = c(0, 150), 
     lwd = 3, 
     col = '#656364')
abline(v = 5000, col = 'gray', lwd = 2)
axis(side = 3, labels = seq(20, 150,length.out = 6), at = seq(1, 1e4, length.out = 6))
mtext(expression('Control parameter ('*K[J]*')'), side = 3, line = 2, cex = 0.9)
mtext('C', at = -2400, line = 1.8, cex = 1.6)

plot(wo_csd_C_A_DEV[,1], wo_csd_C_A_DEV[,2], 
     type = 'l', 
     ylab = '|DEV|', 
     xlab = 'Time', 
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#656364')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_csd_C_A_DEV[,1], wo_csd_C_A_DEV[,3], 
     type = 'l', 
     ylab = 'Re(DEV)', 
     xlab = 'Time',
     xlim = c(0, 1e4), 
     ylim = c(0, 1.1), 
     lwd = 3, 
     col = '#656364')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_csd_C_A_DEV[,1], wo_csd_C_A_DEV[,4],
     type = 'l', 
     ylab = 'Im(DEV)', 
     xlab = 'Time',
     ylim = c(-1, 1),
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#656364')
abline(h = 0, lty = 2, col = 'gray')


plot.new()
# plot(wo_csd_ts[,1], wo_csd_ts[,3], type = 'l', ylab = 'N', xlab = 'Time', ylim = c(0, 150), lwd = 3, col = '#656364')
# abline(v = 5000, col = 'gray', lwd = 2)
# axis(side = 3, labels = seq(1, 3,length.out = 6), at = seq(1, 1e4, length.out = 6))
# mtext(expression('Control parameter ('*K[J]*')'), side = 3, line = 2, cex = 0.9)
mtext('D', at = -0.25, line = 1.8, cex = 1.6)
plot(wo_csd_mult_DEV[,1], wo_csd_mult_DEV[,2], 
     type = 'l', 
     ylab = '|DEV|', 
     xlab = 'Time', 
     ylim = c(0, 1.1), 
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#656364')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_csd_mult_DEV[,1], wo_csd_mult_DEV[,3],
     type = 'l', 
     ylab = 'Re(DEV)',
     xlab = 'Time',
     xlim = c(0, 1e4), 
     ylim = c(-1, 1.1), 
     lwd = 3,
     col = '#656364')
abline(h = 1, lty = 2, col = 'gray')

plot(wo_csd_mult_DEV[,1], wo_csd_mult_DEV[,4], 
     type = 'l', 
     ylab = 'Im(DEV)', 
     xlab = 'Time',
     ylim = c(-1, 1) ,
     xlim = c(0, 1e4), 
     lwd = 3, 
     col = '#656364')
abline(h = 0, lty = 2, col = 'gray')


