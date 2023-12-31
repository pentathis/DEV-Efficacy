library(rEDM)
library(compiler)


source('continuous_model_functions.R')

source('univariate_DEV.R')
source('multivariate_DEV.R')


replicates <- 100

for(i in 1:replicates){
  
  assign(paste0('fold_ts', i), fold())
  assign(paste0('transcritical_ts', i), transcritical())
  assign(paste0('hopf_ts', i), hopf())
  assign(paste0('pitchfork_ts', i), pitchfork())
  assign(paste0('wo_csd_ts', i), wo_csd())
  assign(paste0('no_trans_ts', i), no_trans())
  assign(paste0('wo_bif_ts', i), wo_bif())
}


for(i in 1:replicates){
  
  print(i)
  
  assign(paste0('fold_DEV', i), DEV(get(paste0('fold_ts', i))[1:8100], E = 2,
                                               tau = -1, theta = seq(0, 2.5, 0.5),
                                               winsize = 250, stepsize = 50))

  assign(paste0('transcritical_DEV', i), DEV(get(paste0('transcritical_ts', i))[1:5000], E = 2,
                                         tau = -1, theta = seq(0, 2.5, 0.5),
                                         winsize = 250, stepsize = 50))

  assign(paste0('hopf_DEV', i), DEV(get(paste0('hopf_ts', i))[1:3520, 2], E = 6,
                                              tau = -1, theta = seq(0, 2.5, 0.5),
                                              winsize = 250, stepsize = 50))

  assign(paste0('pitchfork_DEV', i), DEV(get(paste0('pitchfork_ts', i))[1:3250], E = 2,
                                              tau = -1, theta = seq(0, 2.5, 0.5),
                                              winsize = 250, stepsize = 50))

  assign(paste0('wo_csd_DEV', i), DEV(get(paste0('wo_csd_ts', i))[1:5000, 4], E = 6,
                                              tau = -1, theta = seq(0, 2.5, 0.5),
                                              winsize = 250, stepsize = 50))

  assign(paste0('no_trans_DEV', i), DEV(get(paste0('no_trans_ts', i))[1:5000], E = 2,
                                              tau = -1, theta = seq(0, 2.5, 0.5),
                                              winsize = 250, stepsize = 50))

  assign(paste0('wo_bif_DEV', i), DEV(get(paste0('wo_bif_ts', i))[1:2750], E = 2,
                                                  tau = -1, theta = seq(0, 2.5, 0.5),
                                                  winsize = 250, stepsize = 50))


  # DEV for the remaining variables in multivariate models
  assign(paste0('hopf_P_DEV', i), DEV(get(paste0('hopf_ts', i))[1:3520, 3], E = 6,
                                              tau = -1, theta = seq(0, 2.5, 0.5),
                                              winsize = 250, stepsize = 50))

  assign(paste0('wo_csd_R_J_DEV', i), DEV(get(paste0('wo_csd_ts', i))[1:5000, 2], E = 6,
                                                tau = -1, theta = seq(0, 2.5, 0.5),
                                                winsize = 250, stepsize = 50))

  assign(paste0('wo_csd_R_A_DEV', i), DEV(get(paste0('wo_csd_ts', i))[1:5000, 3], E = 6,
                                                    tau = -1, theta = seq(0, 2.5, 0.5),
                                                    winsize = 250, stepsize = 50))

  assign(paste0('wo_csd_C_A_DEV', i), DEV(get(paste0('wo_csd_ts', i))[1:5000, 5], E = 6,
                                                    tau = -1, theta = seq(0, 2.5, 0.5),
                                                    winsize = 250, stepsize = 50))


  # Multivariate DEV for multivariate models
  assign(paste0('hopf_mult_DEV', i), multivariate_DEV(get(paste0('hopf_ts', i))[1:3520,], E = 2,
                                      tau = -1, theta = seq(0, 2.5, 0.5),
                                      winsize = 250, stepsize = 50))

  assign(paste0('wo_csd_mult_DEV', i), multivariate_DEV(get(paste0('wo_csd_ts', i))[1:5000,], E = 4,
                                      tau = -1, theta = seq(0, 2.5, 0.5),
                                      winsize = 250, stepsize = 50))
}



# Calculate mean DEV
fold_DEV_sum <-  0
transcritical_DEV_sum <-  0
hopf_DEV_sum <-  0
pitchfork_DEV_sum <-  0
wo_csd_DEV_sum <-  0
no_trans_DEV_sum <-  0
wo_bif_DEV_sum <-  0

hopf_P_DEV_sum <-  0
wo_csd_R_J_DEV_sum <-  0
wo_csd_R_A_DEV_sum <-  0
wo_csd_C_A_DEV_sum <-  0

hopf_mult_DEV_sum <-  0
wo_csd_mult_DEV_sum <- 0



for(i in 1:replicates){
  
  fold_DEV_sum <-  fold_DEV_sum + get(paste0('fold_DEV', i))
  transcritical_DEV_sum <-  transcritical_DEV_sum + get(paste0('transcritical_DEV', i))
  hopf_DEV_sum <-  hopf_DEV_sum + get(paste0('hopf_DEV', i))
  pitchfork_DEV_sum <-  pitchfork_DEV_sum + get(paste0('pitchfork_DEV', i))
  wo_csd_DEV_sum <-  wo_csd_DEV_sum + get(paste0('wo_csd_DEV', i))
  no_trans_DEV_sum <-  no_trans_DEV_sum + get(paste0('no_trans_DEV', i))
  wo_bif_DEV_sum <-  wo_bif_DEV_sum + get(paste0('wo_bif_DEV', i))


  hopf_P_DEV_sum <- hopf_P_DEV_sum + get(paste0('hopf_P_DEV', i))
  wo_csd_R_J_DEV_sum <-  wo_csd_R_J_DEV_sum + get(paste0('wo_csd_R_J_DEV', i))
  wo_csd_R_A_DEV_sum <- wo_csd_R_A_DEV_sum + get(paste0('wo_csd_R_A_DEV', i))
  wo_csd_C_A_DEV_sum <-  wo_csd_C_A_DEV_sum + get(paste0('wo_csd_C_A_DEV', i))

  hopf_mult_DEV_sum <-  hopf_mult_DEV_sum + get(paste0('hopf_mult_DEV', i))
  wo_csd_mult_DEV_sum <- wo_csd_mult_DEV_sum + get(paste0('wo_csd_mult_DEV', i))
  
}


# Calculate mean values
fold_DEV_mean <- fold_DEV_sum/replicates
transcritical_DEV_mean <- transcritical_DEV_sum/replicates
hopf_DEV_mean <- hopf_DEV_sum/replicates
pitchfork_DEV_mean <- pitchfork_DEV_sum/replicates
wo_csd_DEV_mean <- wo_csd_DEV_sum/replicates
no_trans_DEV_mean <- no_trans_DEV_sum/replicates
wo_bif_DEV_mean <- wo_bif_DEV_sum/replicates

hopf_P_DEV_mean <- hopf_P_DEV_sum/replicates
wo_csd_R_J_DEV_mean <- wo_csd_R_J_DEV_sum/replicates
wo_csd_R_A_DEV_mean <- wo_csd_R_A_DEV_sum/replicates
wo_csd_C_A_DEV_mean <- wo_csd_C_A_DEV_sum/replicates

hopf_mult_DEV_mean <- hopf_mult_DEV_sum/replicates
wo_csd_mult_DEV_mean <- wo_csd_mult_DEV_sum/replicates


# Save the files
write.csv(fold_DEV_mean, 'data/avg_DEV_continuous/fold.csv')
write.csv(transcritical_DEV_mean, 'data/avg_DEV_continuous/transcritical.csv')
write.csv(hopf_DEV_mean, 'data/avg_DEV_continuous/hopf.csv')
write.csv(pitchfork_DEV_mean, 'data/avg_DEV_continuous/pitchfork.csv')
write.csv(wo_csd_DEV_mean, 'data/avg_DEV_continuous/wo_csd.csv')
write.csv(no_trans_DEV_mean, 'data/avg_DEV_continuous/no_trans.csv')
write.csv(wo_bif_DEV_mean, 'data/avg_DEV_continuous/wo_bif.csv')

write.csv(hopf_P_DEV_mean, 'data/avg_DEV_continuous/hopf_P.csv')
write.csv(wo_csd_R_J_DEV_mean, 'data/avg_DEV_continuous/wo_csd_R_J.csv')
write.csv(wo_csd_R_A_DEV_mean, 'data/avg_DEV_continuous/wo_csd_R_A.csv')
write.csv(wo_csd_C_A_DEV_mean, 'data/avg_DEV_continuous/wo_csd_C_A.csv')


write.csv(hopf_mult_DEV_mean, 'data/avg_DEV_continuous/hopf_mult.csv')
write.csv(wo_csd_mult_DEV_mean, 'data/avg_DEV_continuous/wo_csd_mult.csv')
