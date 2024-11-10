library(rEDM)
library(compiler)


source('univariate_DEV.R')
source('discrete_model_functions.R')

replicates <- 100

ts_lengths <- c(seq(100, 900, 100), seq(1000, 10000, 1000))

wsize <- function(t){
  if(t <500){
    return(20)
  }else if(t<1000){
    return(100)
  }else{
    return(250)
  }
}

roundto50<- function(x){
  if(x%%50 > 25){
    return(x + 50 - x%%50)
  }else{
    return(x - x%%50 )
  }
}



# For fold bifurcation 
f_max_values <- seq(2, 11, 1)


for(i in 1:replicates){
  
  df_end_temp <-  matrix(NaN, ncol=length(f_max_values), nrow=length(ts_lengths))
  colnames(df_end_temp) <- f_max_values
  rownames(df_end_temp) <- ts_lengths
  
  df_start_temp <-  matrix(NaN, ncol=length(f_max_values), nrow=length(ts_lengths))
  colnames(df_start_temp) <- f_max_values
  rownames(df_start_temp) <- ts_lengths
  
  
  for(f in f_max_values){
    
    print(c(i, f))
    
    
    # generate a time series 
    fold_ts <- nm_fold(t_end = roundto50(10000*f/1.84), f_min = 0, f_max=f)
      
    for(t in ts_lengths){
      
      print(c(i, f, t))
      
      w <- wsize(t)
      s <- w/5
      
      fold_ts_t <- fold_ts[seq(1, 1e4, length.out=t)]
      
      df_end_temp[toString(t), toString(f)] <- DEV(fold_ts_t[(t-w+1):t],
                                                                E=2, tau=-1, theta=seq(0, 2.5, 0.5),
                                                                winsize=w,
                                                                stepsize = s)[1, '|DEV|']
      
      df_start_temp[toString(t), toString(f)] <- DEV(fold_ts_t[1:w],
                                                    E=2, tau=-1, theta=seq(0, 2.5, 0.5),
                                                    winsize=w,
                                                    stepsize = s)[1, '|DEV|']
      
    }
  }
  
  filename <- paste0('data/rate_vs_ts_len/fold/fold_DEV_end', i, '.csv')
  
  write.csv(df_end_temp, filename)
  
  filename <- paste0('data/rate_vs_ts_len/fold/fold_DEV_start', i, '.csv')
  
  write.csv(df_start_temp, filename)
}


# For transcritical bifurcation 
c_min_values <- seq(0.5, 0.05, by = -0.05)


for(i in 1:replicates){
  
  df_end_temp <-  matrix(NaN, ncol=length(c_min_values), nrow=length(ts_lengths))
  colnames(df_end_temp) <- c_min_values
  rownames(df_end_temp) <- ts_lengths
  
  df_start_temp <-  matrix(NaN, ncol=length(c_min_values), nrow=length(ts_lengths))
  colnames(df_start_temp) <- c_min_values
  rownames(df_start_temp) <- ts_lengths
  
  
  for(c in c_min_values){
    
    # generate a time series
    usable <-  F
    while(usable==F){
      
      transcritical_ts <- lv_transcritical(t_end = roundto50(10000*(1.5-c)*2), c_max = 1.5, c_min=c)[1:1e4,2]
      
      if(any(is.na(transcritical_ts)) == T || any(transcritical_ts > 1.5)){
        usable <-  F
      }else{usable = T}
    }
    
    for(t in ts_lengths){
      
      print(c(i, c, t))
      
      w <- wsize(t)
      s <- w/5
      
      transcritical_ts_t <- transcritical_ts[seq(1, 1e4, length.out=t)]
      
      df_end_temp[toString(t), toString(c)] <- DEV(transcritical_ts_t[(t-w+1):t],
                                                   E=2, tau=-1, theta=seq(0, 2.5, 0.5),
                                                   winsize=w,
                                                   stepsize = s)[1, '|DEV|']
      
      df_start_temp[toString(t), toString(c)] <- DEV(transcritical_ts_t[1:w],
                                                     E=2, tau=-1, theta=seq(0, 2.5, 0.5),
                                                     winsize=w,
                                                     stepsize = s)[1, '|DEV|']
      
    }
  }
  
  filename <- paste0('data/rate_vs_ts_len/transcritical/transcritical_DEV_end', i, '.csv')
  
  write.csv(df_end_temp, filename)
  
  filename <- paste0('data/rate_vs_ts_len/transcritical/transcritical_DEV_start', i, '.csv')
  
  write.csv(df_start_temp, filename)
}


# For pitchfork bifurcation 
a_max_values <- seq(0.5, 5, by = 0.5)


for(i in 1:replicates){
  
  df_end_temp <-  matrix(NaN, ncol=length(a_max_values), nrow=length(ts_lengths))
  colnames(df_end_temp) <- a_max_values
  rownames(df_end_temp) <- ts_lengths
  
  df_start_temp <-  matrix(NaN, ncol=length(a_max_values), nrow=length(ts_lengths))
  colnames(df_start_temp) <- a_max_values
  rownames(df_start_temp) <- ts_lengths
  
  
  for(a in a_max_values){
    
    # generate a time series 
    pitchfork_ts <- lorenz_pitchfork(t_end = roundto50(1e4*(a+1)), a_min = -1, a_max=a)
    
    for(t in ts_lengths){
      
      print(c(i, a, t))
      
      w <- wsize(t)
      s <- w/5
      
      pitchfork_ts_t <- pitchfork_ts[seq(1, 1e4, length.out=t), 2]
      
      df_end_temp[toString(t), toString(a)] <- DEV(pitchfork_ts_t[(t-w+1):t],
                                                   E=2, tau=-1, theta=seq(0, 2.5, 0.5),
                                                   winsize=w,
                                                   stepsize = s)[1, '|DEV|']
      
      df_start_temp[toString(t), toString(a)] <- DEV(pitchfork_ts_t[1:w],
                                                     E=2, tau=-1, theta=seq(0, 2.5, 0.5),
                                                     winsize=w,
                                                     stepsize = s)[1, '|DEV|']
      
    }
  }
  
  filename <- paste0('data/rate_vs_ts_len/pitchfork/pitchfork_DEV_end', i, '.csv')
  
  write.csv(df_end_temp, filename)
  
  filename <- paste0('data/rate_vs_ts_len/pitchfork/pitchfork_DEV_start', i, '.csv')
  
  write.csv(df_start_temp, filename)
}
