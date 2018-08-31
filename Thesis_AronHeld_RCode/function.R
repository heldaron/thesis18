
installpackages <- function(){
  packages <- c("ggplot2", "fUnitRoots", "MultipleBubbles", "zoo", "xts","moments","urca","exuber","vars","profvis","gridExtra")
  if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
    print("Install missing package")
    install.packages(setdiff(packages, rownames(installed.packages())))  
  }
}
load_data <- function(path){
  
  ### FUNCTION TO LOAD THE DATA
  
  require(xts)
  ## 
  
  print(paste(path,"btc.csv",sep = ""))
  BTC <- read.csv(paste(path,"new_btc.csv",sep=""),sep=";",dec=",")
  ?Date
  dates <- as.Date(BTC[,1],format="%d/%m/%Y")
  BTC <- xts(BTC['Close..'],order.by = dates)
  
 
  LTC <- read.csv(paste(path,"new_ltc.csv",sep = ""),sep=";",dec=",")
  dates <- as.Date(LTC[,1],format="%d/%m/%Y")
  LTC <- xts(LTC['Close..'],order.by = dates)
  
  
  ETH <- read.csv(paste(path,"new_eth.csv",sep = ""),sep=";",dec=",")
  
  dates <- as.Date(ETH[,1],format="%d/%m/%Y")
  ETH <- xts(ETH['Close..'],order.by = dates)
  
  DASH <- read.csv(paste(path,"new_dash.csv",sep = ""),sep=";",dec=",")
  
  dates <- as.Date(DASH[,1],format="%d/%m/%Y")
  DASH <- xts(DASH['Close..'],order.by = dates)
  
  
  XRP <- read.csv(paste(path,"new_xrp.csv",sep = ""),sep=";",dec=",")
  
  dates <- as.Date(XRP[,1],format="%d/%m/%Y")
  XRP <- xts(XRP['Close..'],order.by = dates)
  
  

  XLM <- read.csv(paste(path,"new_xlm.csv",sep = ""),sep=";",dec=",")
  
  dates <- as.Date(XLM[,1],format="%d/%m/%Y")
  XLM <- xts(XLM['Close..'],order.by = dates)
  
  
  DATA <- merge.xts(BTC,LTC,ETH,DASH,XRP,XLM)
  colnames(DATA) <- c("BTC","LTC","ETH","DASH","XRP","XRM")
  
  return(DATA)
  
}
descriptive_summary<- function(DATA){
 #return descriptive summary statistic of data set 
  
  require(moments)
  
  i_cols = 8
  i_row = length(DATA[1,])
  summary_matrix <- matrix(ncol=i_cols,nrow=i_row)
  summary_matrix <- as.data.frame(summary_matrix)
  for(i in 1: i_row){
    
    summary_matrix[i,1] <- mean(DATA[,i],na.rm=TRUE)

    summary_matrix[i,2] <- sd(DATA[,i],na.rm=TRUE)
    summary_matrix[i,3] <- min(DATA[,i],na.rm=TRUE)
    summary_matrix[i,4] <- max(DATA[,i],na.rm=TRUE)
    summary_matrix[i,5] <- skewness(DATA[,i],na.rm=TRUE)
    summary_matrix[i,6] <- kurtosis(DATA[,i],na.rm=TRUE)
    summary_matrix[i,7] <- length(na.omit(DATA[,i]))
    summary_matrix[i,8] <- jarque.bera.test(DATA[,i],na.rm=TRUE)$p.value
    
    
    
  }
  colnames(summary_matrix) <- c("Mean","St. Dev.","Min","Max","Skew","Kurt","Numb. Observ.", "Norm. Dist.")
  rownames(summary_matrix) <- names(DATA)
  
  return(as.data.frame(summary_matrix))
  
  
}
tests_for_adf <- function(DATA,maxlag=5){
  # Function for testing data set for a unit root using the ADF test (all 3 types) plus KPSS test w.r.t. lag length 
  
  require(tseries)
  require(fUnitRoots)
  require(urca)
  
  i_cryptos = length(DATA[1,])
  
  test_values_kpss <-  matrix(ncol= maxlag,nrow=i_cryptos)
  #p_values_adf <- matrix(ncol= maxlag,nrow=i_cryptos)
  p_values_adf_nc<- matrix(ncol= maxlag,nrow=i_cryptos)
  p_values_adf_c<- matrix(ncol= maxlag,nrow=i_cryptos)
  p_values_adf_ct<- matrix(ncol= maxlag,nrow=i_cryptos)
  lag_names <- matrix(ncol=maxlag,nrow=1)
  
  for(i in 1: i_cryptos){

    for(i_lag in 1: maxlag){
      
      lag_names[1,i_lag] <-  paste("LAG", i_lag)
      
      kpss_test_object <- ur.kpss(DATA[,i],type="tau",use.lag = i_lag)
      summary_kpss <- summary(kpss_test_object)
      kpss_teststat <- summary_kpss@teststat
      kpss_critical <- summary_kpss@cval
      
      adf_test_object <- adf.test(na.omit(DATA[,i]), alternative = c("stationary"),
               k = i_lag)
      
      adf_test_object_nc <- adfTest(as.ts(na.omit(DATA[,i])), lags = i_lag, type = "nc")
      adf_test_object_c <- adfTest(as.ts(na.omit(DATA[,i])), lags = i_lag, type = "c")
      adf_test_object_ct <- adfTest(as.ts(na.omit(DATA[,i])), lags = i_lag, type = "ct")
      
      
      test_values_kpss[i,i_lag] <- kpss_teststat
      cv_values_kpss <- kpss_critical
      
      #p_values_adf[i,i_lag] <- adf_test_object$p.value
      
      p_values_adf_nc[i,i_lag] <- adf_test_object_nc@test$p.value
      p_values_adf_c[i,i_lag] <- adf_test_object_c@test$p.value
      p_values_adf_ct[i,i_lag] <- adf_test_object_ct@test$p.value
      
    }
  
  }
  
  row.names(test_values_kpss) <- names(DATA)
  colnames(test_values_kpss) <-lag_names
   
  #row.names(p_values_adf) <- names(DATA)
  #colnames(p_values_adf) <-lag_names
  
  row.names(p_values_adf_c) <- names(DATA)
  colnames(p_values_adf_c) <-lag_names
  
  row.names(p_values_adf_nc) <- names(DATA)
  colnames(p_values_adf_nc) <-lag_names
  
  row.names(p_values_adf_ct) <- names(DATA)
  colnames(p_values_adf_ct) <-lag_names
  
  return(list(kpss=test_values_kpss,kpss_critical = kpss_critical,adf_c = p_values_adf_c, adf_nc = p_values_adf_nc, adf_ct = p_values_adf_ct))
  
  
}


plot_all_series_in_one <- function(data){
  # Function to plot all data from given set in one w.r.t. log prices (ln) 
  
  require(broom)
  require(magrittr)
  
  PLOT <- tidy(log(data)) %>% ggplot(aes(x=index,y=value, color=series)) + geom_line() + labs(x="Time",y="Log Prices",color="Legend")
  png("ALL-CRYPTOS.png")
  print(PLOT)
  dev.off()
  
}

unitroot_sadf_gsadf <- function(DATA,i_lag,logs=0){
  ### Function to estimate SADF test results w.r.t. CV plus plot BADF test results 

  require(exuber)
  require(ggplot2)
  i_cryptos = length(DATA[1,])
  names_data <- names(DATA)
  sadf_test <- matrix(nrow=i_cryptos,ncol=4)
  rownames(sadf_test) <- names(DATA)
  colnames(sadf_test) <- c("Test Stat","90%","95%",'99%')
  for(i in 1: i_cryptos){
    print(paste(i, "of", i_cryptos,sep = " "))
    
    
    DATA_run <- as.data.frame(na.omit(DATA[,i]))
    
    if(logs==1){
      
      DATA_run <- log(DATA_run)
      
    }
  
    names(DATA_run)  <- names_data[i] 
    pre_save <- radf(DATA_run, lag = i_lag)
    window <- attr(pre_save, 'minw')
    
    mc <- mc_cv(n = 1000,nrep = 1000,minw = window)
    dates_extract <- data.frame(attr(pre_save,"index"))
    
    diff_l <- length(dates_extract[,1]) - length(as.matrix(pre_save$bsadf))
    
    dates_extract <- dates_extract[(diff_l+1):length(dates_extract[,1]),1]
    
    df = data.frame(dates=dates_extract,values=pre_save$badf )
  
    p1 <- ggplot(df) + geom_line(aes(y = df[,2], x = dates)) +  geom_hline(yintercept=mc$adf[2], linetype="dashed", color = "red") +
          labs(x="Time",y=paste("Test statistic ",names_data[i] ,sep=""))
    
    png(paste0(names_data[i], "_sadf.png"))
    print(p1)
    dev.off()
    
    sadf_test[i,1] <- pre_save$sadf
    sadf_test[i,2:4]<- mc$sadf_cv

  }    
 
  
  
  
  return(sadf_test)

}

calc_returns <- function(DATA,logs=0){
  # Function to calculate returns of data set 
  
  DATA <- na.omit(DATA)
  length_nafree <- length(DATA[,1])
  i_number_data <- length(DATA[1,])
  
  for(i in 1:i_number_data){
    
    a <- DATA[,i]
    
    if(logs==0){
      returns <-  diff(a)/a[-length(a)]
    }else{
      returns <-  diff(a)
    }
    
    if(i==1){
      
      return_mat <- returns
      
    }else{
      
      return_mat <- cbind(return_mat,returns)
      
    }
    
    
  }
  
  colnames(return_mat) <- names(DATA)
  
  return(na.omit(return_mat))
  
  
}

granger_causality_bivariate <- function(DATA,lags,logs=0){
  #Function to form a VAR model to apply the bivariate Granger causality test
  
  require(vars)
  
  
  i_time_series <- length(DATA[1,])
  v_names <- names(DATA)
  
  p_values_granger <- matrix(ncol=i_time_series,nrow= i_time_series)
  colnames(p_values_granger) <- names(DATA)
  rownames(p_values_granger) <- names(DATA)
  
  for(i in 1:i_time_series){
    
    subset <- subset(v_names, v_names != v_names[i])
    
    for(k in 1:length(subset)){
      
      data_subset <- na.omit(cbind(DATA[,i],DATA[,paste(subset[k])]))
      
      data_subset_return <- calc_returns(data_subset,logs)
      
      
      var <- VAR(data_subset_return, p = lags, type = "const")
      save_pv <- causality(var, cause = v_names[i])
      
      p_values_granger[i,match(subset[k],v_names)] <- save_pv$Granger$p.value
      
      
    }
    
  }
  
  rownames(p_values_granger) <- names(DATA)
  return_list <- list(granger= p_values_granger)
  return(return_list)
  
}






############ end #################
#down below not used functions for thesis but taken into consideration..

get_price_plots <- function(DATA){
  #Function to create a price plot of set of cryptos 
  
  require(ggplot2)
  require(profvis)
  
  i_time_series <- length(DATA[1,])
  
  DATA_plot <- DATA['2013-04-28::2018-07-13']
  
  
  names_data <- names(DATA)
  
  for(i in 1: i_time_series){
    print(i)
    plot_gg <- ggplot(DATA_plot[,i], aes(x = Index, y = DATA_plot[,i])) + geom_line() +
      labs(x="Time",y=paste(names_data[i])) + ggtitle(paste("Price development of", names_data[i],sep=" "))
    
    png(paste0(names_data[i], "_priceplot.png"))
    print(plot_gg)
    dev.off()
    
    
    
  }
  
}


critical_values_bootstrap <- function(DATA,i_boots){
  #Function to create CV w.r.t. the bootstrap method
  
  
  i_time_series <- length(DATA[1,])
  cv_adf <- matrix(nrow=i_time_series,ncol = 3)
  cv_sadf <- matrix(nrow=i_time_series,ncol = 3)
  cv_gsadf <- matrix(nrow=i_time_series,ncol = 3)
  
  colnames(cv_adf) <- c("90%","95%",'99%')
  row.names(cv_adf) <- names(DATA)
  colnames(cv_sadf) <- c("90%","95%",'99%')
  row.names(cv_sadf) <- names(DATA)
  colnames(cv_gsadf) <- c("90%","95%",'99%')
  row.names(cv_gsadf) <- names(DATA)
  
  for(i in 1:length(DATA[1,])){
    
    
    cv <- wb_cv(na.omit(DATA[,i]),nboot=i_boots,parallel=FALSE)
    
    cv_adf[i,] <- cv$adf_cv
    cv_sadf[i,] <- cv$sadf_cv
    cv_gsadf[i,] <- cv$gsadf_cv
    
  }
  
  returnlist <- list(cv_adf=cv_adf,cv_sadf=cv_sadf,cv_gsadf=cv_gsadf)
  return(returnlist)
  
}
get_box_plots <- function(DATA){
  #Function to create box plots of data set 
  
  require(ggplot2)
  require(gridExtra)
  
  i_time_series <- length(DATA[1,])
  
  DATA_plot <- DATA['2013-04-28::2018-07-13']
  
  
  names_data <- names(DATA)
  
  for(i in 1: i_time_series){
    print(i)
    
    
    
    plot_gg <- ggplot(data = DATA_plot, aes(y=DATA_plot[,i],x="")) + labs(y="Price in USD",x=paste(names_data[i])) +
      geom_boxplot()
    png(paste0(names_data[i], "_boxplot.png"))
    print(plot_gg)
    dev.off()
    
  }
  
  
}
