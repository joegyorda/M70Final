# Data is five years from 2013-2018 highs, lows, opens, names
# tidyverse, dplyr needed for data cleaning
# RQuantLib must be installed
require(dplyr)
require(tidyverse)
require(lubridate)
require(bizdays)
# source("markowitzCode.R")


################  Main  ################

main = function(backtest=1,capital=100000){
  d = parse.data("all_stocks_5yr.csv")
  cross_val(model=hclust.portfolio, backtest=backtest,d,capital)
}


################  Data Parsing and Formatting  ################
parse.data = function(path){
  ### Read CSV
  findat <- read.csv(path, stringsAsFactors=F)

  ### Truncate Data, just for initial time-consumption purposes, should be on all data
  findat$date = as.POSIXct(findat$date)
  findat_trunc = findat

  ### Reformat Data
  findat_trunc$open
  res <- findat %>% pivot_wider(id_cols = "Name", values_from = "open", names_from = c("date"))

  ### Information on Matrices
  names(findat)
  dim(findat)
  dim(res)

  res = na.omit(res)
  return(res)
}

cross_val = function(model=hclust.portfolio, backtest=1, d, capital){
  #First start date for training
  first = ymd(names(d)[2])
  #Determine last start date for training
  last = ymd(names(d)[ncol(d)])-months(12)
  callast = last + years(2)

  #Load biz calendar
  load_quantlib_calendars(ql_calendars = 'UnitedStates/NYSE', from=first, to=callast)

  
  #Train on 6 months, test on 6 months, no redistribution
  if(backtest==1 | backtest==2){

    # Start dates for training (also end dates for testing) using biz days of NYSE
    starts.train = bizseq(from=first, to=last,'QuantLib/UnitedStates/NYSE')
    
    # End dates for training (also start dates for testing)
    # Add 6 months to start date, round to nearest NYSE business day
    ends.train = add_with_rollback(starts.train, months(6), roll_to_first = TRUE)
    ends.train = adjust.next(ends.train,'QuantLib/UnitedStates/NYSE')
    
    starts.test = ends.train
    ends.test = add_with_rollback(starts.test, months(6), roll_to_first = TRUE)
    ends.test = adjust.next(ends.test,'QuantLib/UnitedStates/NYSE')
    
    #Train on 6 months, test on 6 months, redistribute at 3 months
    if(backtest==2){
      mids.test = add_with_rollback(starts.test, months(3), roll_to_first = TRUE)
      mids.test = adjust.next(ends.test,'QuantLib/UnitedStates/NYSE')
    }
    
    # Matrix of returns by stock by training set (6 month period)
    size= length(starts.train)*nrow(d)
    #returnf = matrix(rep(0,size), nrow=nrow(d))
    returnf = c()
    for(i in 1:(length(starts.train))){

      # Determine weights
      weights.clust = model(d %>% select(format(starts.train[i]):format(ends.train[i])))
      # Determine how much of each stock we buy

      bought = weights.clust*capital
      # Change in stock price

      #print(ends.test[i])
      old_price = d %>% select(format(starts.test[i]))
      new_price = d %>% select(format(ends.test[i]))
      if(backtest==1){ 
        change = (new_price - old_price) / (old_price)
        returnf=  c(returnf,(sum((bought * change)[[1]]))/capital)
      }
      
      if(backtest==2){
        mid_price = d %>% select(format(mids.test[i]))
        mid_change = (old_price - mid_price)/old_price
        mid_bought = weights.clust*sum((bought * mid_change)[[1]])
        
        change = (new_price - mid_price)/mid_price
        returnf=  c(returnf,(sum((mid_bought * mid_change)[[1]]))/capital)
      }
      #returnf[,i] <- (bought * change)[[1]]
    }
    #View(returnf[ , (length(returnf[1,])-10):length(returnf[1,])])
  }

  #Train only on first six months, test on increasing intervals of time
  if(backtest==3){
    ends.train = seq(seq(first, by="6 months", length.out=2)[2], by="3 year",length.out=20)
  }


}

################  Hierarchical Clustering Optimization ################

##### Weight Allocation
calc.allocs = function(clust) {
  int.clusts = list()
  merge = clust$merge
  #levels = diff merging steps
  levels = rep(0, nrow(merge) + 1)
  for (i in 1:nrow(merge)) {
    int.clust = vector()
    for (idx in merge[i,]) {
      # if idx <0m eans first column entry negative, which means one stock, not one cluster
      #this if decides what heppens if it's individual stocks
      if (idx < 0) int.clust = c(int.clust, -idx)
      else int.clust = c(int.clust, int.clusts[[idx]])
    }
    for (idx in int.clust) levels[idx] = levels[idx] + 1
    int.clusts[[i]] = int.clust
  }
  return(0.5 ^ levels)
}

##### Distance Measure
um.dist = function(x) {
  return(sqrt(2*(1-cor(x))))
}

##### Main Function
hclust.portfolio = function(d, method="average") {
  d = um.dist(d)
  clust = hclust(as.dist(d), method=method)
  allocs = calc.allocs(clust)
  return(allocs)
}

###View(hclust.portfolio(res[,2:ncol(res)]))
# main()
