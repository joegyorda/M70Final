# Data is five years from 2013-2018 highs, lows, opens, names
# tidyverse, dplyr needed for data cleaning
# RQuantLib must be installed
require(dplyr)
require(tidyverse)
require(lubridate)
require(bizdays)
source("markowitzCode.R")


################  Main  ################

main = function(backtest=1,capital=100000){
  d = parse.data("all_stocks_5yr.csv")
  cross_val(backtest=backtest,d,capital)
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

cross_val = function(backtest=1, d, capital){
  #First start date for training
  first = ymd(names(d)[2])
  #Determine last start date for training
  last = ymd(names(d)[ncol(d)])-months(12)
  callast = last + years(2)

  #Load biz calendar
  load_quantlib_calendars(ql_calendars = 'UnitedStates/NYSE', from=first, to=callast)

  starts.train = first
  ends.train = add_with_rollback(starts.train, months(6), roll_to_first = TRUE)
  ends.train = adjust.next(ends.train,'QuantLib/UnitedStates/NYSE')
  starts.test = ends.train
  ends.test = add_with_rollback(starts.test, months(6), roll_to_first = TRUE)
  ends.test = adjust.next(ends.test,'QuantLib/UnitedStates/NYSE')
  
  print(starts.train)
  print(ends.train)
  print(starts.test)
  print(ends.test)
  
  weights = calc.weights(starts.train,ends.train,d)
  print("weights")
  print(weights)
  returns = calc.return(starts.test,ends.test,weights, capital, 0)
  print("returns")
  print(returns)
  print("returns - initial capital")
  print(returns - capital)
  
  
  
  #Train on 6 months, test on 6 months, no redistribution
  #if(backtest==1){

    # Start dates for training (also end dates for testing) using biz days of NYSE
    #starts.train = bizseq(from=first, to=last,'QuantLib/UnitedStates/NYSE')
    
    # End dates for training (also start dates for testing)
    # Add 6 months to start date, round to nearest NYSE business day
    #ends.train = add_with_rollback(starts.train, months(6), roll_to_first = TRUE)
    #ends.train = adjust.next(ends.train,'QuantLib/UnitedStates/NYSE')
    
    #starts.test = ends.train
    #ends.test = add_with_rollback(starts.test, months(6), roll_to_first = TRUE)
    #ends.test = adjust.next(ends.test,'QuantLib/UnitedStates/NYSE')
    
    #returns = c()
    #for(i in 1:(length(starts.train))){

    #}
  #}

}

### Calculate weights for training period
# Start - start of training period - date object
# End - end of training period - date object

calc.weights = function(start, end, d, returnLow=0, returnHigh=0){
  # rows are models, columns are stocks
  # Including intervalPortfolio
  #weights = matrix(rep(0,3*nrow(d)),nrow=3)
  # Not inclduing interval portfolio
  
  weights = matrix(rep(0,2*nrow(d)),ncol=nrow(d))
  
  #print(dim(weights))
  #print(dim(d %>% select(format(start):format(end))))
  #print(dim(as.matrix(hclust.portfolio(d %>% select(format(start):format(end))))))
  View(d %>% select(format(start):format(end)))
  print(hclust.portfolio(d %>% select(format(start):format(end))))
  weights[1,] = hclust.portfolio(d %>% select(format(start):format(end)))
  weights[2,] = markBullet(d %>% select(format(start):format(end)))
  #weights[3,] = intervalPortfolio(d %>% select(format(start):format(end)), r=returnLow, R=returnHigh)
  return(weights)
}

### Calculate returns for testing period
# Start - start of testing period - date object
# End - end of testing period - date object
# weights - matrix of weights
# realloc - number of times to reallocate
# capital - starting capital
calc.return = function(start,end,weights,capital,realloc){
  # get start price
  old_price = d %>% select(format(start))
  # number of days per time period
  if(realloc !=0 ){numDays = (end-start)/realloc}
    
  
  for(i in 0:realloc){
    # Stock bought for time period
    bought = weights*capital
    
    # Find end of time period
    if(realloc !=0 ){
      mid = add_with_rollback(starts.test, days(numDays), roll_to_first = TRUE)
      mid = adjust.next(ends.test,'QuantLib/UnitedStates/NYSE')
    }
    else{
      mid=end
    }

    # Ending prices for time period
    new_price = d %>% select(format(mid))
    
    # Change in stock prices for time period
    change = (old_price - new_price)/old_price
    
    # Returns for time period
    # Make sure change is a column vector - stocks as rows 
    returns = (bought %*% change)#[[1]]
    
    # New capital
    capital = sum(returns)
    
    # New prices
    old_price = new_price
  }
  return(capital)
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
