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
  returnsNoRealloc <- cross_val(backtest=1,d,capital,reall=0)
  returnsRealloc <- cross_val(backtest=1,d,capital,reall=1)
  returnsLong <- cross_val(backtest=2,d,capital,reall=10)
  
  write.csv(returnsNoRealloc, file= "returnsNoRealloc.csv")
  write.csv(returnsRealloc, file = "returnsRealloc.csv")
  write.csv(returnsLong, file = "returnsLong.csv")
  write.csv(returnsNoRealloc2, file= "returnsNoRealloc2.csv")
  write.csv(returnsRealloc2, file = "returnsRealloc2.csv")
  #graphing(dates = ends.test,hca_returns = returnsNoRealloc[1,],bullet_returns = returnsNoRealloc[2,],cap=capital)
  #graphing(dates = ends.test,hca_returns = returnsRealloc[1,],bullet_returns = returnsRealloc[2,],cap=capital)
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

cross_val = function(backtest=1, d, capital, reall){
  #First start date for training
  #First start date for training
  first = ymd(names(d)[2])
  #Determine last start date for training
  last = ymd(names(d)[ncol(d)])-months(12)
  
  callast = last + years(2)
  
  #Load biz calendar
  load_quantlib_calendars(ql_calendars = 'UnitedStates/NYSE', from=first, to=callast)
  
  #Train on 6 months, test on 6 months
  if(backtest==1){
    
    # Start dates for training (also end dates for testing) using biz days of NYSE
    starts.train = bizseq(from=first, to=last,'QuantLib/UnitedStates/NYSE')
    
    # End dates for training (also start dates for testing)
    # Add 6 months to start date, round to nearest NYSE business day
    ends.train = add_with_rollback(starts.train, months(6), roll_to_first = TRUE)
    ends.train = adjust.next(ends.train,'QuantLib/UnitedStates/NYSE')
    
    starts.test = ends.train
    ends.test = add_with_rollback(starts.test, months(6), roll_to_first = TRUE)
    ends.test = adjust.next(ends.test,'QuantLib/UnitedStates/NYSE')
    
    returnsNoRealloc = matrix(rep(0,3*length(starts.train)), nrow=3)
    returnsRealloc = matrix(rep(0,3*length(starts.train)), nrow=3)

    for(i in 1:(length(starts.train))){
      
      weights = calc.weights(starts.train[i],ends.train[i],d)
      returns = calc.return(starts.test[i],ends.test[i],weights, capital, 0)
      returnsNoRealloc[,i]=returns
    }
    for(i in 1:(length(starts.train))){
      
      weights = calc.weights(starts.train[i],ends.train[i],d)
      returns = calc.return(starts.test[i],ends.test[i],weights, capital, 1)
      returnsRealloc[,i]=returns
    }
    if(reall==0){return(returnsNoRealloc)}
    if(reall==1){return(returnsRealloc)}
    
    #graphing(dates = ends.test,hca_returns = returnsNoRealloc[1,],bullet_returns = returnsNoRealloc[2,],cap=capital)
  }

  if(backtest==2){
    ends.train = add_with_rollback(first, months(6), roll_to_first = TRUE)
    ends.train = adjust.next(ends.train,'QuantLib/UnitedStates/NYSE')
    starts.test = ends.train
    ends.test = ymd(names(d)[ncol(d)])
    
    numReAllocs = reall
    totalReturns = matrix(rep(0,3*(numReAllocs+1)), nrow=3)
    weights = calc.weights(first,ends.train,d)
    for(i in 0:numReAllocs){
      returns = calc.return(starts.test,ends.test,weights, capital, i)
      totalReturns[,i+1] = returns
    }
    return(totalReturns)
  }

}

### Calculate weights for training period
# Start - start of training period - date object
# End - end of training period - date object

calc.weights = function(start, end, d, returnLow=0.05, returnHigh=0.15){
  # rows are models, columns are stocks
  # Including intervalPortfolio
  weights = matrix(rep(0,3*nrow(d)),nrow=3)
  # Not inclduing interval portfolio
  #weights = matrix(rep(0,2*nrow(d)),ncol=nrow(d))
  
  weights[1,] = hclust.portfolio(t(d %>% select(format(start):format(end))))
  weights[2,] = markBullet(d %>% select(format(start):format(end)))
  weights[3,] = intervalPortfolio(d %>% select(format(start):format(end)), r=returnLow, R=returnHigh)
  return(weights)
}

### Calculate returns for testing period
# Start - start of testing period - date object
# End - end of testing period - date object
# weights - matrix of weights
# realloc - number of times to reallocate
# capital - starting capital
calc.return = function(start,end,weights,capital,realloc){
  capital = rep(capital,nrow(weights))
  realloc = realloc + 1
  
  # get start price
  old_price = d %>% select(format(start))
  # number of days per time period
  numDays = round((end-start)/realloc)
  
  for(i in 1:realloc){
    # Stock bought for time period
    bought = weights*capital
    
    if(i == realloc){
      mid=end
    }
    else{
      # Find end of time period
      mid = add_with_rollback(start, days(numDays), roll_to_first = TRUE)
      mid = adjust.next(mid,'QuantLib/UnitedStates/NYSE')
    }
    
    # Ending prices for time period
    new_price = d %>% select(format(mid))
    # Change in stock prices for time period
    change = as.matrix((new_price - old_price)/old_price)+1
    # Returns for time period
    returns = (bought %*% change)
    
    # New capital
    capital = as.vector(returns)
    
    # New prices
    old_price = new_price
    start = mid
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

################  Graphing Function ################ 
# ### Takes in end dates of testing periods, returns from each model, and initial capital amount
graphing <- function(dates,
                     hca_returns, bullet_returns,cap) {
  
  #int_port_returns = int_port_returns/cap * 100
  hca_returns = hca_returns/cap * 100
  bullet_returns = bullet_returns/cap * 100
  dates = as.Date(dates)
  
  #one dataframe of all relevant plotting data
  dat_df <- data.frame(cbind(hca_returns, bullet_returns))
  
  #colors for legend
  colors <- c("Interval Portfolio" = "#58508d", "HCA" = "#ffa600", "Markowitz Bullet" = "#bc5090")
  
  
  return_plt <- ggplot(data = dat_df) +
    geom_line(aes(y = hca_returns, x = dates, color = "HCA")) +
    geom_line(aes(y = bullet_returns, x = dates, color = "Markowitz Bullet")) +
    labs(y = "Returns (Percentage)", x = "Dates (2013 on)", col = "Model", title = "Performance on 2013-2018 Stock Prices - No Reallocation") +
    scale_color_manual(values = colors) + scale_x_date() + theme_light()
  
  
    final_rets <- c(hca_returns[length(hca_returns)],
                     bullet_returns[length(bullet_returns)])
    
    nms = c("HCA", "Markowitz Bullet")

    return_plt
    
}
