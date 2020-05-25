# Data is five years from 2013-2018 highs, lows, opens, names
require(dplyr)
require(tidyverse)
require(lubridate)

################  Main  ################

main = function(model=1,capital=100000){
  d = parse.data("/Users/student/Downloads/sandp500/all_stocks_5yr.csv")
  cross_val(model=model,d,capital)
  
}


################  Data Parsing and Formatting  ################
parse.data = function(path){
  ### Read CSV
  findat <- read.csv(path)
  
  ### Truncate Data
  findat$date = as.POSIXct(findat$date)
  findat_trunc = findat[1:50000,]
  
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

cross_val = function(model=1, d, capital){
  #First start date for training
  first = ymd(names(d)[2])
  #Determine last start date for training 
  last = ymd(names(d)[nrow(d)])-months(6)
  #Default to friday if weekend
  if((wday(last))==7){last = last - ddays(1)} else 
    if((wday(last))==1){last =  last - ddays(2)}
  
  #Train on 6 months, test on 6 months, no redistribution
  if(model==1){
    # Start dates for training (also end dates for testing)
    starts.train = seq(from=first, to=last, by="day")
    # Delete if weekend
    starts.train <- starts.train[!wday(starts.train)==7 & !wday(starts.train)==1]
    
    # End dates for training (also start dates for testing)
    # Add 6 months to start date
    ends.train = add_with_rollback(starts.train, months(6), roll_to_first = TRUE)
    # If weekend, default to friday
    ends.train[wday(ends.train)==7]<-ends.train[wday(ends.train)==7] - ddays(1)
    ends.train[wday(ends.train)==1]<-ends.train[wday(ends.train)==1] - ddays(2)
    
    # Matrix of returns by stock by training set (6 month period)
    size= length(starts.train)*nrow(d)
    returnf = matrix(rep(0,size), nrow=nrow(d))
    print(returnf)
    for(i in 1:length(starts.train)){
      # Determine weights
      weights.clust = hclust.portfolio(d %>% select(format(starts.train[i]):format(ends.train[i])))
      # Determine how much of each stock we buy
      bought = weights.clust*capital
      # Change in stock price
      change = (d %>% select(format(ends.train[i])) - d %>% select(format(starts.train[i+1]))) / (d %>% select(format(ends.train[i]))) 
      # Save return
      returnf[,i] = bought * change
    }
    print(returnf)
      
  }
  
  #Train on 6 months, test on 6 months, redistribute at 3 months
  if(model==2){
    starts.train = seq(first, by="1 year", length.out=6)
    ends.train = seq(seq(first, by="6 months", length.out=2)[2], by="1 year",length.out=5)
    mids.test = seq(seq(first, by="9 months", length.out=2)[2], by="1 year",length.out=5)
    
   
  }
  
  #Train only on first six months, test on increasing intervals of time
  if(model==3){
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

#View(hclust.portfolio(res[,2:ncol(res)]))
