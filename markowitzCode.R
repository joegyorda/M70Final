# data prep
symb=c("HPQ","HD","MS","M","VZ","T","S","C","TGT","WMT","GM","XOM","F","YHOO","IBM","GOOGL","MSFT")
ns=length(symb)
all=as.data.frame(matrix(NA,ncol=ns,nrow=10000))
names(all)=symb
min.ni=10000
for(i in 1:ns) {
  tabi=read.csv(paste("/users/josephgyorda/desktop/StatBook/stocks/",symb[1],".csv",sep=""),stringsAsFactors=F)
  ni=nrow(tabi)
  if(min.ni>ni) min.ni=ni
  all[1:ni,i]=tabi[,7]
}
all = all[seq(from=min.ni,to=1,by=-1),]        # adj closing price for each stock
data = log(all)

markBullet = function(data) {
  data = log(data)      # log transform data (assume was not transformed already)
  omega = cov(data)
  mu = as.matrix(colMeans(data))
  one = matrix(1, nrow=ncol(data))               # column vector of all 1's
  
  A = as.single(t(mu)%*%solve(omega)%*%one)
  B = as.single(t(mu)%*%solve(omega)%*%mu)
  C = as.single(t(one)%*%solve(omega)%*%one)
  D = B*C-A^2
  
  r_opt = A/C
  r_min = r_opt - .1
  r_max = r_opt + .1
  r_vals = seq(r_min, r_max, .01)   # sequence of r values to use
  
  avals = (rvals*C-A) / D
  n = length(avals)
  w_tildes = matrix(0, nrow=ncol(data), ncol=n)
  volatilities = rep(0, n)
  
  for (i in 1:n) {
    w_tilde = solve(omega)%*%(avals[i]*mu + ((1-avals[i]*A)/C)*one)  # formula 2 in paper
    w_tildes[,i] = w_tilde
    volatility = sqrt(t(w_tilde)%*%omega%*%w_tilde)
    volatilities[i]  = volatility
  }
  
  volatilities = volatilities*100  # convert to percent
  
  print(colSums(w_tildes))  # should all be 1 since all weights must sum to 1
  
  # determine optimal weights
  index = which.min(r_opt-rvals>0)  # index of optimal r value (r = A/C)
  optimal_weights = w_tildes[, index] 
  
  # display Markowitz bullet
  #annual_return = ((1+rvals/365)^365-1)
  annual_return = rvals
  
  plot(volatilities, annual_return, type="l", main="Markowitz Bullet for X Stock Data",
       xlab="Annual Volatility (Percent)", ylab="Annual Expected Return (Percent)")
  
  return(optimal_weights)
}

optimal_weights = markBullet(all)
cat("Optimal weight vector: ", optimal_weights)





# hierarchical clustering test code
weights = hclust.portfolio(data)
weights = rbind(symb, weights)
weights

#hclust_avg = hclust(d, method="average")
#plot(hclust_avg)






