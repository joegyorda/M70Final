autoreg <-
function(X)
{
X = as.matrix(X)

n.dates=dim(X)[2];n.stocks=dim(X)[1]

a = c(); b=c()
# for every data point (date in data)
flag = FALSE

for(i in 1:(n.stocks)){
  x1 <- X[i,1:(n.dates-1)]
  y <- X[i,2:n.dates]
  datar <- data.frame(y=y,x1=x1)
  o = lm(y~x1,datar)
  a = c(a,summary(o)$coefficients[1, 1])
  b = c(b,summary(o)$coefficients[2, 1])
}

mu = rowMeans(a + X*(1-b))
return(mu)
}


####### Original AMAZ.r function #########
#yy=y
#y=yy[(maxlag+1):n] #stock to predict
#X=matrix(ncol=maxlag,nrow=n-maxlag)
#for(j in 1:maxlag){
#  X[,j]=yy[(maxlag-j+1):(n-j)]  # maxlag past values
#} 
### Use previous eight dates to predict next
#o8=lm(y~X) #autoregression model with maxlag predictors
### Summary
#print("Autoregression model with 8 lags:")
#print(summary(o8))
#R2=summary(o8)$r.squared
### Only use previous date to predict next
#o1=lm(y~X[,1])
### Summary
#print(summary(o1))
#R20=summary(o1)$r.squared
#R2C=(R2-R20)/(1-R20)
#}

