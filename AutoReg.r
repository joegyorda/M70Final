autoreg <-
function(data,max.predict)
{
y=data

n.dates=dim(y)[2];n.stocks=dim(y)[1]

### Split by stocks here

a = matrix(rep(0,n.stocks * n.dates),ncol = n.stocks)
b = matrix(rep(0,n.stocks * n.dates),ncol = n.stocks)
# for every data point (date in data)
for(i in 1:n.dates-1){
  # Predict the next day (i+1) by the previous day (i)
  o = lm(X[i+1]~X[i])
  a[i,] = summary(o)$coefficients[1, 1]
  b[i,] = summary(o)$coefficients[2, 1]
}
mu = mean(a + (1-b)%*%X)

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

