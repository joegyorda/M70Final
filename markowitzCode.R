library(pracma)

# source("m70final.R")
# X = parse.data("all_stocks_5yr.csv")
# markBullet(X, plotting=T)

# gets the matrix of daily returns
get.ret.mat = function(X) {
  X = X[2:length(X)]
  # first.date = names(X)[1]
  # last.date = names(X)[length(X)]
  # week.seq = seq(ymd(first.date), ymd(last.date), by="1 week")
  # load_quantlib_calendars(ql_calendars = 'UnitedStates/NYSE', from=first.date, to=last.date)
  # week.seq = adjust.next(week.seq, 'QuantLib/UnitedStates/NYSE')
  # week.seq = as.character(week.seq)
  # X = X[week.seq]
  X = as.matrix(X)
  ret.mat = (X[,2:dim(X)[2]] - X[,1:dim(X)[2] - 1]) / X[,1:dim(X)[2] - 1]
  return(ret.mat)
}

# computes generalized inverse based on Jordan decomposition
ginv = function(X) {
  n = dim(X)[1]
  eig = eigen(X, symmetric=T)
  vals = eig$values
  vecs = eig$vectors
  vals[vals!=0] = 1/vals[vals!=0]
  inv = vecs %*% diag(vals,n,n) %*% t(vecs)
  return(inv)
}

# finds the optimal a for interval allocation
find.a = function(r,R,mu,omega){
  obj.func = function(a, mu, omega, p, P, C, Q) {
    left.side = pnorm((P-a)/sqrt(C * (Q +a^2)))
    right.side= pnorm((p-a)/sqrt(C*(Q+a^2)))
    return(left.side - right.side)
  }
  omega.inv = solve(omega)
  
  A = as.single(t(mu)%*%omega.inv%*%one)
  B = as.single(t(mu)%*%omega.inv%*%mu)
  C = as.single(t(one)%*%omega.inv%*%one)
  D = B*C-A^2
  P=(R*C-A)/D
  p=(r*C-A)/D
  Q=1/D
  rid=A/C
  sol = optimize(obj.func,c((rid*C-A)/D-10,(rid*C-A)+10),mu, omega, p, P, C, Q, maximum=TRUE)
  return(sol$maximum)
}

# computes weights of optimal interval portfolio
intervalPortfolio = function(X,r,R){
  ret.mat = get.ret.mat(X)
  mu = rowMeans(ret.mat)
  omega = cor(t(ret.mat))
  a = find.a(r,R,mu,omega)
  w_tilde = omega.inv%*%(a*mu + ((1-a*A)/C)*one)
  return(w_tilde)
}

# computes optimal weights of markowitz portfolio
markBullet = function(data, plotting=F) {
  ret.mat = get.ret.mat(data)
  mu = rowMeans(ret.mat)
  omega = cor(t(ret.mat))
  omega.inv = tryCatch({
    solve(omega)
  }, error = function(e) {
    print("Using pinv")
    pinv(omega)
  })

  one = matrix(1, nrow=nrow(data))               # column vector of all 1's

  A = as.single(t(mu)%*%omega.inv%*%one)
  B = as.single(t(mu)%*%omega.inv%*%mu)
  C = as.single(t(one)%*%omega.inv%*%one)
  D = B*C-A^2

  r_opt = A/C
  r_min = r_opt - .001
  r_max = r_opt + .001
  r_vals = seq(r_min, r_max, length=50)   # sequence of r values to use

  avals = (r_vals*C-A) / D
  n = length(avals)
  w_tildes = matrix(0, nrow=nrow(data), ncol=n)
  volatilities = rep(0, n)

  for (i in 1:n) {
    w_tilde = omega.inv%*%(avals[i]*mu + ((1-avals[i]*A)/C)*one)  # formula 2 in paper
    w_tildes[,i] = w_tilde
    volatility = sqrt(t(w_tilde)%*%omega%*%w_tilde)
    volatilities[i]  = volatility
  }

  volatilities = volatilities*100  # convert to percent

  # determine optimal weights
  index = which.min(r_opt-r_vals>0)  # index of optimal r value (r = A/C)
  optimal_weights = w_tildes[, index]

  # display Markowitz bullet
  annual_return = ((1+r_vals)^252.75-1) * 100

  if (plotting == T)
    plot(volatilities, annual_return, type="l", main="Markowitz Bullet for X Stock Data",
        xlab="Annual Volatility (Percent)", ylab="Annual Expected Return (Percent)")

  return(optimal_weights)
}