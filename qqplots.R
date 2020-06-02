source("m70final.R")
source("markowitzCode.R")

X = parse.data("all_stocks_5yr.csv")
X = X[2:length(X)]


# X = as.matrix(X)
# X = log(X)

X= get.ret.mat(X)
# X = X[,2:dim(X)[2]] - X[,1:dim(X)[2] - 1]
X= ret.mat[1:48,]
X = t(X)

nc = ncol(X)

# pdf("qqplots.pdf",width=12,height=16)
png("acrossqqplot.png" ,width=3000,height=2000)

par(mfrow=c(6,8))
par(mar=c(4,4,2,2))

eigens = eigen(cov(X))$vectors

for (i in 1:nc) {
	x = sort(X %*% eigens[,i])
	x = (x - mean(x))/sd(x)
	n = length(x)
	th.qnt = qnorm(((1:n) - 0.5)/n)
	q = seq(-5,5,length=1000)
	th.p = pnorm(q)
	lambda = 0.95
	Zl=qnorm((1+lambda)/2)
	lb=qnorm(th.p-Zl*sqrt(th.p*(1-th.p)/n))
	ub=qnorm(th.p+Zl*sqrt(th.p*(1-th.p)/n))
	plot(th.qnt,x,xlab="Theoretical quantile",ylab="Empirical quantile")
	lines(q,lb,col=2,lwd=1.4)
	lines(q,ub,col=2,lwd=1.4)
}

dev.off()

png("withinqqplot.png" ,width=3000,height=2000)

par(mfrow=c(6,8))
par(mar=c(4,4,2,2))

eigens = eigen(cov(X))$vectors

for (i in 1:nc) {
	x = sort(X[,i])
	x = (x - mean(x))/sd(x)
	n = length(x)
	th.qnt = qnorm(((1:n) - 0.5)/n)
	q = seq(-5,5,length=1000)
	th.p = pnorm(q)
	lambda = 0.95
	Zl=qnorm((1+lambda)/2)
	lb=qnorm(th.p-Zl*sqrt(th.p*(1-th.p)/n))
	ub=qnorm(th.p+Zl*sqrt(th.p*(1-th.p)/n))
	plot(th.qnt,x,xlab="Theoretical quantile",ylab="Empirical quantile")
	lines(q,lb,col=2,lwd=1.4)
	lines(q,ub,col=2,lwd=1.4)
}

dev.off()
