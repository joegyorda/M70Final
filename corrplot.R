source("m70final.R")

library(ggcorrplot)

X = parse.data("all_stocks_5yr.csv")
stocks = t(X[,1])
X = as.matrix(X[2:length(X)])
R = cor(t(X))

iR = iiR = solve(R)
n = dim(R)[1]
for(i in 1:n) {
  for(j in 1:n) {
    iR[i,j] = -iiR[i,j]/sqrt(iiR[i,i]*iiR[j,j])
  }
}
rownames(R) = colnames(R) = rownames(iR) = colnames(iR) = stocks

pdf("heatmap.pdf", colormodel="cmyk")
ggcorrplot(R, hc.order=T, colors= c("green", "blue", "red"),outline.color="white",hc.method="average",
           tl.cex=1,tl.srt=90,title="Correlation Heat Map of 468 Stocks")
dev.off()

pdf("partialheatmap.pdf", colormodel="cmyk")
ggcorrplot(iR, hc.order=T, colors= c("green", "blue", "red"),outline.color="white",hc.method="average",
           tl.cex=1,tl.srt=90,title="Partial Correlation Heat Map of 468 Stocks")
dev.off()
