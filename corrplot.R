source("m70final.R")

library(ggcorrplot)

X = parse.data("all_stocks_5yr.csv")
X = as.matrix(X[2:length(X)])
R = cor(t(X))

iR = iiR = solve(R)
n = dim(R)[1]
for(i in 1:n) {
	for(j in 1:n) {
		iR[i,j] = -iiR[i,j]/sqrt(iiR[i,i]*iiR[j,j])
	}
}

pdf("heatmap.pdf", colormodel="cmyk")
ggcorrplot(R, hc.order=T, colors= c("green", "blue", "red"),outline.color="white",hc.method="average")
dev.off()

pdf("partialheatmap.pdf", colormodel="cmyk")
ggcorrplot(iR, hc.order=T, colors= c("green", "blue", "red"),outline.color="white",hc.method="average")
dev.off()