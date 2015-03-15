rm(list = ls())
library(ISLR)
nci.labs = NCI60$labs
nci.data = NCI60$data

dim(nci.data)
nci.labs[1:4]
table(nci.labs)

pr.out = prcomp(nci.data, scale = T)
Cols <- function(vec){
  cols = rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow = c(1,2))
plot(pr.out$x[,1:2], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
plot(pr.out$x[,c(1,3)], col = Cols(nci.labs), pch = 19, xlab = "Z1", ylab = "Z2")
summary(pr.out)
plot(pr.out)
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow = c(1,2))
plot(pve, type = "o", ylab = "PVE", xlab = "Pricipal Component", col = "blue")
plot(cumsum(pve), type = "o", ylab = "Comulative PVE", xlab = "Principla Component", col = "brown3")


sd.data = scale(nci.data)

par(mfrow=c(1,3))
data.dist=dist(sd.data)
plot(hclust(data.dist), labels=nci.labs, main="Complete
Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="average"), labels=nci.labs,
     main="Average Linkage", xlab="", sub="",ylab="")
plot(hclust(data.dist, method="single"), labels=nci.labs,
     main="Single Linkage", xlab="", sub="",ylab="")
