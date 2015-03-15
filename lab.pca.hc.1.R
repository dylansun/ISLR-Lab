states = row.names(USArrests)
names(USArrests)
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

pr.out = prcomp(USArrests, scale = T)
names(pr.out)
summary(pr.out)

## plot the first two pricipal componets as follows
biplot(pr.out, scale = 0)
pr.out$x
pr.out$scale
pr.out$sdev
pr.out$rotation


##k-means clustering
set.seed(2)
x = matrix(rnorm(100), ncol=2)
x[1:25,1] = x[1:25, 1] + 3
x[1:25,2] = x[1:25, 2] - 4
x
km.out = kmeans(x, 2)
km.out 
plot(x, col = (km.out$cluster + 1), main="K-Means clustering results with k=2")

set.seed(4)
km.out = kmeans(x, 3)
plot(x, col = (km.out$cluster + 1), main="K-Means clustering results with k=2")


## Hierarchical clustering
hc.complete = hclust(dist(x), method = "complete")
hc.average = hclust(dist(x), method = "average")
hc.single = hclust(dist(x), method = "single")
par(mfrow= c(1,3))
plot(hc.complete, main = "Complete Linkage", xlab = "", ylab = "", sub="", cex = .9)
plot(hc.average, main = "Average Linkage", xlab = "", ylab = "", sub="", cex = .9)
plot(hc.single, main = "single Linkage", xlab = "", ylab = "", sub="", cex = .9)

par(mfrow = c(1,3))
plot(x, col = (cutree(hc.complete,2)+ 1), main="hierachical clustering complete")
plot(x, col = (cutree(hc.average,2)+ 1), main="hierachical clustering average")
plot(x, col = (cutree(hc.single,2)+ 1), main="hierachical clustering single")

