library(rggobi)
g <- ggobi(iris)
clustering <- hclust(dist(iris[,1:4]),
method="average")
glyph_colour(g[1]) <- cutree(clustering, 3)

#ANIMATION
df <- data.frame(
x=1:2000,
y=sin(1:2000 * pi/20) + runif(2000, max=0.5)
)
g <- ggobi_longitudinal(df[1:100, ])
df_g <- g[1]
for(i in 1:1901) {
df_g[, 2] <- df[i:(i + 99), 2]
}

#Edge data
library(graph)
library(sna)
data(business, marital, florentineAttrs)
g <- ggobi(florentineAttrs)
edges(g) <- business
edges(g) <- marital

# Case study
ellipse<-function(data, npoints=1000, cl=0.95, mean=colMeans(data),
cov=var(data), n=nrow(data))
{
norm.vec <- function(x) x / sqrt(sum(x^2))
p <- length(mean)
ev <- eigen(cov)
sphere <- matrix(rnorm(npoints*p), ncol=p)
cntr <- t(apply(sphere, 1, norm.vec))
cntr <- cntr %*%diag(sqrt(ev$values))%*% t(ev$vectors)
cntr <- cntr * sqrt(p * (n-1)*qf(cl, p, n-p) / (n * (n-p)))
if (!missing(data))
colnames(cntr) <- colnames(data)
cntr + rep(mean, each=npoints)
}
#Output of function above
ggobi(ellipse(mean=c(0,0), cov=diag(2), n=100))
cv<-matrix(c(1,0.15,0.25,1), ncol=2)
ggobi(ellipse(mean=c(1,2), cov=cv, n=100))
ggobi(ellipse(mean=c(0,0,1,2), cov=diag(4), n=100))
ggobi(ellipse(matrix(rnorm(20), ncol=2)))