set.seed(123)
a<-rnorm(10)
x<-1:10
z<-lm(a~x)
z
plot(a)
abline(z)
tmp<-spline(x,a, method='natural')
plot(a)
lines(tmp)
line(tmp)
