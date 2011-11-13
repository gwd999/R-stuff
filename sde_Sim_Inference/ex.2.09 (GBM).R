#ex.2.09. page 71 
#Geometric Brownian Motion
require(sde)
set.seed(123)
d<-expression(x)
s<-expression(0.5*x)
sde.sim(X0=10, drift=d, sigma=s)->X
plot(X, main="Geometric Brownian Motion")