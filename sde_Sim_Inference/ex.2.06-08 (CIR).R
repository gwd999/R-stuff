#ex.2.06,.07,.08 page 71
#3 Versions of the Cox-Ingersol-Ross process
#with (theta1, theta2, theta3)=(6,3,2)
require(sde)
set.seed(123)
d<-expression( 6-3*x )
s<-expression( 2*sqrt(x) )
#CIR-1
sde.sim(X0=10, drift=d, sigma=s)->X
plot(X, main="CIR")
#CIR-2 Milstein scheme
#"d" and "s" stay the same
d<-expression( 6-3*x )
s<-expression( 2*sqrt(x) )
s.x<-expression( 1/sqrt(x) )
set.seed(123)
sde.sim(X0=10, drift=d, sigma=s, sigma.x=s.x, method="milstein")->X
plot(X, main="CIR Milstein")
#CIR-3: with Euler-scheme on transformed process Y(t)=Sqrt(X(t))
#with (theta1, theta2, theta3)=(6,3,2)
set.seed(123)
d<-expression( (6-3*x^2-1)/(2*x) )
s<-expression( 1 )
sde.sim(X0=sqrt(10), drift=d, sigma=s, sigma.x=0)->Y #X0=sqrt(10)because of transform
plot(Y^2, main="CIR Euler transformed [Y(t)=Sqrt(X(t))]") 
