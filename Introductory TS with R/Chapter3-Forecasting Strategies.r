########CHAPTER 3 FORECASTING STRATEGIES 
#Chap.3.1. Leading variables and Associated variables 
#www <- "http://www.massey.ac.nz/~pscowper/ts/ApprovActiv.dat" #OLD link does not seem to work anymore
www <- "http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/Maine.dat"
Build.dat<-read.table(www, header=T); attach(Build.dat)
App.ts<-ts(Approvals, start=c(1996,1), frequency=4)
Act.ts<-ts(Activity, start=c(1996,1), frequency=4)
ts.plot(App.ts, Act.ts, lty=c(1,3)) #Fig.3.1. shows Building approvals (solid line) and Building activity

#CROSS-CORRELATION 
#Take two ts for variables x and y, that are stationary in the mean and the variance.  
#The variables may each be serially correlated and correlated with each other at different time lags. 
#The combined model is second-order-stationary if all these correlations deoend only on the "lag", and then we can 
#define the Cross-Covariance-function (ccvf) as a function of the lag. 
#ccvf=E[(x(t+k)-mu(x))*(y(t)-mu(y)]

#Cross correlation between building approvals and building activity 
#the ts.union() FUNCTION binds ts with a common frequency, padding with "NA"s to the union of their time coverage. 
#If ts.union() is used within acf() R returns the correlograms for the two variables and the cross-correlogram in 
#a single figure. You can get the numerical values by using the print() FUNCTION.  
acf(ts.union(App.ts, Act.ts)) #Fig.3.2. 
print(acf(ts.union(App.ts, Act.ts))) 
#The ccf can be calculated for any two ts that overlap, but if they have trends or similar seasobal effetcs these will dominate.  
#It may be that we are looking for trends ad seasonalities, but the population ccf is defined for a stationary 
#random process and therefore lets remove trend and seasonality with decompose(), 
#which uses a centred moving average of the four quarters. The use of ccf() follows later. 
app.ran<-decompose(App.ts)$random 
act.ran<-decompose(Act.ts)$random
app.ran.ts<-window(app.ran, start=c(1996,3))
act.ran.ts<-window(act.ran, start=c(1996,3))
acf(ts.union(app.ran.ts, act.ran.ts)) #Fig.3.3. 
ccf(app.ran.ts, act.ran.ts) #Fig.3.4. 
#again with print() you can see the values 
print(acf(ts.union(app.ran.ts, act.ran.ts)))

#Chap.3.3. Bass model (by Frank Bass - 1969) 
#Model statess that the 
#Total number of people that bought a product at time t depends on
#1. the total number of people who eventually buy the product->m 
#2. the coefficient of innovation->p 
#3. the coefficient of imitation->q 
#(One) interpretation of the model: 
#The time from product launch until prchase is assumed to have a probability distribution, that ca be parameterised  
#in terms of p and q. 
#The time to peak is then given with: t(peak)=[log(q)-log(p)]/(p+q)
#EXAMPLE:
#Showing a typical Bass curve by fitting sales per unit of time equation (Eq. 3.12 - implemented below within the nls FUNCTION) 
#to yearly sales of VCRs in the U.S. betwenn 1980 and 1989 (Bass website) using nls() FUNCTION (non-linear least squares)
#cumsum() FUNCTION is usefull for monitoring changes in the mean level of the proecss. 

T79<-1:10
Tdelt<-(1:100)/10
Sales<-c(840, 1470, 2110, 4000, 7590, 10950, 10530, 9470, 7790, 5890)
Cusales<-cumsum(Sales)
Bass.nls<-nls(Sales ~ M*( ((P+Q)^2/P)*exp(-(P+Q)*T79)) / (1+(Q/P)*exp(-(P+Q)*T79))^2, 
				start=list(M=60630, P=0.03, Q=0.38))
summary(Bass.nls)
#The final rounded estimates for m, p, q are 68000 (6.798e+04), 0.0066 (6.594e-03) and 0.64 (6.381e-01) 
#The starting values (P=0.03, Q=0.38) used in the function/model above are p and q for a typical product. 
#The data and fitted curve can be plotted with the following code 
Bcoef<-coef(Bass.nls) 
m<-Bcoef[1]
p<-Bcoef[2]
q<-Bcoef[3] 
ngete<-exp(-(p+q)*Tdelt)
Bpdf<-m*((p+q)^2/p)* ngete / (1+(q/p)*ngete)^2
plot(Tdelt, Bpdf, xlab="Year from 1979", ylab="Sales per year", type='l') #Fig.3.5.
points(T79, Sales) 
Bcdf<-m*(1-ngete)/(1+(q/p)*ngete)
plot(Tdelt, Bcdf, xlab="Year from 1979", ylab="Cumulative sales", type='l') #Fig.3.6.
points(T79, Cusales)  

#Chap.3.4. Exponential smoothing and the Holt-Winters method.skeleton
#Objective is prediction of some future value given past observations up to some time n
#Typical application is forecasting of a well-established product in a stable market 
#The model is: x(t)=mu(t)+w(t) 
#where mu is the non-stationary mean of the process at time t. 
#Given that there is no systematic trend a (intutively) reasonable estimate of the mean at time t -> a(t) is given 
#by the weighted average of our observations at time t and our estimate of the mean at time (t-1). 
#a(t)=alpha*x(t)+(1-alpha)*a(t-1) , 0 < alpha < 1
#a(t) is the exponentially weighted moving average (EWMA) at time t
#alpha determines the amount of smoothing; alpha near one = little smoothing and a(t)~x(t) - this would only be appropriate of 
#chages in the mean level were expected to be large by comparison with sigma. 
#alpha near 0 = highly smoothed estimates - takes little account of the most recent observation. 
#Typical compromise figure for alpha is 0.2, since in practice we typically expect the change in the mean 
#between t-1 and t to be smaller than sigma. R can provide estimates for alpha - by minimisig the the sum of squared one-step-ahead prediction errors.  

#However calculating aplha the R way is not necessarily the best way - if ts is long and the mean changed little, 
#the value of alpha will be small. Small alpha leads to slow response times to unexpected changes - 
#desastrous for instance in the case of sales forecasts of e.g. VCRs when DVDs were invented.   

#In the case of assumption of no systematic trend and no seasonal effects forecasts made at time n for any lead 
#time are just the estimated mean at time n.      
#Example: Complaints to a motoring organisation 
www<-"http://www.massey.ac.nz/~pscowper/ts/motororg.dat" 
Motor.dat<-read.table(www, header=T); attach(Motor.dat)  
Comp.ts<-ts(complaints, start=c(1996,1), freq=12) 
plot(Comp.ts, xlab="Time / months", ylab="Complaints") #Fig.3.7.
#No evidence of trend or seasonal effect => reasonable to use exponential smoothing
#Expo-Smoothing is a special case of the Holt-Winters algorithm, with the parameters set to 0;  
Comp.hw1<-HoltWinters(complaints, beta=F, gamma=F); Comp.hw1
plot(Comp.hw1) #Fig.3.8.
Comp.hw1$SSE
#using diffferent alpha 
Comp.hw2<-HoltWinters(complaints, alpha=.2, beta=F, gamma=F); Comp.hw2
Comp.hw2$SSE
#Coefficient is higher now and SS1PE hsa also increased to 2526 

#Sec.3.4.2 Holt-Winters method.skeleton
#Usually you have more information about the market than Expo-smoothing can take into account. 
#Sales are often seasonal, short period trends may occur ad disappear, competitors may enter the market ... 
#Changes in the seasonally adjusted mean = the LEVEL - from one period to the next will be referred to as SLOPE.  
#The Holt-Winter method generalizes Equation: a(t)=alpha*x(t)+(1-alpha)*a(t-1) , 0 < alpha < 1
#and the additive seasonal form of their updating equations with period 'p' is, resulting in 
#3 equations for level a(t), slope b(t) and seasonal effect s(t) and alpha, beta and gamma are the smoothing parameters. 
#See page 59 (Mitte) for details. 
#Typical weight for alpha is 0.2, typical choices for beta and gamma are also 0.2 
#In R the HoltWinters() FUNCTION can be used to estimate smooothing parameters for the Holt-Winters model by 
#minimising the one-step-ahead prediction errors (SS1PE). 

#Example: Sales of Australian wine 
www<-"http://www.massey.ac.nz/~pscowper/ts/wine.dat"
wine.dat<-read.table(www, header=T); attach(wine.dat) 
sweetw.ts<-ts(sweetw, start=c(1980,1), freq=12)
plot(sweetw.ts, xlab="Time (months)", ylab="Sales (1000 litres)") # Fig.3.9. - Sales of AUS sweet white wine 
sweetw.hw<-HoltWinters(sweetw.ts, seasonal="mult") 
sweetw.hw; sweetw.hw$coef; sweetw.hw$SSE
#Coefficients point to the fact that the level and seasonal variation adapt rapidly, whereas the trend (beta=0) is slow to do so
sqrt(sweetw.hw$SSE/length(sweetw))
sd(sweetw)
#All parameters at 0.2
sweetw.hw2<-HoltWinters(sweetw.ts, seasonal="mult", alpha=0.2, beta=.2, gamma=.2) 
sweetw.hw2; sweetw.hw2$coef; sweetw.hw2$SSE
#Example: Four year ahead forecasts for the air passenger data 
data(AirPassengers) 
AP<-AirPassengers
AP.hw<-HoltWinters(AP, seasonal="mult")
plot(AP.hw) #Fig.3.12.
AP.hw$alpha; AP.hw$beta; AP.hw$gamma
AP.predict<-predict(AP.hw, n.ahead=4*12) # since montly data is predicted for 4 years
ts.plot(AP, AP.predict, lty=1:2) #Fig.3.13. 




































































































 



