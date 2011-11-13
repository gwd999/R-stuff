#####CHAPTER 4 BASIC STOCHASTIC MODELS

#Chap.4.1. White Noise 
#A residual error is the difference between the observed data and the model predicted valule at time t. 
#As the resid errors occur in time => its a ts 
#In Chap.2. we found that ts features such as trend or seasonal variation are reflected in the Correlogram 
#Thus if a model has accounted for all the serial correlation in the data the resid error would be serially UNCORRELATED
#such that the resid errors correlogram would show no obvious pattern! 

#DEFINITION: a ts is discrete white noise (DWN) if the variables are independent and identically distributed (iid)
#with a mean of zero. This implies that variables all have the same variance and Cor(w(i), w(j))=0 for i<>j 
#If in addition the variables follow a normal distribution the series is called Gaussian white noise 

#Sec.4.2.3. Simulation in R 
set.seed(1)
w<-rnorm(100)
plot(w, type='l')
#all in one line
plot(rnorm(100), type='l') 

#Histogram plot
x<-seq(-3,3, length=1000)
hist(rnorm(100), prob=T); points(x, dnorm(x), type='l') 
#Second-order properties of correlogram 
acf(rnorm(100)) #Fig.4.2.
set.seed(2) 

#The only parameter for a white noise series is the variancem which is estimated by the residual variance, 
#adjusted by deggrees of freedom, given in the computer output of the fited model.  

#Chap.4.3. Random Walks (RW) 
#x(t) is a random walk if: x(t)=x(t-1)+w(t), where w(t) is a white noise series 
#back substition gives: x(t)=w(t)+w(t-1)+w(t-2)+...+w(1)

#On Backward-shift or Lag-operators see page 71 and 72! 
#Random walk second-order properties 
# mu=0
#Cov(x(t),x(t+k))=t*sigma^2 - the Cov is a fuction of time => so the process is non-stationary => RW RW only suitable for short term predictions 
#The Correlogram for a RW is characterized by positive autocorrelations that decay very slowly from unity. 

#Sec.4.3.6. The Differencing Operator 
#Differencing adjacent terms of a series can TRANSFORM a NON-STATIONARY into a STATIONARY SERIES! 
#Hence differencing turns out to be a usefull filtering procedure in the study of non-stationary ts.

#Sec. 4.3.7. Simulation 
#Often helpful for ts-model-study to simulate, plot and fit models to this simulated data.  
#Random Walk Simulation:  
x<-w<-rnorm(1000)
for (t in 2:1000)x[t]<-x[t-1]+w[t]
plot(x, type='l') #Fig:4.3.Random Walk 

#Chap.4.4. Fitted models and diagnostic plots 
#Sec.4.4.1. Simulated random walk series 

#Correlogram 
acf(x) #Fig.4.4. 
#Correlogram of the first order differences 
acf(diff(x)) #Fig4.5. - white noise again  

#Example: Exchange rate series 
www<-"http://www.massey.ac.nz/~pscowper/ts/pounds_nz.dat"
Z<-read.table(www, header=T)
Z.ts<-ts(Z, st=1991, fr=4)
Z.hw<-HoltWinters(Z.ts, alpha=1, gamma=0)
acf(diff(Z.ts)) #Fig.4.6. Correlogram - with significant value at lag 1 
acf(resid(Z.hw)) #Fig. 4.7. - Correlogram - significant value at lag 1 which indicates that extension of RW was 
				 #needed for this series - has disappeared after taking the residuals of the "HoltWintered" ts

#Sec.4.4.3. Random Walk with Drift
#by introducing a drift parameter "delta" 
#Taking closing prices of Hewlwtt Packard (HP) 
www<-"http://www.massey.ac.nz/~pscowper/ts/HP.txt" 
HP.dat<-read.table(www, header=TRUE); attach(HP.dat) 
plot(as.ts(Price))
DP<-diff(Price) ; plot(as.ts(DP)) #Fig.4.9. Lag 1 differences of HP daily closing prices
acf(DP) #Acf of lag 1 differences 
#95% confidence intervall for drift parameter 
mean(DP)+c(-1.6445,1.6445)*sd(DP)/sqrt(length(DP))
#in the book they made a mistake and used c(-2,2) 
mean(DP)+c(-2,2)*sd(DP)/sqrt(length(DP))

#Chap.4.5. Autoregressive Models
#The series x_t is an autoreggressive process of prder p - abbreviated AR(p) if 
# x_t = alpha_1 * x_{t-1} + alpha_2 * x_{t-2} + ... + alpha_p * x_{t-p} + w_t 
#where w_t is white noise. 
#This equation can be exproessed as a polynomial of order p in terms of the backward shift operator: 

#theta_p(B)x_t = (1-alpha_1*B - alpha_2*B^2 - ... - alpha_p*B^p)x_t = w_t
#It should be noted that: 
	#1. the Random Walk is the special case AR(1) with alpha_1 = 1 
	#2. The exponential smoothing model is the special case aplha_i = alpha(1-alpha)^i, for i=1,2,.. 
	#and p->Infinity 
	#3. The model is a regression of x_t on past terms from the same seris => hence 'autoregessive' 
	#4. The model parameters can be estimated by minimising the  Sum of Squared Erros (SSE). 
	#5. A prediction at time t is given by: x_t= alpha_1*x_{t-1} +  alpha_2*x{t-2} + ... +alpha_p * x{t-p} 

#Sec. 4.5.2. Stationary and non-stationary AR processes	


