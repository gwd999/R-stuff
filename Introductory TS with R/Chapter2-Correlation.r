#######CORRELATION 
#Chap.2.1 Expectatio and the ensamble 
#Sec.2.1.1. Expected Value - mean()
#E(x) is the mean of x1
#Variance<-E[(x-E(x))^2] and the Covriance<-E[(x-E8x))*(y-E(y))]; "sample Cov"<-Sum[(x-E(x))*(y-E(y))]/(n-1)
www<-"http://www.massey.ac.nz/~pscowper/ts/Herald.dat"
Herald.dat<-read.table(www, header=T) 
attach(Herald.dat) 
#Calculating cov() for the Herald.dat pairs ('CO' and 'Benzoa') in three different ways
x<-CO; y<-Benzoa; n<-length(x) 
sum((x-mean(x))*(y-mean(y)))/(n-1) 
mean((x-mean(x))*(y-mean(y))) 
cov(x,y)
#Calculating Correlation - cor() and std()
cov(x,y)/(sd(x)*sd(y))
cor(x,y)
#Sec.2.2.2. The ensamlbe and STATIONARITY 
#The mean function of a ts model is: mu(t)=E[x(t)]
#If the mean function is constant we say its STATIONARY=>the sample estimate of the population mean is the sample mean
#in practice thiscan often be achieved by removing trends with the decompose() FUNCTION 
#Stationarity implies that the time averaging starting point is irrelevant

#Sec2.2.3. VARIANCE - var()
#Under the assumption of a stationary model the 'constant' population variance can be estimated from the sample variance 
# In a ts analysis sequential observations may be correlated. If the Correlation is postive Var(x) will underestimate
#the population Variance in a short series. This bias decreases rapidly as the ts length increases. 

#Sec.2.2.4. Autocorrelation 
#Consider a ts model that is stationary in the mean and variance. The variables may be correlated and the model 
#is 'second-order-stationary' if the correlation between variables depends ONLY on the time steps seperating them. 
#This umber of time steps seperatig them (correlated variables) is known as the "lag". 
#Correlation of a variable with itself at different times is known as 'autocorrelation' or 'serial correlation'. 
#If a ts is second-order-stationary, define the autocovariance function (acvf) as a function of the "lag" k. 
#acvf=E[(x(t)-mu)*(x(t+k)-mu)]
#Autocorrelation: rho=acvf/var(x)

www<-"http://www.massey.ac.nz/~pscowper/ts/wave.dat"
wave.dat<-read.table(www, header=T); attach(wave.dat) 
layout(1:2)
plot(ts(waveht)); plot(ts(waveht[1:60]))#Fig.2.3.
#The Autocorrelations of x are stored in the vector acf(x)$acf 
#with the "lag" k autocorrelation located IN acf(x)$acf[1+k] 
#e.g. lag 1 autocorrelation for for waveht is
acf(waveht)$acf[2] #you can display all lags by acf(waveht)$acf[] 
#The lag 1 Autocovariance is given by 
acf(waveht, type=c("covariance"))$acf[2] 

#Chap.2.3. The Correlogram 
#acf() FUNCTION produces the correlogram plot by default.   
#Correlogram for Air Passanger data 
data(AirPassengers) 
AP<-AirPassengers 
AP.decom<-decompose(AP, "mult")
plot(ts(AP.decom$random[7:138])) #Fig.2.7. 
acf(AP.decom$random[7:138]) #Fig.2.8. Correlogram for random component of AirPassenger data period 1949-1960
#Fig.2.8. correlogram suggests a damped 'cosine' shape, 
#which is characteristic of an autoregressive model of order  2 (see Chap.4.), or that the seasonal adjustment has 
#not been entirely effective. 
#HOW TO CHECK FOR EFFECTIVENESS OF SEASONAL ADJUSTMENTS
#1. Look at the standard deviation of the original series 
sd(AP[7:138]) #=119
#2. Look at the standard deviation of the series after subtracting the trend 
sd(AP[7:138]-AP.decom$trend[7:138]) # only 41 
#3. Look at the standard deviation of the series after seasonal adjustment  
sd(AP.decom$random[7:138]) #only a very small 0.0334
#=>therefore one can say that the seasonal adjustment was very EFFECTIVE 

#Example based on Font Reservoir series 
www<-"http://www.massey.ac.nz/~pscowper/ts/Fontdsdt.dat"
Fontdsdt.dat<-read.table(www, header=T)
attach(Fontdsdt.dat)
layout(1:1)
plot(ts(adflow),ylab='adflow') #Fig.2.9.
acf(adflow, xlab='adflow (months)', main="") #Fig.2.10. 
   
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

