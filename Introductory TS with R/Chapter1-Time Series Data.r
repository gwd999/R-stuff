#Sample Code chunks from Chapter 1 of Book "Introductory Time Series with R"
data(AirPassengers)
AP<-AirPassengers
class(AP)
start(AP);end(AP);frequency(AP)
plot(AP, ylab="Passengers (1000's)") #Fig. 1.1.
#For getting a clearer view of the trend the seasonal effect can be removed 
#with the 'aggregate' function: summary of the values can be viewed with 
#a 'boxplot', the seasons being extracted with the 'cycle' function 
#Finally put the plots in a single window using 'layout' 
layout(1:2)
plot(aggregate(AP))   #Increasing trend - Fig. 1.2a
boxplot(AP~cycle(AP)) #Seasonal effects - Fig. 1.2b

#UNEMPLOYMENT MAINE 
www<-"http://www.massey.ac.nz/~pscowper/ts/Maine.dat"
Maine.month<-read.table(www, header=TRUE)
attach(Maine.month)
class(Maine.month) #still a "data.frame"
Maine.month.ts<-ts(unemploy, start=c(1996,1), frequency=12)
Maine.annual.ts<-aggregate(Maine.month.ts)/12
#Plot
layout(1:2)
plot(Maine.month.ts, ylab="unemployed (%)")
plot(Maine.annual.ts, ylab="unemployed (%)")
#USING THE 'window' FUNCTION 
#Calculate precise percentages using 'window' function, which extracts part 
#of the ts between start and end points, and will sample with an intervall equal to "frequency"
Maine.Feb<-window(Maine.month.ts, start=c(1996,2), frequency =TRUE)
Maine.Aug<-window(Maine.month.ts, start=c(1996,8), frequency =TRUE)
Feb.ratio<-mean(Maine.Feb)/mean(Maine.month.ts)
Aug.ratio<-mean(Maine.Aug)/mean(Maine.month.ts)
Feb.ratio
Aug.ratio

#Multiple time series 
#Electricity, beer and chocolate
www<-"http://www.massey.ac.nz/~pscowper/ts/cbe.dat"
CBE<-read.table(www, header=TRUE)
CBE[1:4,]
class(CBE)
Elec.ts<-ts(CBE[,3],start=1958, freq=12)
Beer.ts<-ts(CBE[,2],start=1958, freq=12)
Choc.ts<-ts(CBE[,1],start=1958, freq=12)
#You can use 'plot' with 'cbind' to plot several series on one figure!
plot(cbind(Elec.ts, Beer.ts, Choc.ts)) # Fig. 1.5.
#the 'ts.intersect' function for obtaining intersections of two series that overlap in time
AP.elec<-ts.intersect(AP, Elec.ts)
start(AP.elec)
end(AP.elec)
AP.elec[1:3,]
#The data for each series is extracted and plotted 
AP<-AP.elec[,1];Elec<-AP.elec[,2]
layout(1:2)
plot(AP, main="", ylab="Air passengers / 1000's") # Fig. 1.7. (oben) 
plot(Elec, main="", ylab="Electricity porduction / MkWh") # Fig. 1.7 (unten)
plot(as.vector(AP), as.vector(Elec), xlab="Air passengers", #as.vector is needed to convert "ts" objects to ordinary vectors needed for scatterplot
          ylab="Electricity production") # Fig. 1.8. 
abline(reg=lm(Elec~AP)) #Linear model trend line
cor(AP, Elec) #Correlation coefficient => CORRELATION DOES NOT IMPLY CAUSATION!!! This is a good example for that!

#Chap. 1.4.4 Quarterly Exchange Rate: GBP to NZ Dollar
www<-"http://www.massey.ac.nz/~pscowper/ts/pounds_nz.dat"
Z<-read.table(www, header=T)
Z[1:4,]
Z.ts<-ts(Z, st=1991, fr=4)
plot(Z.ts, xlab="time / years", ylab="Quarterly exchange rate in $NZ/GBP") # Fig. 1.9

#Emphasize the two local up and down trends with 'window' function
Z.92.96<-window(Z.ts, start=c(1992,1), end=c(1996,1))
Z.96.98<-window(Z.ts, start=c(1996,1), end=c(1998,1))
layout(1:2)
plot(Z.92.96,xlab="Time (years", ylab="Quarterly exchange rate in $NZ/GBP")  # Fig. 1.10. (oben)
plot(Z.96.98,xlab="Time (years)", ylab="Quarterly exchange rate in $NZ/GBP") # Fig. 1.10. (unten) 
#Chapter 1.4.5 Global Te,perature series 
www<-"http://www.massey.ac.nz/~pscowper/ts/global.dat"
Global<-scan(www)
Global.ts<-ts(Global, st=c(1856,1), end=c(2005,12), fr=12) 
Global.annual<-aggregate(Global.ts, FUN=mean) #since we are concerned about the trend we use 'agregate'
plot(Global.ts) #Fig. 1.11. (oben)
plot(Global.annual) #Fig 1.11. (unten) 
#Next the momthly time intervalls for 36 years (from 1970-2005) are extracted using the 'time' FUNCTION
New.series<-window(Global.ts, start=c(1970,1), end=c(2005,12))
New.time<-time(New.series) 
plot(New.series); abline(reg=lm(New.series ~ New.time)) #Fig. 1.12
  
#Chap1.5. Decomposition of time series (into Trend, Seasonal effect and an Error term)
#Estimating trends - simplest way - MOVING AVERAGES "MA" - two possible models available for the trend 
# Additive or multiplicative (but multiplicative can be turned into additive by logarithmus function)
#usually the length of the MA will be chosen to average out the seasonal effects
#When Averaging out monthly series if is often necessary to center via two MA's 
#usually one MA from Jan.X0-Dec.X0 and another one from Feb.X0-Jan.X1 otherwise 
#the average would come to rest on 6.5 months and not after 6 months.
#See page 20 and 21. 
#Estimates of the monthly additive (seasonal) effect can be obtained by subtracting 
#the calculated MA - see page 21.
#Chap.1.5.4 SMOOTHING
#Above centered MA's are a smoothing technique
#Another smoothing techn. available in R is the 'stl' function which uses the "loess" regression techniques - 
#this is a line or hogher polynomial 
plot(decompose(Elec.ts)) #Fig 1.14. 
#alternative
plot(stl(Elec.ts, s.window="periodic"))
#Manually
Elec.decom<-decompose(Elec.ts, type="mult") #using "mult"iplicative model 
plot(Elec.decom)
Trend<-Elec.decom$trend
Seasonal<-Elec.decom$seasonal
#Fig.1.13 p.22 - Trend with superimposed multiplicative seasonal effects 
ts.plot(cbind(Trend, Trend*Seasonal),lty=1:2) #Fig. 1.13. 


                          