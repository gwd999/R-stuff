#######################################################
# Setup the Package
#######################################################

# Install RAdamant Package from source files:
# Download source files (RAdamant_0.8.3.2) from http://radamant.ning.com/page/download-radamant
install.packages("...Path-To-File/RAdamant_0.8.3.2.tar.gz", type = "source", repos = NULL)
# Alternatively, Windows Users can download the binaries

# Load RAdamant Package
library(RAdamant)

#######################################################
# Package Modularity
#######################################################

func.line.cnt("RAdamant")

#######################################################
# Graphical Features
#######################################################

# Load Theme definitions from file
loadThemes()
# Retrieve one of the current available themes
getTheme(1)
# Return all theme entries (of the current theme in use) matching the string "srt"
getThemeAttr("srt")
# Modify the value for any theme parameter
setThemeAttr(xlab.srt = 0)
# Set the specified theme (by name or Id) as current
setCurrentTheme("finance")

# MAC USERS: plot.width and plot.height must be specified in inches
setThemeAttr(plot.width = 8, plot.height = 6)
# WINDOWS USERS: plot.width and plot.height can be specified in pixels
setThemeAttr(plot.width = 1000, plot.height = 500)
# Generate four random time series 
X = matrix(cumsum(rnorm(1000)), ncol = 4)
colnames(X) = c("A", "B", "C", "D");
# Set labels for the x-axis
Xlab = paste("t[", 0:249, "]", sep = "");

# Plot series and add shaded area to the second
cplot(X
    , main = "Four Random Time Series"
    , xlabels = parse(text = Xlab)
    , shaded = c(FALSE, TRUE)
	, xtitle = "Time"
    # Theme overrides
    , shade.angle = 45
	, lwd = 2
	, xlab.cex = 0.9
    )

# Plot
cplot(X[, 1]
    , main = "Gradient Shaded Area Plot"
    , xlabels = parse(text = Xlab)
    , shaded = TRUE
	, xtitle = "Time"
    # Use different Theme
    , theme.params = getTheme("vanilla")
    #### Theme overrides ####
    # filling density of the shaded area
    , shade.density = 100 
    # Alpha transparency will be interpolated from 0 to 1 (Not Run, VERY SLOW)
    #, shade.alpha = c(0, 1) 
    # Multiple colors for the shaded area
    , shade.col = jet.colors(30) 
    # Multiple stripes are used to generate color gradient
    , shade.stripes = 200 
    # Remove rotation for x-axis
    , xlab.srt = 0 
	, # Thicker line
	, lwd = 2
	, # bigger font
	, xlab.cex = 0.9
    )


# Define 3D sinc function
x = seq(-10, 10, len = 100)
y = x
z = outer(x, y
			, function(x, y) {
				a = sqrt(x^2 + y^2)
				res = sin(a)/a
				res[a == 0] = 1
				res
			}
		)
		
par(mfrow=c(1, 2))
cplot3d(x, y, z
		, fill = "colormap"
		, main = "Colormap Filling"
		, xtitle = "x"
		, ytitle = "y"
		, ztitle = expression(z == frac(sin(x^2 + y^2),(x^2 + y^2)))
		, theta = 20
		, ztitle3d.offset = 0.22
		)
cplot3d(x, y, z, fill = "gradient", main = "Gradient Filling"
		, xtitle = "x"
		, ytitle = "y"
		, ztitle = expression(z == frac(sin(x^2 + y^2),(x^2 + y^2)))
		, theta = 20
		)


#######################################################
# Logging
#######################################################

# Retrieve the log path and filename
getLogFile();

# Set log path and filename
#setLogFile("Path-To-File");

# Enable/Disable logging to console (TRUE/FALSE)
setConsoleLogging(TRUE);
# Minimal level of function debugging (level = 0 disables the logging)
setDebugLevel(1);
# Increase trace level, including calls in the stack up to the third level down.
setDebugTraceLevel(3);

# Generate some random data
X1 = rnorm(100);
X2 = cumsum(X1);
# Run Multi-Correlation Function
res = mcf(cbind(X1, X2));

# Restore the trace level to default.
setDebugTraceLevel(1);
# Increase level of function debugging (info about loops)
setDebugLevel(2);
# Run Multi-Correlation Function
res = mcf(cbind(X1, X2));

# Restore default
setDebugLevel(1);


######################
# Frequency Analysis
######################

# Sampling period
Ts = 0.01
# Generate 2 seconds timeline
t = seq(0, 5, by = Ts)
# Sampling frequency
Fs = 1/Ts
# Simple Cosine - Constant frequency and phase = pi/4
f0 = 10;
Cos = as.matrix(cos(2*pi*f0*t + pi/4)); 
colnames(Cos) = "Simple Cosine"
rownames(Cos) = paste(t, "s", sep = "")
# Chirp signal - Cosine of increasing frequency
f = 2*t;
chirp = as.matrix(2*cos(2*pi*f*t))
colnames(chirp) = "Chirp"
rownames(chirp) = paste(t, "s", sep = "")
# Plot the two signals
cplot(cbind(Cos, chirp)[1:201, ]
	, main = "Stationarity Vs Non Stationarity"
	, xtitle = "Time"
	, xlabels = paste(t, "s", sep = "")
	, lwd = 2
	, xlab.srt = 0
	, legend.pos = "bottomleft"
	)
shade.plot(to = Cos[1:201, ]
			, from = rep(0, 201)
			, shade.angle = -30
			)
shade.plot(to = chirp[1:201, ]
			, from = rep(0, 201)
			, shade.col = getThemeAttr("col", exact = TRUE)[2]
			, shade.angle = 30
			)

# Plot the FFT
FFT(cbind(Cos, chirp)
	, Fs = 100
	, theme = getTheme(2)
	, x.ticks = 11
	, xlab.offset = 0.09
	, xtitle.offset = 0.15
	, ylab.offset = 0.02
	)

	
startDt = as.Date("2001-01-01");
endDt = as.Date("2011-03-31");
# Autobytel
ABTL = get.fs("ABTL", from = startDt, to = endDt)
plot(ABTL, ylab.offset = 0.02, overrides2 = list(ylab.offset = 0.02, xlab.offset = 0.08))

# Plot Returns
R_ABTL = Ret(ABTL
			, plot = TRUE
			, style = "bar"
			, main = "One Day Returns"
			, legend = "Autobytel"
			, theme.params = getTheme(2)
			, xlabels = rownames(ABTL)
			)
colnames(R_ABTL) = "Autobytel"

# Frequency Analysis - Zoom Half spectrum (right side) and use Bartlet-Hann windowing
FFT(R_ABTL[-1, , drop = FALSE]
    , window = barthann
    , zoom = 30
	, half = TRUE
	, show.periodicity = TRUE
	, x.ticks = 10
	, xlab.offset = 0.08
	, ylab.offset = 0.02
	, xtitle.offset = 0.2
	, ytitle.offset = 0.04
	)

# Spectrogram Stationary vs Non Stationary signals
spec1 = specgram(Cos
				, win.size = 64
				, Fs = 100
				, show.periodicity = TRUE
				, ylab.offset = 0.02
				, ytitle.offset = 0.06
				)

spec2 = specgram(chirp
				, win.size = 64
				, Fs = 100
				, show.periodicity = TRUE
				, ylab.offset = 0.02
				, ytitle.offset = 0.06
				)


# 3D Spectrogram of Cosine
spec1_3d = specgram(Cos
				, win.size = 128
				, show.periodicity = TRUE
				, ylab.offset = 0.02
				, ytitle.offset = 0.06
				, plot3d = TRUE
				, xtitle3d.offset = 0.5
				, ytitle3d.offset = 0.35
				)

# 3D Spectrogram of chirp
spec2_3d = specgram(chirp
				, win.size = 128
				, show.periodicity = TRUE
				, ylab.offset = 0.02
				, ytitle.offset = 0.06
				, plot3d = TRUE
				, xtitle3d.offset = 0.5
				, ytitle3d.offset = 0.35
				)

# Spectrogram of Financial Series return
spec3 = specgram(R_ABTL[-1, , drop = FALSE]
				, win.size = 128
				, show.periodicity = TRUE
				, ylab.offset = 0.02
				, ytitle.offset = 0.06
				, plot3d = TRUE
				, xtitle3d.offset = 0.5
				, ytitle3d.offset = 0.35
				)


######################
# Smoothers
######################
# Get financial series from Yahoo Finance
AMZN = get.fs("AMZN", from = startDt, to = endDt)
# extract only "Close" price for a smaller subsample 
Series = AMZN[2000:2500 ,"Close", drop=FALSE]
colnames(Series) = "Amazon_close"

sma(Series, win.size = 30, na.rm=TRUE, plot=TRUE, col=c("white", "green"), lwd=c(1,2))

vec.lag = c(10, 30, 50, 100)
sma(Series, win.size = vec.lag, na.rm=FALSE, plot=TRUE
	, col=c("white", "red", "yellow", "green", "cyan"))

ema(Series, win.size = 30, na.rm=FALSE, plot=TRUE, col=c("white", "yellow"), lwd=c(1,2))

ema(Series, win.size = vec.lag, na.rm=FALSE, plot=TRUE
	, col=c("white", "red", "yellow", "green", "cyan"))

wma(Series, win.size = 30, na.rm=FALSE, plot=TRUE, rm.transient=TRUE, lwd=c(1,2))

wma(Series, win.size = vec.lag, na.rm=FALSE, plot=TRUE
	, col=c("white", "red", "yellow", "green", "cyan"))

kama(Series, fast.win=2, slow.win=30, lag=30, plot=TRUE)

frama(Series, win.size=30, tau=4.6, col=c("white", "green"), lwd=c(1,2), plot=TRUE)

gmma(Series, plot=TRUE)

# Plot Multiple Moving Averages together using "Movav" plot method
m1 = sma(Series, win.size = 30, na.rm=TRUE)
m2 = ema(Series, win.size = 30, na.rm=TRUE)
m3 = wma(Series, win.size = 30, na.rm=TRUE)
m4 = kama(Series, fast.win=2, slow.win=30, lag=30)

MOV = cbind(m1, m2, m3, m4)
dim(MOV)

plot.Movav(x = MOV, fs = Series, main = "Mov.Av. comparison")

m1 = sma(AMZN, win.size = 100, na.rm=TRUE)
m2 = ema(AMZN, win.size = 100, na.rm=TRUE)
m3 = wma(AMZN, win.size = 100, na.rm=TRUE)
m4 = kama(AMZN, fast.win=2, slow.win=30, lag=100)

MOV = cbind(Amazon= AMZN[ ,"Close"], m1, m2, m3, m4)

# change theme
setCurrentTheme(2)

# plot only smoothers
plot.Movav(x = MOV[,-1], fs = NULL, main = "Mov.Av. comparison")

# plot smoothers and original series (close)
plot.Movav(x = MOV, fs = NULL, main = "Mov.Av. comparison")

# plot smoothers and original series plus volume on the bottom 
plot.Movav(x = MOV[,-1], fs = AMZN, cex.main=0.6, lwd=0.5, main="Mov.Av. comparison")
				
######################
# Indicators
######################
# Get financial series from Yahoo Finance
FS = get.fs("AMZN", from = as.Date("2006-01-01"), to = as.Date("2010-03-31"))

# set graphical theme
setCurrentTheme(2)

# Macd
macd(FS, main="MACD")

# Absolute Price Oscillator
apo(FS, plot=TRUE)

# Klinger Oscillator
kvo(FS, main="Klinger Oscillator")

# Relative Strenght Index
rsi(FS, lag=30, plot=TRUE)

# Parabolic Stop-and-Reverse
prbsar(FS, plot=TRUE, main="Parabolic Stop-Reverse")

# Chande Momentum Oscillator
cmof(FS, lag = 10, plot=TRUE)

# Bollinger Bands
BolBand(FS, win.size = 30, plot=TRUE, main="Bollinger Band")

# Bollinger bands - %B
BolBandB(FS, win.size = 30, plot=TRUE, main="Bollinger Band %B")

# Aroon oscillator
aroon(FS, lag = 150, main="Aroon oscillator - lag:150")
			
######################
# Correlation Analysis
######################

# Mutual Funds

# PIMCO Total Return Instl, Morning Star: 5
# Cash        Stock        Bonds        Other
# 58.77        -           27.00        10.89
PTTRX = get.fs("PTTRX", from = startDt, to = endDt)

# Vanguard Wellington Inv, Morning Star: 5
# Cash        Stock        Bonds        Other
# 6.57        65.55        27.74        -
VWELX = get.fs("VWELX", from = startDt, to = endDt)

# American Funds Bond Fund of Amer A, Morning Star 2
# Cash        Stock        Bonds        Other
# 6.86        0.02         92.76        0.07
ABNDX = get.fs("ABNDX", from = startDt, to = endDt)

# American Funds AMCAP A
# Cash        Stock        Bonds        Other
# 9.59        90.41        -            -
AMCPX = get.fs("AMCPX", from = startDt, to = endDt)

# Compute the returns for one of the utual funds
R_VWELX = Ret(VWELX, na.rm = TRUE)
colnames(R_VWELX) = "Vanguard Wellington 1-Day Returns"

# AutoCorrelation and Partial Autocorrelation function of the returns
mcf(R_VWELX
	, xlab.offset = 0.1
	, ylab.offset = 0.01
	, ytitle.offset = 0.05
	, ytitle.srt = 0
	, xlab.cex = 0.7
	, ylab.cex = 0.7
	, one.side.margin = c(3, 4, 2, 2)
	)


# Combine all funds
funds = combine(VWELX, ABNDX, AMCPX, which = "Adj.Close")


# AutoCorrelation and Partial Autocorrelation function of all the price series on one shot
mcf(funds
	, lag.max = 100
	, xlab.offset = 0.1
	, ylab.offset = 0.01
	, ytitle.offset = 0.05
	, ytitle.srt = 0
	, xlab.cex = 0.7
	, ylab.cex = 0.7
	, one.side.margin = c(3, 4, 2, 2)
	)

# Compute 10-day returns
r_funds = Ret(funds, lag = 10, na.rm = TRUE)
rownames(r_funds) = rownames(funds)[-c(1:10)]

# Auto Correlation Analysis
mcf(r_funds
	, xlab.offset = 0.08
	, ylab.offset = 0.02
	, ytitle.offset = 0.06
	, ytitle.srt = 0
	)


# Cross Correlation Analysis (The first series against all the others)
cross.ccf(Y = r_funds[, 1, drop = FALSE], X = r_funds[, -1])
# Multi Correlation Analysis
mcplot(r_funds, new.device = TRUE, nclass = 60, coLin = FALSE, legend.cex = 0.8)


######################
# GARCH models
######################

setCurrentTheme(1)

# Load a series
NASDAQ = get.fs("^IXIC", SName = "NASDAQ", from = as.Date("2007-01-01"), to = as.Date("2009-07-31"));

# Calculate log returns
X = Ret(NASDAQ, lag = 5, log=TRUE, na.rm=TRUE)

# plot returns
plot(X, style="bar")

chist(X, lwd=2)
JB.test(X, plot=TRUE)

# plot squared returns (volatility)
plot(X^2, style="bar", lwd=2, main="Plot of Squared.LogRet.NASDAQ_5")

# Estimate GARCH(1,1) - Garch
g1 = Garch(X, order=c(alpha=1, beta=1), type="garch", prob="norm")
g1

g2 = Garch(X, order=c(alpha=1, beta=1), type="garch", prob="t")
g2

g3 = Garch(X, order=c(alpha=1, beta=1), type="garch", prob="ged")
g3

# Estimate EGARCH(1,1)
eg = Garch(X, order=c(alpha=1, beta=1), type="egarch", prob="t", n.init=NULL)

# Estimate TGARCH(1,1) 
tg = Garch(X, order=c(alpha=1, beta=1), type="tgarch", prob="g", n.init=NULL)

# NIC - GARCH
newsimp(g1, shaded=TRUE)
# NIC - TGARCH
newsimp(tg, shaded=TRUE)
# NIC - EGARCH
newsimp(eg, shaded=TRUE)

########################
# Performance Evaluation
########################

# Get financial series from Yahoo Finance
startDt = as.Date("2006-01-01");
endDt = as.Date("2010-03-31");
ABTL = get.fs("ABTL", from = startDt, to = endDt)
BIIB = get.fs("BIIB", from = startDt, to = endDt)
SONY = get.fs("SNE", from = startDt, to = endDt)
NTWK = get.fs("NTWK", from = startDt, to = endDt)
AMZN = get.fs("AMZN", from = startDt, to = endDt)
AVNR = get.fs("AVNR", from = startDt, to = endDt)

# Combine series and create a portfolio
PTF = combine(ABTL, BIIB, SONY, NTWK, AMZN, AVNR)

# Load a Benchmark Portfolio Index
NASDAQ = get.fs("^IXIC", SName = "NASDAQ", from = startDt, to = endDt);

# Calculate returns
r_nas = Ret(NASDAQ, lag = 1, log=FALSE, na.rm=TRUE)
r_ptf = Ret(PTF, lag = 1, log=FALSE, na.rm=TRUE)

# Capital Asset Price Model
capm = Capm(PTF = r_ptf, PTF_M = r_nas, rfr = 0.001 )
capm

# Performance Measurement for class "Capm"
Sharpe(capm)
Treynor(capm)
Jensen(capm)
Appraisal(capm)
FourMeasures(capm)

######################
# Performance Analysis
######################

# Efficient frontier using constrained optimisation
PtfFront(funds
		# Compute 100 points on the frontier
		, n_sim = 100
		# Compute the portfolio weights assuming 10-day holding period
		, lag = 10
		# Tolerance to the constraint on the weights
		, wTol = 1e-5
		, theme = getTheme(1)
		, main = "Constrained Frontier Simulation: 100 points"
		, ylab.offset = 0.02, ytitle.offset = 0.02, legend.cex = 0.8)

# Efficient frontier using unconstrained optimisation
PtfFront(funds
		# Compute 100 points on the frontier
		, n_sim = 100
		# Compute the portfolio weights assuming 10-day holding period
		, lag = 10
		# Unconstrained optimisation - Allows for short selling
		, constrained = FALSE
		, main = "Unconstrained Frontier Simulation: 100 points"
		, theme = getTheme(2)
		, ylab.offset = 0.02, ytitle.offset = 0.02, legend.cex = 0.8)

# Show the mean returns of the assets
as.matrix(colMeans(r_funds))

# Create portfolio with target return mi >= 0.2%
opt = PtfOpt(ptf = funds, mi = 0.002, lag = 10)
opt

# Use different constraints and multiple targets
# NA are produced as return = 0.003 is not achievable with thee assets: a note is generated
opt2 = PtfOpt(ptf = funds
			# Providing 2 target returns, 2 portfolios will be computed
			, mi = c(0.002, 0.003)
			# Set the minimum weight for each asset
			, wmin = c(0.1, 0.2, 0.2)
			# Set the maximum weight for each asset
			, wmax = c(0.7, 0.6, 0.4)
			, lag = 10
			)
opt2

# Create the portfolio (rebalancing at each step)
ptf = PtfValue(funds, weights = opt$weights)
rownames(ptf) = rownames(funds);
colnames(ptf) = "Portfolio"

# Create the second portfolio
ptf2 = PtfValue(funds, weights = opt2$weights[, 1])
rownames(ptf2) = rownames(funds);
colnames(ptf2) = "Portfolio 2"



# Plot the value
cplot(cbind(ptf, ptf2)
		, xlabels = rownames(ptf)
		, main = "Two Custom Portfolios"
		)


# Plot the holding percentages for the first portfolio
cplot(round(100*attr(ptf, "holdings"), 2)
		, xlabels = rownames(ptf)
		, main = "Portfolio Composition"
		, ylab.suffix = "%"
		, new.dev = TRUE
		)



# Plot the holding percentages for the second portfolio
cplot(round(100*attr(ptf2, "holdings"), 2)
		, xlabels = rownames(ptf2)
		, main = "Portfolio 2 Composition"
		, ylab.suffix = "%"
		, new.dev = TRUE
		)
		
		
# Plot the returns
r_ptf = Ret(ptf
			, lag = 10
			, na.rm = TRUE
			, plot = TRUE
			, style = "bar"
			, xlabels = rownames(ptf)[-c(1:10)]
			, new.dev = TRUE
			)
rownames(r_ptf) = rownames(ptf)[-c(1:10)]


# Plot Historical Returns on Investment of the portfolio
plot(hroi(ptf
		# Analyse the performance for multiple holding periods
		, lag = round(seq(1, 300, len = 100))
		# Compute simple returns (default would calculate log-returns)
		, log = FALSE
		# Only the selected lags will be processed
		, mode = "selected"
		# Use GPD distribution for VaR Calculation (Asymmetrical)
		, VaR.type = "GPD"
		)
	)

# Plot Historical Returns on Investment of the portfolio
plot(hroi(ptf
		# Analyse the performance for multiple holding periods
		, lag = round(seq(1, 300, len = 100))
		# Compute simple returns (default would calculate log-returns)
		, log = FALSE
		# Only the selected lags will be processed
		, mode = "selected"
		# Use Normal distribution for VaR Calculation (Asymmetrical)
		, VaR.type = "Normal"
		)
	)




######################
# VaR Analysis
######################

# Confidence levels
ci = seq(0.01, 0.05, 0.01);

# Historical VaR and ES
hVaR(r_ptf, p = ci)
hES(r_ptf, p = ci)

# Weighted Historical VaR and ES
whVaR(r_ptf, p = ci)
whES(r_ptf, p = ci)

# Distribution Based - Normal
VaR(r_ptf, probf = "Normal", p = ci)
ES(r_ptf, probf = "Normal", p = ci)

# Distribution Based - T Student
VaR(r_ptf, probf = "T", p = ci)
ES(r_ptf, probf = "T", p = ci)

# Distribution Based - Generalised Pareto Distribution
VaR(r_ptf, probf = "GPD", p = ci)
ES(r_ptf, probf = "GPD", p = ci)

# Dynamic VaR and ES (200-day sliding window)
dynVaR = movApply(r_ptf, win.size = 200, func = VaR)
dynES = movApply(r_ptf, win.size = 200, func = ES)

# Plot VaR and ES
cplot(cbind(r_ptf, dynVaR, dynES)
	, main = "Dynamic 10-Day VaR and Expected Shortfall"
	, xlabels = rownames(r_ptf)
	, legend = c("Portfolio Return", "Normal VaR 5%", "Normal ES 5%")
	, new.dev = TRUE
	)


######################
# GPD-EVT Analysis
######################
N = NROW(r_ptf);

# Set Treshold for EVT Var
trsh = -100*hVaR(r_ptf, p = 0.05)[1];

# Extract Portfolio Losses (percentages)
ptf_loss  = -100 * r_ptf [r_ptf  < 0, , drop = FALSE];
# Extract tail
loss_tail = ptf_loss[ptf_loss > trsh];

# Estimate EVT Distribution parameters with Joint Confidence Interval <= 1%
gpd.ci(Xtail = loss_tail, alpha = 0.01, df = 2, trsh = trsh);
VaRmaxLL = gpd.VaR.ci(Xtail = loss_tail, alpha = 0.01, df = 2, trsh = trsh, N = N);
VaRmaxLL
ESmaxLL = gpd.ES.ci(Xtail = loss_tail, alpha = 0.01, df = 2, trsh = trsh, N = N);
ESmaxLL
# Bootstrap Parameters (VaR, xi)
VaRboot = boot(loss_tail, nboots = 1000, func = gpd.VaR.ml, trsh = trsh, N = N);
# Bootstrap Parameters (ES, xi)
ESboot = boot(loss_tail, nboots = 1000, func = gpd.ES.ml, trsh = trsh, N = N);

# Compute Log-Likelihood surface as a function of VaR and xi
surfVaR = gpd.VaR.surface(Xtail = loss_tail, N = N, trsh = trsh);
# Compute Log-Likelihood surface as a function of ES and xi
surfES = gpd.ES.surface(Xtail = loss_tail, N = N, trsh = trsh);

# Compute Confidence Interval Contour using Profile Likelihood (VaR, xi)
VaRcontour = gpd.VaR.contour(Xtail = loss_tail, trsh = trsh, N = N, grid.size = 500);
# Compute Confidence Interval Contour using Profile Likelihood (ES, xi)
EScontour = gpd.ES.contour(Xtail = loss_tail, trsh = trsh, N = N, grid.size = 500);

boot.plot = function(Rboot
					, Xtail
					, ctour
					, maxLL
					, func = NULL
					, x, y, z
					, xlim, ylim, zlim, pmat
					, col.lev = col.lev
					, ...
				) {

	logLike = func(Rboot, Xtail = Xtail, ...);
	# Get colormap
	cmap = getThemeAttr("colmap", exact = TRUE);
	# Compute color ffor the bootstrapped points
	pointcols = cmap[cut(logLike, col.lev, include.lowest = TRUE)];
	# Plot the points
	points3d(Rboot[, 1], Rboot[, 2], zlim[1], pmat = pmat, col = pointcols, pch=16, cex=0.5);

	# Compute color ffor the bootstrapped points
	pointcols = cmap[cut(logLike, col.lev, include.lowest = TRUE)];
	# Contour lines
	ctour.Len = NROW(ctour);
	ctour.cols = cmap[cut(func(parms = ctour[1, 1:2], Xtail = Xtail, ...), col.lev, include.lowest = TRUE)];
	# Plot contour lines
	lines3d(ctour[c(1:ctour.Len, ctour.Len:1), 1]
			, c(ctour[, 2], ctour[ctour.Len:1, 3])
			, zlim[1]
			, pmat = pmat
			, col = ctour.cols
			, lwd = 2)
	
	max.cols = cmap[1];
	lines3d(maxLL[1, 2:3], rep(maxLL[2, 1], 2), zlim[1], col = max.cols, lwd = 1, lty = 1)
	lines3d(rep(maxLL[1, 1], 2),maxLL[2, 2:3], zlim[1], col = max.cols, lwd = 1, lty = 1)

}

# Plot VaR
cplot3d(surfVaR$VaR
		, surfVaR$xi
		, surfVaR$LogLike
		, pre = boot.plot
		, func = gpd.VaR.like
		, maxLL = VaRmaxLL[[1]]
		, Xtail = loss_tail
		, N = N
		, trsh = trsh
		, Rboot = VaRboot
		, ctour = VaRcontour[[1]]
		, fill = "colormap"
		, x.ticks = 4
		, xtitle = "VaR"
		, xtitle3d.srt = 0
		, ytitle3d.srt = 0
		, ytitle = expression(xi)
		, xlab.suffix = "%"
		, ztitle = "Log-Likelihood"
		, main = "VaR: Log-Likelihood Surface and Bootstrap"
		)

cplot3d(surfES$ES
		, surfES$xi
		, surfES$LogLike
		, pre = boot.plot
		, func = gpd.ES.like
		, maxLL = ESmaxLL[[1]]
		, Xtail = loss_tail
		, N = N
		, trsh = trsh
		, Rboot = ESboot
		, ctour = EScontour[[1]]
		, fill = "colormap"
		, x.ticks = 4
		, xtitle = "ES"
		, xtitle3d.srt = 0
		, ytitle3d.srt = 0
		, ytitle = expression(xi)
		, xlab.suffix = "%"
		, ztitle = "Log-Likelihood"
		, main = "ES: Log-Likelihood Surface and Bootstrap"
		, theme.params = getTheme(1)
		)		


###############################
# Macro Economic Stress Testing
###############################

# Load Macro Economic series from file
dataPath = "Path-To-Folder-Containing-Macro-Data"
load(paste(dataPath, "macroSeries.RData", sep = "/"))


# Aggregate portfolio returns to monthly average
dates = as.Date(rownames(r_ptf));
group = paste(format(dates, "%YQ"), ceiling(as.numeric(format(dates, "%m"))/3), sep = "")
r_ptf_q = as.matrix(aggregate(as.matrix(r_ptf[,]), by = list(group), FUN = mean)[-41,-1, drop = FALSE])
colnames(r_ptf_q) = "Ptf";
rownames(r_ptf_q) = time.labels;

# Collect macro variables
macroData = cbind(r_irb_q[, c(1, 6)] # UK and Germany
					, r_debt2gdp_q[, 8:9, drop = FALSE] # Italy, Neederland
					, r_reer_q[, c(13, 14, 16)] # US, China, Japan 
					, r_gdp_q[, c(1, 12, 13)] # EU, US, UK
					, ir_q
					)

# Visual Analysis
cross.plot(Y = r_ptf_q, X = macroData
			, xlabels = time.labels
			, xlab.offset = 0.09
			, xtitle.offset = 0.18
			, ytitle.srt = 0
			, ytitle2.offset = 0.12
			)

# Univariate Analysis
univar(r_ptf_q, macroData, xlab.offset = 0.09, xtitle.offset = 0.18, ytitle.srt = 0)

# Regression
model = mreg(Y = r_ptf_q, X = macroData
			, type = "stepwise"
			, max.vars = 3
			, xlabels = time.labels
			, legend.cex = 0.9
			, ylab.cex = 0.9
			, xlab.cex = 0.9
			)
summary(model)

# Extract model terms used in the regression
model.terms = get.predictors(model[[1]]$lm)

# Sensitivity Analysis
sensAnalysis(model
			, mode = "EW"
			, win.size = 10
			, by = 5
			, plot = TRUE
			, xlabels = time.labels
			, x.ticks = 10
			, ylab.offset = 0.005
			, ytitle.offset = 0.04
			, legend.cex = 1
			, xlab.cex = 0.95
			, xlab.offset = 0.05
			)

# VAR model
macroModel = VecAr(macroData[, model.terms]
					, ar.order = 1:2
					, regtype = "stepwise"
					, max.var = 3
					, type = "none"
					, xlabels = time.labels
					)
summary(macroModel)

# VAR Aggregated Simulation
predict(macroModel
		, steps = 8
		, simulate = TRUE
		, scenarios = 200
		, sd.sim = 3
		, aggregate = TRUE
		, plot = TRUE
		, xlabels = c(time.labels, paste(rep(2011:2012, each=4), 1:4, sep = "Q"))
		, shaded = TRUE
		, shade.angle = 30
		, shade.density = 60
		, x.ticks = 10
		)

# VAR Stress scenario generation
modScenarios = predict(macroModel
						, steps = 8
						, simulate = TRUE
						, scenarios = 1000
						, sd.sim = 3
						, aggregate = FALSE
						, plot = FALSE
						)


# Portfolio prediction based on generated scenarios
predict(model
		, newdata = modScenarios
		, aggregate = TRUE
		, plot = TRUE
		, shaded = TRUE
		, xlabels = c(time.labels, paste(rep(2011:2012, each=4), 1:4, sep = "Q"))
		)				
						