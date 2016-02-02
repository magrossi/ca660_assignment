####################################
# CA660 - Data analysis assignment #
# Marcelo Grossi and Xuan Yang     #
####################################

require("pxR")
require("scales")
require("forecast")
require("tseries")

# Read the data from pre-processed data files
cna10 <- readRDS("../db/cna10.rds")
cna13 <- readRDS("../db/cna13.rds")
cna33_h <- readRDS("../db/cna33_h.rds")
cna33_r <- readRDS("../db/cna33_r.rds")
hsa01 <- readRDS("../db/hsa01.rds")
hsa06_n <- readRDS("../db/hsa06_n.rds")
hsa06_s <- readRDS("../db/hsa06_s.rds")
hsa08_va <- readRDS("../db/hsa08_va.rds")
hsa08_na <- readRDS("../db/hsa08_na.rds")
gdp <- readRDS("../db/gdp.eur.rds")

# Plot predictor data (the rest) and to be predicted data (hsa06_n)
x.pre <- ts.intersect(cna10, cna13, cna33_h, cna33_r, hsa01, hsa08_va, hsa08_va, gdp)
plot(x.pre, main = "Intersection of Available Data Sets")

# Pick new regressor data and scales
cna13.scaled <- rescale(cna13)
hsa01.scaled <- rescale(hsa01)
hsa08.scaled <- rescale(hsa08_va)
gdp.scaled <- rescale(gdp)

# Scales the target data as well
y.pos <- rescale(hsa06_n)

# Build matrix of regressors
x.pos <- ts.intersect(cna13.scaled, hsa01.scaled, hsa08.scaled, gdp.scaled)
plot(x.pos, main = "Predictor Regressors")

# Sample and Testing time interval
sample.start = 1975
sample.end = 2005
test.start = sample.end + 1
test.end = 2011

# Prepare the in sample and out of sample data
x.sample <- window(x.pos, start = sample.start, end = sample.end)
y.sample <- window(y.pos, start = sample.start, end = sample.end)
x.test <- window(x.pos, start = test.start, end = test.end)
y.test <- window(y.pos, start = test.start, end = test.end)

# Calculate the (p,d,q) coefficients of the ARIMA(p,d,q) model
arima.coeff <- auto.arima(y.sample, xreg=x.sample)
# arima.coeff <- auto.arima(y.sample, xreg=x.sample, test="pp", approximation=FALSE)

# Validate fit by ARIMA errors
tsdisplay(arima.errors(fit), main="ARIMA errors")

# Fit model to in sample data
y.fit <- Arima(y.sample, xreg=x.sample, order = c(3,0,0)) # 3,0,0
# y.fit <- Arima(y.sample, xreg=x.sample, order = c(3,0,0)) # 0,1,0

# Plot the fitted model comparing to real data
plot(fitted(y.fit), ylab="", type="o", lwd=1.5, lty=2, main = "Fitted model vs Original data") # Fitted model
lines(y.sample, col=4, type="o", ylab="") # original data

# add a legend 
# legend(c("Fitted Model", "Original Data"))

# Predict out of sample data
y.forecast <- forecast(y.fit, h = nrow(x.test), xreg=x.test)

# Plot the prediction and compare it with the real values
par(yaxt="n")
plot(window(y.scaled, end = test.end), ylab="", type="o", main="Forecasts for house prices (2006 - 2011)")
lines(y.forecast$mean,col=4,ylab="",lty=2,lwd=2,type="o")

# Test the residuals
Box.test(y.forecast$residuals, lag=10, type="Lj")
#Box-Ljung test
#data:  y.forecast$residuals
#X-squared = 7.6182, df = 10, p-value = 0.6661
tsdisplay(residuals(y.forecast), main="Forecast Residuals")
