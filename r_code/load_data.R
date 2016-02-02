library("pxR")
path <- "../db/"
files <- 
  c("CIA01 Estimates of Household Income by County and Region, Year and Statistic.px",
    "CNA13 Annual Rate of Population Increase by Sex, Province or County, CensusYear and Statistic.px",
    "HSA01 House Completions by Housing Sector and Year.px",
    "HSA06 Average Price of Houses by Area, Year and Statistic.px",
    "HSA08 House Loans Approved and Paid by House Type New or Second hand, Year and Statistic.px")
# diff computes the difference between data at point i with data at point i - 1
diff(ts)

# head and tail returns the all but the first/last k (head) or all after/before first/lats (tail)
head(gdp.ar$resid, -4) # removes the last 4
tail(gdp.ar$resid, -4) # removes the first 4

# Unites ts1 and ts2 into a ts matrix
ts.union(ts1, ts2)

# Intersects into a ts matrix where there are data available for both
ts.intersect(ts1, ts2)

# Window filters time series (works on matrices too)
mw <- window(m, start=1975, end=2011)

# Plots multiple time series in one chart, can deal with missing values
ts.plot(ts1, ts2,col=c("blue", "red"))
ts.plot(mw)

# Aggregate sums (or custom FUN) the cycles into major cycle (like monthly into year, etc.)
aggregate(ts)
aggregate(ts, FUN=mean)

# Gets frequency of time series
frequency(mw)

# Cycle, gets cycle of time series
cycle(ts)

# start(ts) annd end(ts) gets start and end of time series
start(ts)
end(ts)

# auto regression line
abline(ts)

# cross correlation, return lagged cross correlation
acf(ts)
pacf(ts) # partial acf

# saveRDS and readRDS to save and retrieve objects in R (works on data series and etc.)
saveRDS(ts, filename)
var <- readRDS(filename)

# Prediction and Modelling
plot(HoltWinters(gdp, gamma = FALSE))
gdp.hw <- HoltWinters(gdp, gamma = FALSE)
gdp.predict <- predict(gdp.hw, n.ahead = 10)
ts.plot(gdp, gdp.predict)
ts.plot(gdp, gdp.predict, lty=1:2) # lty changes the line type

# Auto regression models
gdp.ar <- ar(gdp, method="mle")
gdp.ar$resid # time series with the residuals

# Fit linear model
lm(formula, ts) # dont understand much
gdp.re <- lm(gdp~time(gdp))
summary(gdp.re)

#\Call:
#  lm(formula = gdp ~ time(gdp))
#
#Residuals:
#  Min     1Q Median     3Q    Max 
#-10331  -7208  -2008   5577  20694 
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) -2.189e+06  1.395e+05  -15.69   <2e-16 ***
#  time(gdp)    1.111e+03  7.019e+01   15.83   <2e-16 ***
#  ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Residual standard error: 8263 on 53 degrees of freedom
#Multiple R-squared:  0.8253,	Adjusted R-squared:  0.8221 
#F-statistic: 250.5 on 1 and 53 DF,  p-value: < 2.2e-16

# ARIMA (Autoregressive integrated moving average)
# MA(3)
ma <- arima(ts, order=c(0,0,3)) # Moving average of 3 time periods
#ARMA(1,1)
arma <- arima(ts, order=c(1,0,1)) # MA of 1 prior period and 1 prior of white noise
#ARIMA(2,1,2)
arima <- arima(ts, order=c(2,1,2)) # 2 white noise, 1st order difference, 2 MA

# Predict based on model
arma.pred <- predict(arma, n.ahead=10) # arma.pred$pred is prediction

# GARCH (Generalized autoregressive conditioned heteroskedastic)
# library (tseries)
garch(gdp, grad="numerical") 

# Using exogenous predictors
model <- Arima(window(hsa06_ts, end = 2008), xreg=window(m, end = 2008), order = c(2,0,0))
mode.forecast <- forecast(model, xreg=window(m, start=2009), h = 3)
mode.forecast$mean # for the prediction values

# Auto arima for auto estimating order parameters
auto.arima(m[,1], xreg=m[,2])
x <- Arima(m[,1], xreg=m[,2], order=c(1,0,2)) # The estimated order

# Box.test (dont know what it does)
Box.test(residuals(fit), fitdf=3, lag=4, type="Ljung")

# Example of using in and out of sample for fitting
# http://stats.stackexchange.com/questions/87111/how-to-put-an-exogenous-variable-into-the-arima-model
intourist <- ts(number of torism, start=c(2540,1),end=c(2550, 12), freq=12)
incli <- ts(CLI_Index, start=c(2540,1),end=c(2550, 12), freq=12)

#your dates are very weird, are you from future? :)
outcli <- ts(CLI_Index, start=c(2551,1),end=c(2553, 12), freq=12)


# sarima (2,1,0)(0,1,1) 12 with cli
tourist.fit1 <- Arima(intourist , order = c(2,1,0), seasonal = list(order=c(0,1,1), period=12) , xreg=incli)
forecast.fit1 <- forecast(tourist.fit1 , h = length(outcli) ,xreg=outcli ) 

# if matrix 
forecast.fit1 <- forecast(tourist.fit1 , h = nrow(outcli) ,xreg=outcli ) 


#plot(fitted(y.fit)) # Fitted model
#lines(p_in) # original data
# Arima model (use caps A, there is an arima that does not work as I expect)
auto.arima(p_in, xreg=e_in) # For estimating the order
fit <- Arima(p_in, xreg=e_in, order = c(1,0,2)) # For fitting the function


fc <- forecast(fit, h = nrow(x.test) ,xreg=x.test ) 
plot(fitted(ep_fit)) # Fitted model
lines(p_in) # original data
