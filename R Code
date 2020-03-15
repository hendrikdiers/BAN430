remove(list = ls())

library(forecast)
library(ggplot2)
library(ggpubr) #install.packages("ggpuhr")
library(fma)
library(expsmooth)
library(fpp2)
library(seasonal)
library(mFilter) #install.packages("mFilter")
library(tseries) #install.packages("tseries")
library(scales)
library(stargazer) #install.packages("stargazer")


###### Reading Data 
DATA = read.csv(url("https://raw.githubusercontent.com/hendrikdiers/BAN430/master/data_clean_forecasting_assigment1.csv"), header = TRUE, sep = ";", dec = "," )
DATA = na.omit(DATA) #Deleting omitted variables
GDP = ts((DATA[,2]), frequency = 4, start = c(1990, 1), end = c(2019,4))
UNEMP = ts((DATA[,3]), frequency = 4, start = c(1990, 1), end = c(2019,4))
M3 = ts(log(DATA[,4]), frequency = 4, start = c(1990, 1), end = c(2019,4)) #log transformed


###### Splitting data in Training- and Test-Data

#Training Data: Q1 1990 - Q4 2014 (100)
GDP.train = window(GDP, start = 1990, end = c(2014,4))
UNEMP.train = window(UNEMP/100, start = 1990, end = c(2014,4)) #Levels
M3.train = window(M3, start = 1990, end = c(2014,4))

#Test Data: Q1 2015 - Q4 2019 (20)
GDP.test = window(GDP, start = (2015))
#length of forecast
h20 = 20


###### Preliminary (exploratory) analysis
summary(GDP.train)
quantile(GDP.train)

#Boxplot to visualy verify the results
boxplot(GDP.train,res=100, width = 1000, height = 1000)

autoplot(GDP.train)+
  ggtitle("Real Gross Domestic Product USA")+
  xlab("Quarter")+
  ylab("Value per Quarter")+
  scale_y_continuous(labels = dollar)

#Regression on quartaly dummies and trend
summary(tslm((GDP.train) ~ trend + season))

#Seasonal subseries plots to emphasises seasonal patterns
ggsubseriesplot(GDP.train)+
  ylab("Value per Quarter (in Thousend)") +
  ggtitle("Seasonal subseries plot: GDP USA")+
  scale_y_continuous(labels = dollar)

#Autocorrelation plots
Acf = ggAcf(GDP.train, lag.max = 40)+
  ggtitle("Autocorrelation function for real quarterly GDP USA")
#Parctial Autocorrelation plots
Pacf = ggPacf(GDP.train, lag.max = 40)+
  ggtitle("Partial Autocorrelation function for real quarterly GDP USA")
ggarrange(Acf, Pacf, ncol = 2, nrow=1, legend = NULL,
          font.label = list(size = 14))


###### Time series decomposition and forecasting

#Classical multiplicative decompostion 
#Is widely used but not recommended any more
GDP.train.decomp.mult = decompose(GDP.train, type="multiplicative")
(GDP.train) %>% decompose(type="multiplicative") %>%
  autoplot() + xlab("Quater") +
  scale_y_continuous(name="in Thousands", labels = dollar)
ggtitle("Classical multiplicative decomposition of GDP")

#X11 decomposition
GDP.train.decomp.x11 = (GDP.train) %>% seas(x11="")
autoplot(GDP.train.decomp.x11) +
  xlab("Quater") +
  scale_y_continuous(name="in Thousands", labels = dollar)+
  ggtitle("X11 decomposition of GDP")

#SEATS decomposition
#Seasonal Extraction in ARIMA Time Series
GDP.train %>% seas() %>%
  autoplot() +
  scale_y_continuous(name="in Thousands", labels = dollar)+
  ggtitle("SEATS decompostion of GDP")

#STL decomposition (Chapter 6.6)
#Seasonal and Trend decompostion using Loess
#several advantages over the other decomposing methods
GDP.train.decomp.stl = stl(GDP.train, t.window = 13, s.window = "periodic"  , robust = TRUE)
autoplot(GDP.train.decomp.stl) +
  scale_y_continuous(name="in Thousands", labels = dollar)+
  ggtitle("STL decompostion of GDP")

#Seasonal adjustment: Plotting STL trend component with unadjusted GDP
autoplot(GDP.train, series = "GDP") +
  autolayer(GDP.train.decomp.stl$time.series[,2], series = "GDP trend")+
  xlab("Quater")+ylab("GDP")+scale_y_continuous(name="in Thousands", labels = dollar)+
  ggtitle("GDP and STL trend component")

#Decomposing GDP over total time horizant and extracting test period
GDP.decomp.stl = stl(GDP, t.window = 13, s.window = "periodic"  , robust = TRUE)
GDP.decomp.stl.test.sea = window(GDP.decomp.stl$time.series[,1], start = c(2015), end = c(2019,4))
GDP.decomp.stl.test.trend = window(GDP.decomp.stl$time.series[,2], start = c(2015), end = c(2019,4))
GDP.decomp.stl.test.remain = window(GDP.decomp.stl$time.series[,3], start = c(2015), end = c(2019,4))

#Business cycle
#on seasonal adjusted
GDP.filter.seasadj = hpfilter(GDP.decomp.stl$time.series[,2], drift=TRUE)
GDP.filter.seasadj.cycle = ts(GDP.filter.seasadj$cycle, start = c(1990, 1), end = c(2019, 4), frequency = 4)

#on raw data
GDP.filter.raw = hpfilter(GDP, drift=TRUE)
GDP.filter.raw.cycle = ts(GDP.filter.raw$cycle, start = c(1990, 1), end = c(2019, 4), frequency = 4)

ggplot()+
  autolayer(GDP.filter.seasadj.cycle, series = "Seasonal adjusted")+
  autolayer(GDP.filter.raw.cycle, series = "Seasonal unadjusted")+xlab("Quater")+
  scale_y_continuous(name="Business Cycle", labels = dollar)


###### Forecasting the componantes of the series

#Used decomposition method: STL
GDP.train.decomp.stl.sea = GDP.train.decomp.stl$time.series[,1]
GDP.train.decomp.stl.trend = GDP.train.decomp.stl$time.series[,2]
GDP.train.decomp.stl.remai = GDP.train.decomp.stl$time.series[,3]

#Forecasting the trend

###Trend Forecast 
GDP.train.decomp.stl.trend.fore = forecast(GDP.train.decomp.stl.trend, h = h20)
ggplot() +
  autolayer(GDP.train.decomp.stl.trend.fore, series = "Trend Forecast")+
  autolayer(GDP.decomp.stl.test.trend, series = "Trend in Test Data")+
  ggtitle("Forecast of trend from decomposed series") + xlab("Quater")+
  scale_y_continuous(name="GPD Value (in thousand)", labels = dollar)

###Checking Residuals from Trend Forecast
checkresiduals(GDP.train.decomp.stl.trend.fore)

#Froecasting the Seasonality
GDP.train.decomp.stl.sea.fore = forecast(GDP.train.decomp.stl.sea, h = h20)
ggplot() +
  autolayer(GDP.train.decomp.stl.sea.fore, series = "Seasonal Forecast")+
  autolayer(GDP.decomp.stl.test.sea, series = "Seasonality in Test Data")+
  ggtitle("Forecast of Seasonality from decomposed series")+ xlab("Quater")+
  scale_y_continuous(name="GPD Value (in thousand)", labels = dollar)

#Checking residuals from Seasonaltiy forecast
checkresiduals(GDP.train.decomp.stl.sea.fore)

#Froecasting the Remaining
GDP.train.decomp.stl.remai.fore = forecast(GDP.train.decomp.stl.remai, h = h20)
ggplot() +
  autolayer(GDP.train.decomp.stl.remai.fore, series = "Remaining Forecast")+
  autolayer(GDP.decomp.stl.test.remain, series = "Remaining in Test Data")+
  ggtitle("Forecast of Remaining from decomposed series")+ xlab("Quater")+
  scale_y_continuous(name="GPD Value (in thousand)", labels = dollar)

#Check residuals from remaining forecast
checkresiduals(GDP.train.decomp.stl.remai.fore)

#Forecast unsing the forecast of the three componants
GDP.train.decomp.stl.add.mean = (GDP.train.decomp.stl.remai.fore$mean +
                                   GDP.train.decomp.stl.sea.fore$mean +
                                   GDP.train.decomp.stl.trend.fore$mean)
GDP.train.decomp.stl.add.upper = (GDP.train.decomp.stl.remai.fore$upper +
                                    GDP.train.decomp.stl.sea.fore$upper +
                                    GDP.train.decomp.stl.trend.fore$upper)
GDP.train.decomp.stl.add.lower = (GDP.train.decomp.stl.remai.fore$lower +
                                    GDP.train.decomp.stl.sea.fore$lower +
                                    GDP.train.decomp.stl.trend.fore$lower)
ggplot()+
  autolayer(window(GDP.train, start = c(2005,1)), series = "Taining Data")+
  autolayer(GDP.train.decomp.stl.add.upper[,1], series = "Upper Forecast")+
  autolayer(GDP.train.decomp.stl.add.mean, series = "Mean Forecast")+
  autolayer(GDP.train.decomp.stl.add.lower[,1], series = "Lower Forecast")+            
  autolayer(GDP.test, series = "Test Data")+
  ggtitle("Forecast by additiv decomponant forecast")


#Selecting an appropriate ETS model
GDP.train.ets.auto = ets(GDP.train, model="ZZZ", damped=FALSE, additive.only = FALSE)
summary(GDP.train.ets.auto) #ETS(M,A,M)
autoplot(GDP.train.ets.auto)
GDP.train.ets.auto.fore = forecast(GDP.train.ets.auto, h = 20)

ggplot()+
  autolayer(window(GDP.train, start = c(2005,1)), series = "Training Data")+
  autolayer(GDP.train.ets.auto.fore, series = "Forecast from EST(M,A,M)")+
  autolayer(GDP.test, series = "Test Data")+
  ggtitle("Forecasting with ETS(M,A,M) model")+ scale_y_continuous(name="GPD Value (in thousand)", labels = dollar)+xlab("Quater")

#Portmanteau tests for autocorrelation
#Test if errors are coming from a white noise series
#Box Pierce test
Box.test(GDP.train.ets.auto$residuals, lag = 10, fitdf = 0)
#Box-Ljung test
Box.test(GDP.train.ets.auto$residuals, lag = 10, fitdf = 0, type = "Lj")

#Shapiro_wilk test for residul normality
shapiro.test(GDP.train.ets.auto$residuals)

#Simple forecasting methods to compare with
autoplot(window(GDP.train, start = c(2005,1)), series = "Training Data") +
  autolayer(meanf(GDP.train, h = h20, bootstrap = TRUE), series = "Mean", PI = FALSE)+
  autolayer(naive(GDP.train, h = h20, bootstrap = TRUE), series = "Naive", PI = FALSE)+
  autolayer(snaive(GDP.train, h =  h20, bootstrap = TRUE, drift = TRUE), series = "Sesonal naive", PI = FALSE)+
  autolayer(rwf(GDP.train, h = h20, drift = TRUE, bootstrap = TRUE), series = "Random Walk with drift", PI = FALSE)+
  autolayer(rwf(GDP.train, h = h20, drift = FALSE, bootstrap = TRUE), series = "Random Walk", PI = FALSE)+
  autolayer(GDP.test, series = "Test Data")+
  ggtitle("Forcast for quarterly GDP")+
  xlab("Quater")+scale_y_continuous(name="GPD Value (in thousand)", labels = dollar)+
  guides(colour=guide_legend(title="Forecast"))
#visualy the random walk with drift forecast seems to be the best out of the basic forecast methods
#random walk forecast with drift will be used to compare with other forecast methods

###Exponential Smoothing
#Holt's Method      
GDP.train.fore.holt = holt(GDP.train, h = h20, dumped = FALSE)
#Damped Holt's method: Decreasing trend
#Damped Holt's method seems to be unappropriated
#Holt-Winters additive method: Seasonality
GDP.train.fore.hw1 = hw(GDP.train, seasonal = "additiv", h = h20) 
GDP.train.fore.hw2 = hw(GDP.train, seasonal = "multiplicativ", h = h20)

autoplot(window(GDP.train, start = c(2005,1)))+
  autolayer(GDP.test, series = "Test Data")+
  autolayer(GDP.train.fore.holt, series = "Holt's method", PI = FALSE)+
  autolayer(GDP.train.fore.hw1, series = "HW additiv forecast", PI = FALSE)+
  autolayer(GDP.train.fore.hw2, series = "HW multipicativ forecast", PI = FALSE)+
  ggtitle("Forecast GDP from Holt's method")+
  xlab("Quaters")+scale_y_continuous(name="GPD Value (in thousand)", labels = dollar)+
  guides(colours=guide_legend(title = "Forecast"))
#Visualy the multiplicativ Holt-Winter forecast seems to be the best out of the exponential soomthing forecast methods
#Holt-Winters additive method will be used to compare with other forecast methods

checkresiduals(GDP.train.fore.holt)
checkresiduals(GDP.train.fore.hw1)
checkresiduals(GDP.train.fore.hw2)

GDP.train.fore.holt.acc = accuracy(GDP.train.fore.holt, GDP.test)
GDP.train.fore.hw1.acc = accuracy(GDP.train.fore.hw1, GDP.test)
GDP.train.fore.hw2.acc = accuracy(GDP.train.fore.hw2, GDP.test)
GDP.train.exsm.accu = as.data.frame(matrix(cbind(GDP.train.fore.holt.acc[2,], GDP.train.fore.hw1.acc[2,], GDP.train.fore.hw2.acc[2,]), 3, 8, byrow = TRUE)
                                    , row.names = c("Holt's method", "HW additiv", "HW multipicativ"))
colnames(GDP.train.exsm.accu) = colnames(GDP.train.fore.holt.acc)
GDP.train.exsm.accu
#The multiplicativ forecast seems to be the best regarding the RMSE

#Plot the three most advanced forecasts
ggplot()+
  autolayer(window(GDP.train, start = c(2005,1)), series = "Training Data")+
  autolayer(forecast(GDP.train.ets.auto, h = 20),PI = FALSE, series = "Forecast with estimated EST(M,A,M")+
  autolayer(GDP.train.fore.hw2, series = "HW multipicativ forecast",PI = FALSE)+
  autolayer(rwf(GDP.train, h = h20, drift = TRUE, bootstrap = TRUE), series = "Random Walk with drift", PI = FALSE)+
  autolayer(GDP.test, series = "Test Data") + ggtitle("Diffrent Forecast methods compared") + xlab("Quater")+
  scale_y_continuous(name="GPD Value (in thousand)", labels = dollar)

#Compare distribution of residuals from choosen series
checkresiduals(rwf(GDP.train, drift = TRUE, h = h20))
checkresiduals(GDP.train.fore.hw2)
checkresiduals(GDP.train.ets.auto)

GDP.train.accu.rwf = accuracy(rwf(GDP.train, drift = TRUE, h = h20), GDP.test)
GDP.train.accu.ets = accuracy(f = forecast(GDP.train.ets.auto, h = h20), GDP.test)
GDP.train.accu = as.data.frame(matrix(cbind(GDP.train.accu.rwf[2,], GDP.train.fore.hw2.acc[2,], GDP.train.accu.ets[2,]), 3, 8, byrow = TRUE)
                               , row.names = c("RANDOM W.", "HW multiplicativ forecast", "EST(M,A,M)"))
colnames(GDP.train.accu) = colnames(GDP.train.accu.rwf)
round(GDP.train.accu, digits = 4)


###### Forecasts using splines and Box-Cox
#Box-Cox
lambda = BoxCox.lambda(GDP.train)
#Lambda = 0.1949816
GDP.train.box = BoxCox(GDP.train, lambda)
autoplot(GDP.train.box)+
  ggtitle("GDP Box-Cox transformed (lambda = 0.19)")+xlab("Quater")+scale_y_continuous(name="GPD Value (in thousand)", labels = dollar)

#With and without bias adjustment
GDP.train.box.fore.non = rwf(GDP.train.box, drift=TRUE, lambda=lambda, h=h20, level=80)
GDP.train.box.fore.adj = rwf(GDP.train.box, drift=TRUE, lambda=lambda, h=h20, level=80, biasadj=TRUE)

autoplot(window(GDP.train, start = c(2005,1)), series = "Training Data") +
  autolayer(InvBoxCox(GDP.train.box.fore.adj$mean, lambda = lambda), series="Bias adjusted") +
  autolayer(InvBoxCox(GDP.train.box.fore.non$mean, lambda = lambda), series="Simple back transformation") +
  autolayer(GDP.test, series = "Test Data")+ scale_y_continuous(name="GPD Value (in thousand)", labels = dollar)+xlab("Quater")+ggtitle("Forecasting GDP with Box-Cox transformation")+
  guides(colour=guide_legend(title="Legend"))

#As it could visualy be recognized: Adjusting the bias has not much of an influence
checkresiduals(GDP.train.box.fore.adj)
checkresiduals(GDP.train.box.fore.non)
GDP.train.box.fore.adj.acc = accuracy(GDP.train.box.fore.adj, BoxCox(GDP.test, lambda = lambda))
GDP.train.box.fore.non.acc = accuracy(GDP.train.box.fore.non, BoxCox(GDP.test, lambda = lambda))
GDP.train.box.accu = as.data.frame(matrix(cbind(GDP.train.box.fore.adj.acc[2,], GDP.train.box.fore.non.acc[2,]), 2, 8, byrow = TRUE)
                                   , row.names = c("BC adjusted", "BC not adjusted"))
colnames(GDP.train.box.accu) = colnames(GDP.train.box.fore.adj.acc)
round(GDP.train.box.accu, digits = 4)

#Splines
GDP.train.spline = splinef(GDP.train, h = 20)
autoplot(window(GDP.train, start = c(2005,1)), series = "Training Data")+
  autolayer(GDP.train.spline, series = "Forecast")+
  autolayer(GDP.test, series = "Test Data")+
  ggtitle("Forecasting GDP with Natural cubic smoothing splines") + scale_y_continuous(name="GPD Value (in thousand)", labels = dollar) +
  xlab("Quater")

checkresiduals(GDP.train.spline)
accuracy(GDP.train.spline, GDP.test)
shapiro.test(GDP.train.spline$residuals)

#Compare both forecasts
autoplot(window(GDP.train, start = c(2005,1)), series = "Training Data")+
  autolayer(InvBoxCox(GDP.train.box.fore.non$mean, lambda = lambda), series="Forecast with BoxCox") +
  autolayer(GDP.train.spline, series = "Forecast from Splines", PI = FALSE)+
  autolayer(GDP.test, series = "Test Data")+
  scale_y_continuous(name="GPD Value (in thousand)", labels = dollar) + xlab("Quater") + ggtitle("Comparing Forecast methods: Box-Cox and Natural cubic smoothing splines") 


###### Time series cross-validation
GDP.tsCV = tsCV(GDP.train, rwf, h = 2) 
GDP.tsCV
summary(GDP.tsCV)

tsCV_self = function(ts, fcfunction, h=1){ #creating the tsCV_self function which takes an timeseries, a forecasting function and h as attributes
  ts = as.ts(ts)                #creating a time series object
  n = length(ts)                #defining the length of the output matrix 
  m = ts(matrix(NA, nrow = n, ncol = h)) #constrction of the output matrix, in the first place filled with "NA" which will be overwritten by the function
  tsp(m) = tsp(ts)              #Ensures that the final matirx has the same time series attributes like the given timeseries, (start, end, frequency)
  indx = seq(1, n - 1L)         
  for (i in indx) {
    ts.subset = subset(         #defining the subset
      ts,
      start = 1L,
      end = i
    )
    fc = fcfunction(ts.subset, h = h) #applying the forecast function successively on the subset
    if (!is.element("try-error", class(fc))) { #making sure that no error has occured 
      m[i, ] = ts[i + (1:h)] - fc$mean         #Calculating the errors and writing them into the output matrix
    }
  }  
  if (h == 1) { #if h=1 the output is given as a vector
    return(m[, 1L])
  } else {
    colnames(m) = paste("h=", 1:h, sep = "")
    return(m) #the filled matrix wil be the return of this function
  }    
}

GDP.tsCV_self = tsCV_self(GDP.train, rwf, h = 5)
GDP.tsCV_self
summary(GDP.tsCV_self)


###### Integrating other variables
GDP.train.diff = diff(log(GDP.train), lag = 1)
#tests for stationarity
kpss.test(GDP.train.diff, null="Trend") 
adf.test(GDP.train.diff)

UNEMP.train.level = window(UNEMP.train, start = c(1990,2))
M3.train.diff = diff(M3.train, lag = 1)

autoplot(GDP.train.diff, series = "First dif. GDP")+
  autolayer(M3.train.diff, series = "First dif. M3")+
  autolayer(UNEMP.train.level, series = "Unemployment Rate")+
  ggtitle("Plot of first dif. in GDP, Unemployment and M3")+ylab("")+xlab("Quater")

autoplot(cbind(GDP.train.diff, UNEMP.train.level, M3.train.diff), facets = TRUE)+
  ggtitle("Plot of GDP with additional variables") + xlab("Quater") + ylab("")

qplot(GDP.train.diff, M3.train.diff, asp = 1) +
  ylab("First Difference log(M3)") + xlab("First Difference log(GDP)")+
  ggtitle("Scatterplot: First differences log(GDP) - log(M3)")

qplot(GDP.train.diff, UNEMP.train.level, asp = 1) +
  ylab("Unemployment Rate") + xlab("First Difference log(GDP)")+
  ggtitle("Scatterplot: First difference log(GDP) - Unemploymant Rate")

GDP.train.fit = tslm(GDP.train.diff ~ M3.train.diff + UNEMP.train.level + season) #as obvious, there should not be a trend
summary(GDP.train.fit)
checkresiduals(GDP.train.fit)

GDP.train.fit.fore1 = forecast(GDP.train.fit,
                               newdata = data.frame(
                                 M3.train.diff = rep(mean(M3.train.diff), h20),
                                 UNEMP.train.level = rep(mean(UNEMP.train.level),h20)),
                               h = h20)

GDP.train.fit.fore2 = forecast(GDP.train.fit,
                               newdata = data.frame(
                                 M3.train.diff = rep(mean(M3.train.diff), h20),
                                 UNEMP.train.level = rep(5*mean(UNEMP.train.level),h20)),
                               h = h20)

GDP.train.fit.fore3 = forecast(GDP.train.fit,
                               newdata = data.frame(
                                 M3.train.diff = rep(mean(M3.train.diff), h20),
                                 UNEMP.train.level = rep(0.1*mean(UNEMP.train.level),h20)),
                               h = h20)

autoplot(window(GDP.train.diff, start = c(2005,1)), series = "Training Data")+
  autolayer(GDP.train.fit.fore1, series = "Mean of past Unemployment and M3", PI = FALSE)+
  autolayer(GDP.train.fit.fore2, series = "Five times the Mean of past unemployment rate", PI = FALSE)+
  autolayer(GDP.train.fit.fore3, series = "10% of the past unemployment rate", PI = FALSE)+
  autolayer(diff(log(GDP.test), lag = 1), series = "Test Data")+
  ggtitle("Forecasting first differenc Log(GDP) with diffrent Unemployment rates and M3")+xlab("Quater")+ylab("Differnce")

