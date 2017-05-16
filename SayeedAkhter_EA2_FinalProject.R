############################## SAYEED AKHTER MOHAMMED
############################## ANALYTICS 2 PROJECT

############################## PART 1 (LDA & QDA) #####################################################

# Importing the data 
mydata=read.csv(file="C:/Users/akhte/Desktop/Engineering Analytics II/Project/pollution_us_2000_2016.csv", header = TRUE)
head(mydata)
# Removing NAs
mydata=na.omit(mydata)
summary(mydata)
str(mydata$Date.Local)
#converting mydata into date format
mydata$Date.Local <- as.Date(mydata$Date.Local, "%Y-%m-%d")
range(mydata$Date.Local)

#Selecting the State column and required pollutant AQIs
newdata=mydata[,c(6,14,19,24,29)]
newdata=data.frame(newdata)

#Splitting data into train(75%) and test(25%)
smp_size=floor(0.75 * nrow(newdata))
set.seed(123)
train_ind=sample(seq_len(nrow(newdata)), size = smp_size)

train=newdata[train_ind, ]
test=newdata[-train_ind, ]

#LDA
## lda() prints discriminant functions based on centered (not standardized) variables. 
## The "proportion of trace" that is printed is the proportion of between-class variance 
## that is explained by successive discriminant functions.
## the list element 'class' gives the predicted class 
library(MASS)
lda=lda(State~., data = train)
lda
summary(predict(lda, test)$class)
lda$scaling
#the singular values (svd) that gives the ratio of the between-
#and within-group standard deviations on the linear discriminant variables
lda$svd
#We can use the singular values
#to compute the amount of the between-group variance that is explained by each linear discriminant.
var_lda = lda$svd^2/sum(lda$svd^2)
var_lda
#we see that the first linear discriminant explains more than {~52%} of the between-group variance

#QDA
qda=qda(State~., data = train)
qda
summary(predict(qda, test)$class)
qda$scaling



############################## PART 2 (CLUSTERING) #####################################################

# Standardize data for Clustering
mydata.NO2=mydata[,c(11:14)]
mydata.O3=mydata[,c(16:19)]
mydata.SO2=mydata[,c(21:24)]
mydata.CO=mydata[,c(26:29)]

mydata.NO2=scale(mydata.NO2,center = TRUE,scale = TRUE)
mydata.O3=scale(mydata.O3,center = TRUE,scale = TRUE)
mydata.SO2=scale(mydata.SO2,center = TRUE,scale = TRUE)
mydata.CO=scale(mydata.CO,center = TRUE,scale = TRUE)

###################### Now repeating for NO2 ####################################

wss=(nrow(mydata.NO2)-1)*sum(apply(mydata.NO2,2,var))
for (i in 2:15) wss[i]=sum(kmeans(mydata.NO2, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters for NO2",ylab="Within groups sum of squares")

# K-Means=5 Means Clustering
k.means.NO2=kmeans(mydata.NO2, 5)
k.means.NO2
# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
library(cluster) 
clusplot(mydata.NO2, k.means.NO2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# get cluster means 
aggregate(mydata.NO2,by=list(k.means.NO2$cluster),FUN=mean)

# So how well did the K-means clustering uncover the actual structure of the data contained in the type variable? 
# A cross-tabulation of NO2 to the States in USA is given by
ct.NO2=table(mydata$State, k.means.NO2$cluster)
ct.NO2

#We can quantify the agreement between the States and cluster,
#using an adjusted Rank index provided by the flexclust package.
library(flexclust)
randIndex(ct.NO2)
#The adjusted Rand index provides a measure of the agreement between two partitions, adjusted for chance. It ranges from -1 (no agreement) to 1 (perfect agreement).
#Agreement between the States and the cluster solution is 0.005. BAD!!

###################### Now repeating for O3 ####################################

# Determine number of clusters
wss=(nrow(mydata.O3)-1)*sum(apply(mydata.O3,2,var))
for (i in 2:15) wss[i]=sum(kmeans(mydata.O3, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters for O3",ylab="Within groups sum of squares")

# K-Means=6 Means Clustering
k.means.O3=kmeans(mydata.O3, 6)
k.means.O3
# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
#library(cluster)
#clusplot(mydata.O3, k.means.O3$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
#Not using it as the plot doesnt show any detail on boundaries

# Get cluster means 
aggregate(mydata.O3,by=list(k.means.O3$cluster),FUN=mean)

# So how well did the K-means clustering uncover the actual structure of the data contained in the type variable? 
# A cross-tabulation of NO2 to the States in USA is given by
ct.O3=table(mydata$State, k.means.O3$cluster)
ct.O3

#We can quantify the agreement between the States and cluster,
#using an adjusted Rank index provided by the flexclust package.
library(flexclust)
randIndex(ct.O3)
#The adjusted Rand index provides a measure of the agreement between two partitions, adjusted for chance. It ranges from -1 (no agreement) to 1 (perfect agreement).
#Agreement between the States and the cluster solution is 0.01. BAD but better than NO2!!

###################### Now repeating for SO2 ####################################

# SO2- Determine number of clusters
wss=(nrow(mydata.SO2)-1)*sum(apply(mydata.SO2,2,var))
for (i in 2:15) wss[i]=sum(kmeans(mydata.SO2, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters for SO2",ylab="Within groups sum of squares")

# K-Means=4 Means Clustering
k.means.SO2=kmeans(mydata.SO2, 5)
k.means.SO2
# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
#library(cluster) 
#clusplot(mydata.SO2, k.means.SO2$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# get cluster means 
aggregate(mydata.SO2,by=list(k.means.SO2$cluster),FUN=mean)

# So how well did the K-means clustering uncover the actual structure of the data contained in the type variable? 
# A cross-tabulation of NO2 to the States in USA is given by
ct.SO2=table(mydata$State, k.means.SO2$cluster)
ct.SO2

#We can quantify the agreement between the States and cluster,
#using an adjusted Rank index provided by the flexclust package.
library(flexclust)
randIndex(ct.SO2)
#The adjusted Rand index provides a measure of the agreement between two partitions, adjusted for chance. It ranges from -1 (no agreement) to 1 (perfect agreement).
#Agreement between the States and the cluster solution is 0.060. FINE and better than NO2 and O3!!

###################### Now repeating for CO ####################################

# CO- Determine number of clusters
wss=(nrow(mydata.CO)-1)*sum(apply(mydata.CO,2,var))
for (i in 2:15) wss[i]=sum(kmeans(mydata.CO, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters for CO",ylab="Within groups sum of squares")

# K-Means=5 Means Clustering
k.means.CO=kmeans(mydata.CO, 5)
k.means.CO
# Cluster Plot against 1st 2 principal components
# vary parameters for most readable graph
#library(cluster) 
#clusplot(mydata.CO, k.means.CO$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)

# get cluster means 
aggregate(mydata.CO,by=list(k.means.CO$cluster),FUN=mean)

# So how well did the K-means clustering uncover the actual structure of the data contained in the type variable? 
# A cross-tabulation of NO2 to the States in USA is given by
ct.CO=table(mydata$State, k.means.CO$cluster)
ct.CO

#We can quantify the agreement between the States and cluster,
#using an adjusted Rank index provided by the flexclust package.
library(flexclust)
randIndex(ct.CO)
#The adjusted Rand index provides a measure of the agreement between two partitions, adjusted for chance. It ranges from -1 (no agreement) to 1 (perfect agreement).
#Agreement between the States and the cluster solution is -0.007. As BAD as NO2 and O3!!



############################## PART 3 (Time Series & Forecasting)) ################################################

library(forecast)
mydata=read.csv(file="C:/Users/akhte/Desktop/Engineering Analytics II/Project/pollution_us_2000_2016.csv", header = TRUE)
mydata=mydata[complete.cases(mydata),]

# Converting mydata into date format
mydata$Date.Local <- as.Date(mydata$Date.Local, "%Y-%m-%d")
range(mydata$Date.Local)

############################ Starting with NO2 #################################

mydataNO2=mydata$NO2.Mean

#Let's go and observe what the data looks like
mydataNO2 = as.data.frame(mydataNO2)
NO2ts = ts(mydataNO2)
NO2ts
# Now let's plot the time series to see how it is behaving'
plot.ts(NO2ts)

# Plot trend is not clearly visible
library("TTR")
NO2ts.SMA = SMA(NO2ts, n=30)
plot.ts(NO2ts.SMA)
# There are still a lot of random fluctuations,
# Let's see if we can smooth it so that we can understand the trend better
NO2ts.SMA = SMA(NO2ts, n=10000)
plot.ts(NO2ts.SMA)
# Clearly there is a dip in Mean with time
# By removing the random component using smoothing, we are able to get more insights into this

# Now let's look at a few ways of decomposing the time series using R
# Decomposing Non-Seasonal data: Typical non-seasonal data consist of a trend component and a random component
# This involves decomposing seasonal data into seasonal component, trend component, and random component
mydata=mydata[order(mydata$Date.Local), ]
plot(mydata$Date.Local, mydata$NO2.Mean, type = "l")
years=format(mydata$Date.Local, "%Y")
tab=table(years)
tab
mean(tab[1:(length(tab) - 1)])

# The TS seems to have seasonality, trend, and of course some random fluctuations as well
## Now let's coerce it and read it as a time series
NO2_TS = ts(mydataNO2, start = 2000, frequency = 26920 )
NO2_TS
NO2_decomp=decompose(NO2_TS)

# We can plot all the decomposition components on the same chart
plot(NO2_decomp)

######################################################################

# Now let's talk about forecasting
# If there is a standard time series with no seasonality
# we can use simple exponential smoothing for forecasting
# As we discussed earlier for simple expo smoothing
# the parameter is alpha. 0 < alpha < 1
myTs_NO2 = ts(mydata$NO2.Mean, start=c(2000,1), frequency=26920)
NO2Forecast = HoltWinters(myTs_NO2, beta = F, gamma = F)
NO2Forecast
##By default, HoltWinters() just makes forecasts for the same time period covered by our original time series. 
NO2Forecast$fitted
plot(NO2Forecast)

#rainForecast_future = forecast.HoltWinters(NO2Forecast, h = 100000)
#rainForecast_future
#plot.forecast(rainForecast_future)
#rainForecast_future$residuals

######################################################################

## Forecast with ARIMA(1,0,0) model 
forecastArima = function(mydata, n.ahead=100000){
  myTs = ts(mydata$NO2.Mean, start=c(2000,1), frequency=26920)
  fit.arima = arima(myTs, order=c(1,0,0))
  fore = forecast(fit.arima, h=n.ahead)
  plot(fore)
  upper = fore$upper[,'95%']
  lower = fore$lower[,'95%']
  trend = as.numeric(fore$fitted)
  pred = as.numeric(fore$mean)
  output = data.frame(actual = c(mydata$NO2.Mean, rep(NA, n.ahead)),
                      trend = c(trend, rep(NA, n.ahead)),
                      pred = c(rep(NA, nrow(mydata)), pred),
                      lower = c(rep(NA, nrow(mydata)), lower),                       
                      upper = c(rep(NA, nrow(mydata)), upper),                       
                      date = c(mydata$Date.Local, max(mydata$Date.Local) + (1:n.ahead))  
  )
  return(output)
}

result.arima = forecastArima(mydata, n.ahead = 100000)
plot(result.arima)


############################ Starting with O3 #################################

mydataO3=mydata$O3.Mean

#Let's go and observe what the data looks like
mydataO3 = as.data.frame(mydataO3)
O3ts = ts(mydataO3)
O3ts
# Now let's plot the time series to see how it is behaving'
plot.ts(O3ts)

# Plot trend is not clearly visible
library("TTR")
O3ts.SMA = SMA(O3ts, n=30)
plot.ts(O3ts.SMA)
# There are still a lot of random fluctuations,
# Let's see if we can smooth it so that we can understand the trend better
O3ts.SMA = SMA(O3ts, n=10000)
plot.ts(O3ts.SMA)
# Clearly there is a dip in Mean with time
# By removing the random component using smoothing, we are able to get more insights into this

# Now let's look at a few ways of decomposing the time series using R
# Decomposing Non-Seasonal data: Typical non-seasonal data consist of a trend component and a random component
# This involves decomposing seasonal data into seasonal component, trend component, and random component
mydata=mydata[order(mydata$Date.Local), ]
plot(mydata$Date.Local, mydata$O3.Mean, type = "l")
years=format(mydata$Date.Local, "%Y")
tab=table(years)
tab
mean(tab[1:(length(tab) - 1)])

# The TS seems to have seasonality, trend, and of course some random fluctuations as well
## Now let's coerce it and read it as a time series
O3_TS = ts(mydataO3, start = 2000, frequency = 26920 )
O3_TS
O3_decomp=decompose(O3_TS)

# We can plot all the decomposition components on the same chart
plot(O3_decomp)

####################################################################

# Now let's talk about forecasting
# If there is a standard time series with no seasonality
# we can use simple exponential smoothing for forecasting
# As we discussed earlier for simple expo smoothing
# the parameter is alpha. 0 < alpha < 1
myTs_O3 = ts(mydata$O3.Mean, start=c(2000,1), frequency=26920)
O3Forecast = HoltWinters(myTs_O3, beta = F, gamma = F)
O3Forecast
##By default, HoltWinters() just makes forecasts for the same time period covered by our original time series. 
O3Forecast$fitted
plot(O3Forecast)

#rainForecast_future = forecast.HoltWinters(O3Forecast, h = 100000)
#rainForecast_future
#plot.forecast(rainForecast_future)
#rainForecast_future$residuals

######################################################################

## Forecast with ARIMA(1,0,0) model 
forecastArima = function(mydata, n.ahead=100000){
  myTs = ts(mydata$O3.Mean, start=c(2000,1), frequency=26920)
  fit.arima = arima(myTs, order=c(1,0,0))
  fore = forecast(fit.arima, h=n.ahead)
  plot(fore)
  upper = fore$upper[,'95%']
  lower = fore$lower[,'95%']
  trend = as.numeric(fore$fitted)
  pred = as.numeric(fore$mean)
  output = data.frame(actual = c(mydata$NO2.Mean, rep(NA, n.ahead)),
                      trend = c(trend, rep(NA, n.ahead)),
                      pred = c(rep(NA, nrow(mydata)), pred),
                      lower = c(rep(NA, nrow(mydata)), lower),                       
                      upper = c(rep(NA, nrow(mydata)), upper),                       
                      date = c(mydata$Date.Local, max(mydata$Date.Local) + (1:n.ahead))  
  )
  return(output)
}

result.arima = forecastArima(mydata, n.ahead = 100000)
plot(result.arima)

############################ Starting with SO2 #################################

mydata=mydata[complete.cases(mydata),]
mydata$Date.Local <- as.Date(mydata$Date.Local, "%Y-%m-%d")
mydata=mydata[order(mydata$Date.Local), ]
mydataSO2=mydata[mydata$Date.Local>"2010-12-31",]
dataSO2=mydataSO2$SO2.Mean
#Let's go and observe what the data looks like
dataSO2 = as.data.frame(dataSO2)
SO2ts = ts(mydataSO2)
SO2ts
# Now let's plot the time series to see how it is behaving'
plot.ts(SO2ts)

# Plot trend is not clearly visible
library("TTR")
SO2ts.SMA = SMA(SO2ts, n=30)
plot.ts(SO2ts.SMA)
# There are still a lot of random fluctuations,
# Let's see if we can smooth it so that we can understand the trend better
SO2ts.SMA = SMA(SO2ts, n=10000)
plot.ts(SO2ts.SMA)
# Clearly there is a dip in Mean with time
# By removing the random component using smoothing, we are able to get more insights into this

# Now let's look at a few ways of decomposing the time series using R
# Decomposing Non-Seasonal data: Typical non-seasonal data consist of a trend component and a random component
# This involves decomposing seasonal data into seasonal component, trend component, and random component
mydata=mydata[order(mydata$Date.Local), ]
plot(mydataSO2$Date.Local, mydataSO2$SO2.Mean, type = "l")
years=format(mydataSO2$Date.Local, "%Y")
tab=table(years)
tab
mean(tab[1:(length(tab) - 1)])

# The TS seems to have seasonality, trend, and of course some random fluctuations as well
## Now let's coerce it and read it as a time series
SO2_TS = ts(dataSO2, start = 2011, frequency = 32499 )
SO2_TS
SO2_decomp=decompose(SO2_TS)

# We can plot all the decomposition components on the same chart
plot(SO2_decomp)

######################################################################

# Now let's talk about forecasting
# If there is a standard time series with no seasonality
# we can use simple exponential smoothing for forecasting
# As we discussed earlier for simple expo smoothing
# the parameter is alpha. 0 < alpha < 1
myTs_SO2 = ts(dataSO2, start=c(2011,1), frequency=32499)
SO2Forecast = HoltWinters(myTs_SO2, beta = F, gamma = F)
SO2Forecast
##By default, HoltWinters() just makes forecasts for the same time period covered by our original time series. 
SO2Forecast$fitted
plot(SO2Forecast)

#rainForecast_future = forecast.HoltWinters(SO2Forecast, h = 100000)
#rainForecast_future
#plot.forecast(rainForecast_future)
#rainForecast_future$residuals

######################################################################

## Forecast with ARIMA(1,0,0) model 
forecastArima = function(mydataSO2, n.ahead=100000){
  myTs = ts(mydataSO2$SO2.Mean, start=c(2011,1), frequency=32499)
  fit.arima = arima(myTs, order=c(1,0,0))
  fore = forecast(fit.arima, h=n.ahead)
  plot(fore)
  upper = fore$upper[,'95%']
  lower = fore$lower[,'95%']
  trend = as.numeric(fore$fitted)
  pred = as.numeric(fore$mean)
  output = data.frame(actual = c(mydataSO2$SO2.Mean, rep(NA, n.ahead)),
                      trend = c(trend, rep(NA, n.ahead)),
                      pred = c(rep(NA, nrow(mydataSO2)), pred),
                      lower = c(rep(NA, nrow(mydataSO2)), lower),                       
                      upper = c(rep(NA, nrow(mydataSO2)), upper),                       
                      date = c(mydataSO2$Date.Local, max(mydataSO2$Date.Local) + (1:n.ahead))  
  )
  return(output)
}

result.arima = forecastArima(mydataSO2, n.ahead = 100000)
plot(result.arima)

############################ Starting with CO #################################

mydata=mydata[complete.cases(mydata),]
mydataCO=mydata$CO.Mean

#Let's go and observe what the data looks like
mydataCO = as.data.frame(mydataNO2)
COts = ts(mydataCO)
COts
# Now let's plot the time series to see how it is behaving'
plot.ts(COts)

# Plot trend is not clearly visible
library("TTR")
COts.SMA = SMA(COts, n=30)
plot.ts(COts.SMA)
# There are still a lot of random fluctuations,
# Let's see if we can smooth it so that we can understand the trend better
COts.SMA = SMA(COts, n=10000)
plot.ts(COts.SMA)
# Clearly there is a dip in Mean with time
# By removing the random component using smoothing, we are able to get more insights into this

# Now let's look at a few ways of decomposing the time series using R
# Decomposing Non-Seasonal data: Typical non-seasonal data consist of a trend component and a random component
# This involves decomposing seasonal data into seasonal component, trend component, and random component
mydata=mydata[order(mydata$Date.Local), ]
plot(mydata$Date.Local, mydata$CO.Mean, type = "l")
years=format(mydata$Date.Local, "%Y")
tab=table(years)
tab
mean(tab[1:(length(tab) - 1)])

# The TS seems to have seasonality, trend, and of course some random fluctuations as well
## Now let's coerce it and read it as a time series
CO_TS = ts(mydataCO, start = 2000, frequency = 26920 )
CO_TS
CO_decomp=decompose(CO_TS)

# We can plot all the decomposition components on the same chart
plot(CO_decomp)

######################################################################

# Now let's talk about forecasting
# If there is a standard time series with no seasonality
# we can use simple exponential smoothing for forecasting
# As we discussed earlier for simple expo smoothing
# the parameter is alpha. 0 < alpha < 1
myTs_CO = ts(mydata$CO.Mean, start=c(2000,1), frequency=26920)
COForecast = HoltWinters(myTs_CO, beta = F, gamma = F)
COForecast
##By default, HoltWinters() just makes forecasts for the same time period covered by our original time series. 
COForecast$fitted
plot(COForecast)

#rainForecast_future = forecast.HoltWinters(NO2Forecast, h = 100000)
#rainForecast_future
#plot.forecast(rainForecast_future)
#rainForecast_future$residuals

######################################################################

## Forecast with ARIMA(1,0,0) model 
forecastArima = function(mydata, n.ahead=100000){
  myTs = ts(mydata$CO.Mean, start=c(2000,1), frequency=26920)
  fit.arima = arima(myTs, order=c(1,0,0))
  fore = forecast(fit.arima, h=n.ahead)
  plot(fore)
  upper = fore$upper[,'95%']
  lower = fore$lower[,'95%']
  trend = as.numeric(fore$fitted)
  pred = as.numeric(fore$mean)
  output = data.frame(actual = c(mydata$NO2.Mean, rep(NA, n.ahead)),
                      trend = c(trend, rep(NA, n.ahead)),
                      pred = c(rep(NA, nrow(mydata)), pred),
                      lower = c(rep(NA, nrow(mydata)), lower),                       
                      upper = c(rep(NA, nrow(mydata)), upper),                       
                      date = c(mydata$Date.Local, max(mydata$Date.Local) + (1:n.ahead))  
  )
  return(output)
}

result.arima = forecastArima(mydata, n.ahead = 100000)
plot(result.arima)