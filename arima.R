require(forecast)
library(lubridate)
library(tidyr)

uber_dummy <- function(date, ini_date, n){
  date = as.Date(date, "%m/%d/%Y")
  date.ini = as.Date(ini_date, "%m/%d/%Y")
  
  w = as.integer(format(date, "%W"))
  y = as.integer(format(date, "%Y"))
  ini.y = as.integer(format(date.ini, "%Y"))
  ini.w = as.integer(format(date.ini, "%W"))
  start = (w-ini.w)+52*(y-ini.y)
  if (start< n){
    return(c(rep(0,start), rep(1,(n-start))))
  }
  else{
    return(rep(0,n))
  }
  
}

uber_dummy.m <- function(date, ini_date, n){
  date = as.Date(date, "%m/%d/%Y")
  date.ini = as.Date(ini_date, "%m/%d/%Y")
  
  m = as.integer(format(date, "%m"))
  y = as.integer(format(date, "%Y"))
  ini.y = as.integer(format(date.ini, "%Y"))
  ini.m = as.integer(format(date.ini, "%m"))
  start = (m-ini.m)+12*(y-ini.y)
  if (start< n){
    return(c(rep(0,start), rep(1,(n-start))))
  }
  else{
    return(rep(0,n))
  }
  
}

yearly_to_weekly <- function(x){
  
  w=rep(x[1],14)
  for( i in seq(2,12)){
    w= c(w,rep(x[i],52))
  }
  w= c(w,rep(x[13],34))
  return(w)
}


# data.flu = read.csv('data/google_trends.csv')
data.flu = read.csv('data/data_flu.csv')
# data.chi = ts(data.flu[,'Chicago..IL'], freq=365.25/7, start=2003+(245+28)/365.25)
data = ts(data.flu[,'San.Francisco..CA'], freq=365.25/7, start=2003+(245+28)/365.25)
# data.ny = ts(data.flu[,'New.York..NY'], freq=365.25/7, start=2003+(245+28)/365.25)
plot(data, xlab='Years', ylab = 'Flu')
plot(diff(data),ylab='Differenced Flu')
plot(log10(data),ylab='Log (Flu)')
plot(diff(log10(data)),ylab='Differenced Log (Flu)')

par(mfrow = c(1,2))
acf(ts(diff(log10(data))),main='ACF Flu')
pacf(ts(diff(log10(data))),main='PACF Flu')

ARIMAfit = auto.arima(log10(data), approximation=TRUE,trace=FALSE, stationary=TRUE, max.p=1, max.q=1)
summary(ARIMAfit)

par(mfrow = c(1,1))
pred = predict(ARIMAfit, n.ahead = 158)
pred
plot(data,type='l',xlim=c(2004,2018),xlab = 'Year',ylab = 'Tractor Sales')
lines(10^(pred$pred),col='blue')
lines(10^(pred$pred+2*pred$se),col='orange')
lines(10^(pred$pred-2*pred$se),col='orange')



#Linear regresion 
week = as.integer(format(as.Date(as.character(data.flu[,1]), "%m/%d/%Y"), "%W"))
month = as.integer(format(as.Date(as.character(data.flu[,1]), "%m/%d/%Y"), "%m"))
year =  as.integer(format(as.Date(as.character(data.flu[,1]), "%m/%d/%Y"), "%Y"))

data.flu$week = week
data.flu$month = month
data.flu$year = year

# ts_m = aggregate(. ~month+year, data=data.flu, FUN=sum, na.rm=TRUE,na.action=NULL)
ts_m = data.flu
# my.data <- gather(data.flu, city, flu)
# data.y <- c(data.flu[,'New.York..NY'][-1],data.flu[,'San.Francisco..CA'][-1],
            # data.flu[,'Chicago..IL'][-1],data.flu[,'Houston..TX'][-1],data.flu[,'Boston..MA'][-1],
            # data.flu[,'Philadelphia..PA'][-1])

data.y <- c(ts_m[,'New.York..NY'][-1],ts_m[,'San.Francisco..CA'][-1],
            ts_m[,'Chicago..IL'][-1], ts_m[,'Houston..TX'][-1],ts_m[,'Boston..MA'][-1],
            ts_m[,'Philadelphia..PA'][-1])
n = nrow(ts_m)
data.y2 <- c(ts_m[,'New.York..NY'][-n],ts_m[,'San.Francisco..CA'][-n],
             ts_m[,'Chicago..IL'][-n],ts_m[,'Houston..TX'][-n],ts_m[,'Boston..MA'][-n],
             ts_m[,'Philadelphia..PA'][-n])

data.week <- c(rep(week[-1],6))
data.month <- c(rep(ts_m$month[-1],6))
data.year <- c(rep(ts_m$year[-1],6))
data.city <- c(rep('NY',nrow(ts_m)-1),rep('SF',nrow(ts_m)-1),
               rep('CH',nrow(ts_m)-1),rep('HO',nrow(ts_m)-1),
               rep('BO',nrow(ts_m)-1),rep('PH',nrow(ts_m)-1)
               )
NY.ptrans = yearly_to_weekly(c(0,0,54.6,54.2,54.6,54.8,54.9,55.7,56.3,55.9,56.7,57.1,57))
CH.ptrans = yearly_to_weekly(c(0,0,25.30,25.40,26.70,26.7,26.5,26.5,27.6,26.3,27.8,28.2,28.3))
SF.ptrans = yearly_to_weekly(c(0,0,32.7,30.3,33,31.9,31.8,34.1,31.6,33.1,32.7,34,34.7))
data.ptrans <- c(NY.ptrans[-1],SF.ptrans[-1],CH.ptrans[-1], rep(0,3*619)) 
ini_date = data.flu[1+7,1]

#Uber pool
NY.pool.date = uber_dummy("12/01/14", ini_date, nrow(ts_m)-1)
SF.pool.date = uber_dummy("09/1/14", ini_date, nrow(ts_m)-1)
CH.pool.date = uber_dummy("11/17/15", ini_date, nrow(ts_m)-1)
HO.pool.date = rep(0,nrow(ts_m)-1)
BO.pool.date = uber_dummy("08/13/15", ini_date, nrow(ts_m)-1)
PH.pool.date = uber_dummy("2/10/16", ini_date, nrow(ts_m)-1)
# BU.pool.date = rep(0,nrow(data.flu))

data.uber.pool <- c(NY.pool.date,SF.pool.date,CH.pool.date,HO.pool.date,BO.pool.date,PH.pool.date)

#Uberx
NY.date = uber_dummy("09/01/13", ini_date, nrow(ts_m)-1)
SF.date = uber_dummy("01/01/13", ini_date, nrow(ts_m)-1)
CH.date = uber_dummy("04/23/13", ini_date, nrow(ts_m)-1)
HO.date = uber_dummy("05/02/14", ini_date, nrow(ts_m)-1)
BO.date = uber_dummy("08/1/13", ini_date, nrow(ts_m)-1)
PH.date = uber_dummy("5/1/14", ini_date, nrow(ts_m)-1)
# BU.date = rep(0,nrow(data.flu))

data.uber <- c(NY.date,SF.date,CH.date,HO.date,BO.date,PH.date)

matrix <- data.frame(y = data.y,
                     week = as.factor(data.week),
                     # month = as.factor(data.month),
                     year = as.factor(data.year),
                     city = as.factor(data.city),
                     uber = as.factor(data.uber),
                     pool =  as.factor(data.uber.pool),
                     ptrans = data.ptrans,
                     prev = data.y2)
 
# matrix <- data.frame(y = data[-1],
#                      week = as.factor(week[-1]),
#                      year = as.factor(year[-1]),
#                      previous = data[-length(data)],
#                      uber = uber[-1]
#                      )


model <- lm(y ~ 0  + city+ year+ week+ pool+uber+ ptrans, data = matrix)
summary(model)
smmr_1 <-summary(model)
paste("R-squared: ",
      round(smmr_1$r.squared, 3),
      ", p-value of F test: ",
      1-pf(smmr_1$fstatistic[1], smmr_1$fstatistic[2], smmr_1$fstatistic[3]))


