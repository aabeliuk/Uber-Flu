require(forecast)
library(lubridate)
library(tidyr)

uber_dummy <- function(date, ini_date, n){
  date = as.Date(date, "%m/%d/%Y")
  date.ini = as.Date(ini_date, "%m/%d/%Y")
  
  w = as.integer(format(date, "%W"))
  y = as.integer(format(date, "%Y"))
  ini = as.integer(format(date.ini, "%Y"))
  start = w+52*(y-ini)
  return(c(rep(0,start), rep(1,(n-start))))
}


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
year =  as.integer(format(as.Date(as.character(data.flu[,1]), "%m/%d/%Y"), "%Y"))

# my.data <- gather(data.flu, city, flu)
data.y <- c(data.flu[,'New.York..NY'],data.flu[,'San.Francisco..CA'],data.flu[,'Chicago..IL'])
data.week <- c(rep(week,3))
data.year <- c(rep(year,3))
data.uber <- c(NY.date,SF.date,CH.date)
NY.date = uber_dummy("05/01/11", "9/28/03", nrow(data.flu))
SF.date = uber_dummy("07/1/10", "9/28/03", nrow(data.flu))
CH.date = uber_dummy("04/23/13", "9/28/03", nrow(data.flu))



matrix <- data.frame(y = data.y,
                     week = as.factor(data.week),
                     year = as.factor(data.year),
                     uber = as.factor(data.uber))
 
# matrix <- data.frame(y = data[-1],
#                      week = as.factor(week[-1]),
#                      year = as.factor(year[-1]),
#                      previous = data[-length(data)],
#                      uber = uber[-1]
#                      )


model <- lm(y ~ 0 + week + uber, data = matrix)
summary(model)
smmr_1 <-summary(model)
paste("R-squared: ",
      round(smmr_1$r.squared, 3),
      ", p-value of F test: ",
      1-pf(smmr_1$fstatistic[1], smmr_1$fstatistic[2], smmr_1$fstatistic[3]))


