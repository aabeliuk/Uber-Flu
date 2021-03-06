require(forecast)
library(forecast)
library(lubridate)
library(tidyr)

library(zoo)
library(dyn)

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
uber_dummy <- function(date, ini_date, n){
  print(date)
  print(ini_date)
  date = as.Date(paste(date,1,sep="-"),"%Y-%m-%d")
  date.ini = as.Date(paste(ini_date,1,sep="-"), format="%Y-%m-%d")
  #date = as.Date(c(date), format="%Y-%m")
  #date.ini = as.Date(c(ini_date), format="%Y-%m")

  
  w = as.integer(format(date, "%m"))
  y = as.integer(format(date, "%Y"))
  ini = as.integer(format(date.ini, "%Y"))
  start = w+12*(y-ini)
  #sprintf("%s, %s, %s yields %s", w, y, ini, start)
  #return()
  return(c(rep(0,start), rep(1,(n-start))))
}


data.flu = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/data_flu.csv')
flu_trends.flu = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/cold_and_flu.csv')
flu_trends.maxtemp = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/max_temp_monthly.csv')
flu_trends.mintemp = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/min_temp_monthly.csv')
flu_trends.prcp = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/prcp_monthly.csv')
flu_trends.snow = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/snow_monthly.csv')


# data.chi = ts(data.flu[,'Chicago..IL'], freq=365.25/7, start=2003+(245+28)/365.25)
# data.ny = ts(data.flu[,'New.York..NY'], freq=365.25/7, start=2003+(245+28)/365.25)
flu_trends = ts(flu_trends.flu['Geo..New.York.NY'], freq=365.25/12, start=2004)
plot(flu_trends, xlab='Years', ylab = 'Flu')
plot(diff(flu_trends),ylab='Differenced Flu')
plot(log10(flu_trends),ylab='Log (Flu)')
plot(diff(log10(flu_trends)),ylab='Differenced Log (Flu)')

par(mfrow = c(1,2))
acf(ts(diff(log10(flu_trends))),main='ACF Flu')
pacf(ts(diff(log10(flu_trends))),main='PACF Flu')



#Linear regresion 
month = as.integer(format(as.Date(paste(as.character(flu_trends.flu[,1]),1,sep="-"), "%Y-%m-%d"), "%m"))
year = as.integer(format(as.Date(paste(as.character(flu_trends.flu[,1]),1,sep="-"), "%Y-%m-%d"), "%Y"))

#month = as.integer(format(as.Date(as.character(flu_trends.flu[,1]), "%Y-%m"), "%m"))
#year =  as.integer(format(as.Date(as.character(flu_trends.flu[,1]), "%Y-%m"), "%Y"))

# my.data <- gather(data.flu, city, flu)
lengths(flu_trends.flu[,'Geo..New.York.NY'])
flu_trends.y <- c(flu_trends.flu[,'Geo..New.York.NY'],
                  flu_trends.flu[,'Geo..San.Francisco.Oakland.San.Jose.CA'],
                  flu_trends.flu[,'Geo..Chicago.IL'],
                  flu_trends.flu[,'Geo..Philadelphia.PA'],
                  flu_trends.flu[,'Geo..Los.Angeles.CA'],
                  flu_trends.flu[,'Geo..Miami.Ft..Lauderdale.FL'],
                  flu_trends.flu[,'Geo..Phoenix.AZ'],
                  flu_trends.flu[,'Geo..Seattle.Tacoma.WA'],
                  flu_trends.flu[,'Geo..Buffalo.NY'])
flu_trends.maxtemp <- c(flu_trends.maxtemp[,'NewYork'],
                        flu_trends.maxtemp[,'SanFrancisco'],
                        flu_trends.maxtemp[,'Chicago'],
                        flu_trends.maxtemp[,'Philadelphia'],
                        flu_trends.maxtemp[,'LosAngeles'],
                        flu_trends.maxtemp[,'Miami'],
                        flu_trends.maxtemp[,'Phoenix'],
                        flu_trends.maxtemp[,'Seattle'],
                        flu_trends.maxtemp[,'Buffalo'])
flu_trends.mintemp <- c(flu_trends.mintemp[,'NewYork'],
                        flu_trends.mintemp[,'SanFrancisco'],
                        flu_trends.mintemp[,'Chicago'],
                        flu_trends.mintemp[,'Philadelphia'],
                        flu_trends.mintemp[,'LosAngeles'],
                        flu_trends.mintemp[,'Miami'],
                        flu_trends.mintemp[,'Phoenix'],
                        flu_trends.mintemp[,'Seattle'])
flu_trends.prcp <- c(flu_trends.prcp[,'NewYork'],
                        flu_trends.prcp[,'SanFrancisco'],
                        flu_trends.prcp[,'Chicago'],
                        flu_trends.prcp[,'Philadelphia'],
                        flu_trends.prcp[,'LosAngeles'],
                        flu_trends.prcp[,'Miami'],
                     flu_trends.prcp[,'Phoenix'],
                     flu_trends.prcp[,'Seattle'])
flu_trends.snow <- c(flu_trends.snow[,'NewYork'],
                        flu_trends.snow[,'SanFrancisco'],
                        flu_trends.snow[,'Chicago'],
                        flu_trends.snow[,'Philadelphia'],
                        flu_trends.snow[,'LosAngeles'],
                     flu_trends.snow[,'Miami'],
                     flu_trends.snow[,'Phoenix'],
                     flu_trends.snow[,'Seattle'])
flu_trends.month <- c(rep(month,8))
flu_trends.year <- c(rep(year,8))
flu_trends.uberx <- c(NY.datex,SF.datex,CH.datex, PHI.datex, LA.datex, MI.datex, PNX.datex, SEA.datex)
flu_trends.uberpool <- c(NY.datepool, SF.datepool, CH.datepool,   PHI.datepool, LA.datepool, MI.datepool, PNX.datepool, SEA.datepool)
flu_trends.city <- c(rep("NY", 169), rep("SF", 169), rep("CHI", 169), 
                     rep("PHI", 169), rep("LA", 169), rep("MI", 169),
                     rep("PNX", 169), rep("SEA", 169))

#NY.date = uber_dummy("05/01/11", "9/28/03", nrow(data.flu))
#SF.date = uber_dummy("07/1/10", "9/28/03", nrow(data.flu))
#CH.date = uber_dummy("04/23/13", "9/28/03", nrow(data.flu))
#uber_dummy("2011-05", "2004-01", nrow(flu_trends.flu))
NY.datex = uber_dummy("2013-09", "2004-01", nrow(flu_trends.flu))
SF.datex = uber_dummy("2013-01", "2004-01", nrow(flu_trends.flu))
CH.datex = uber_dummy("2013-04", "2004-01", nrow(flu_trends.flu))
AUS.datex = uber_dummy("2014-04", "2004-01", nrow(flu_trends.flu))
PHI.datex = uber_dummy("2014-04", "2004-01", nrow(flu_trends.flu))
LA.datex = uber_dummy("2013-06", "2004-01", nrow(flu_trends.flu))
MI.datex = uber_dummy("2014-06", "2004-01", nrow(flu_trends.flu))
PNX.datex = uber_dummy("2013-10", "2004-01", nrow(flu_trends.flu))
SEA.datex = uber_dummy("2013-04", "2004-01", nrow(flu_trends.flu))

NY.datepool = uber_dummy("2015-01", "2004-01", nrow(flu_trends.flu))
SF.datepool = uber_dummy("2014-08", "2004-01", nrow(flu_trends.flu))
CH.datepool = uber_dummy("2015-12", "2004-01", nrow(flu_trends.flu))
AUS.datepool = uber_dummy("2015-03", "2004-01", nrow(flu_trends.flu))
PHI.datepool = uber_dummy("2016-03", "2004-01", nrow(flu_trends.flu))
LA.datepool = uber_dummy("2015-03", "2004-01", nrow(flu_trends.flu))
MI.datepool = uber_dummy("2015-12", "2004-01", nrow(flu_trends.flu))
PNX.datepool = c(rep(0,169))
SEA.datepool = uber_dummy("2016-05", "2004-01", nrow(flu_trends.flu))



matrix <- data.frame(y = flu_trends.y,
                     month = as.factor(flu_trends.month),
                     year = as.factor(flu_trends.year),
                     uberx = as.factor(flu_trends.uberx),
                     uberpool = as.factor(flu_trends.uberpool),
                     city = as.factor(flu_trends.city),
                     max_temp = flu_trends.maxtemp,
                     min_temp = flu_trends.mintemp,
                     prcp = flu_trends.prcp,
                     snow = flu_trends.snow)

# matrix <- data.frame(y = data[-1],
#                      week = as.factor(week[-1]),
#                      year = as.factor(year[-1]),
#                      previous = data[-length(data)],
#                      uber = uber[-1]
#                      )


model <- lm(log(y) ~ 0 + city + year + month + uberpool + uberx + 
              month:uberpool + month:uberx + max_temp + min_temp + prcp + snow
              + min_temp:uberpool + prcp:uberpool + snow:uberpool + min_temp:uberx
            + min_temp:uberx + prcp:uberx + snow:uberx + lag(y,1), data = matrix)
summary(model)
smmr_1 <-summary(model)
paste("R-squared: ",
      round(smmr_1$r.squared, 3),
      ", p-value of F test: ",
      1-pf(smmr_1$fstatistic[1], smmr_1$fstatistic[2], smmr_1$fstatistic[3]))

