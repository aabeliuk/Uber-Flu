require(forecast)
library(forecast)
library(lubridate)
library(tidyr)
library(dplyr)
library(magrittr)

library(zoo)
library(dyn)

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")
uber_dummy <- function(date, ini_date, n){
  date = as.Date(paste(date,1,sep="-"),"%Y-%m-%d")
  date.ini = as.Date(paste(ini_date,1,sep="-"), format="%Y-%m-%d")
  #date = as.Date(c(date), format="%Y-%m")
  #date.ini = as.Date(c(ini_date), format="%Y-%m")

  
  m = as.integer(format(date, "%m"))
  y = as.integer(format(date, "%Y"))
  ini = as.integer(format(date.ini, "%Y"))
  start = m+12*(y-ini)
  #sprintf("%s, %s, %s yields %s", w, y, ini, start)
  #return()
  return(c(rep(0,start), rep(1,(n-start))))
}

uber_dummy_interrupted <- function(date1,date2, ini_date, n){
  date1 = as.Date(paste(date1,1,sep="-"),"%Y-%m-%d")
  date2 = as.Date(paste(date2,1,sep="-"),"%Y-%m-%d")
  date.ini = as.Date(paste(ini_date,1,sep="-"), format="%Y-%m-%d")
  #date = as.Date(c(date), format="%Y-%m")
  #date.ini = as.Date(c(ini_date), format="%Y-%m")
  
  
  m1 = as.integer(format(date1, "%m"))
  y1 = as.integer(format(date1, "%Y"))
  m2 = as.integer(format(date2, "%m"))
  y2 = as.integer(format(date2, "%Y"))
  ini = as.integer(format(date.ini, "%Y"))
  start = m1+12*(y1-ini)
  stop = m2-m1+12*(y2-y1)
  #sprintf("%s, %s, %s yields %s", w, y, ini, start)
  #return()
  return(c(rep(0,start),rep(1,stop), rep(0,(n-(start+stop) ))))
}

data.flu = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/data_flu.csv')
flu_trends.flu = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/cold_and_flu.csv')
flu_trends.maxtemp = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/max_temp_monthly.csv')
flu_trends.mintemp = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/min_temp_monthly.csv')
flu_trends.prcp = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/prcp_monthly.csv')
flu_trends.snow = read.csv('/Users/Bartley/Documents/USC/dev/repos/Uber-Flu/data/snow_monthly.csv')


#Linear regresion 
month = as.integer(format(as.Date(paste(as.character(flu_trends.flu[,1]),1,sep="-"), "%Y-%m-%d"), "%m"))
year = as.integer(format(as.Date(paste(as.character(flu_trends.flu[,1]),1,sep="-"), "%Y-%m-%d"), "%Y"))

NY.datex = uber_dummy("2013-09", "2004-01", nrow(flu_trends.flu))
SF.datex = uber_dummy("2013-01", "2004-01", nrow(flu_trends.flu))
CH.datex = uber_dummy("2013-04", "2004-01", nrow(flu_trends.flu))
AUS.datex = uber_dummy("2014-04", "2004-01", nrow(flu_trends.flu))-uber_dummy_interrupted("2016-5","2017-5", "2004-01", nrow(flu_trends.flu))
PHI.datex = uber_dummy("2014-04", "2004-01", nrow(flu_trends.flu))
LA.datex = uber_dummy("2013-06", "2004-01", nrow(flu_trends.flu))
MI.datex = uber_dummy("2014-06", "2004-01", nrow(flu_trends.flu))
PNX.datex = uber_dummy("2013-10", "2004-01", nrow(flu_trends.flu))
SEA.datex = uber_dummy("2013-04", "2004-01", nrow(flu_trends.flu))
BU.datex = c(rep(0,169))
HO.datex = uber_dummy("2014-04", "2004-01", nrow(flu_trends.flu))
BO.datex = uber_dummy("2013-08", "2004-01", nrow(flu_trends.flu))

NY.datepool = uber_dummy("2015-01", "2004-01", nrow(flu_trends.flu))
SF.datepool = uber_dummy("2014-08", "2004-01", nrow(flu_trends.flu))
CH.datepool = uber_dummy("2015-12", "2004-01", nrow(flu_trends.flu))
AUS.datepool = uber_dummy("2015-03", "2004-01", nrow(flu_trends.flu)) - uber_dummy_interrupted("2016-5","2017-5", "2004-01", nrow(flu_trends.flu))
PHI.datepool = uber_dummy("2016-03", "2004-01", nrow(flu_trends.flu))
LA.datepool = uber_dummy("2015-03", "2004-01", nrow(flu_trends.flu))
MI.datepool = uber_dummy("2015-12", "2004-01", nrow(flu_trends.flu))
PNX.datepool = c(rep(0,169))
SEA.datepool = uber_dummy("2016-05", "2004-01", nrow(flu_trends.flu))
BU.datepool = c(rep(0,169))
HO.datepool = uber_dummy("2015-03", "2004-01", nrow(flu_trends.flu))
BO.datepool = uber_dummy("2015-08", "2004-01", nrow(flu_trends.flu))

# nathan funtions

pre_data <- function(flu_trends, city_name, city_var){
  y = flu_trends.y[min(which(flu_trends.city == city_name)):
                     (min(which(flu_trends.city == city_name))+min(which(city_var == 1)) - 1)]
  
  month = as.factor(flu_trends.month[min(which(flu_trends.city == city_name)):
                                   (min(which(flu_trends.city == city_name))+min(which(city_var == 1)) - 1)])
  
  year = as.factor(flu_trends.year[min(which(flu_trends.city == city_name)):
                                     (min(which(flu_trends.city == city_name))+min(which(city_var == 1)) - 1)])
  
  city = as.factor(flu_trends.city[min(which(flu_trends.city == city_name)):
                                     (min(which(flu_trends.city == city_name))+min(which(city_var == 1)) - 1)])
  
  matrix <- data.frame(y = y, month = month, year = year, city = city)
  return(matrix)
}

full_data <- function(flu_trends, city_name, city_var){
    y = flu_trends.y[min(which(flu_trends.city == city_name)):
                       (min(which(flu_trends.city == city_name)) + 168)]
    
    month = as.factor(flu_trends.month[min(which(flu_trends.city == city_name)):
                                         (min(which(flu_trends.city == city_name)) + 168)])
    
    year = as.factor(flu_trends.year[min(which(flu_trends.city == city_name)):
                                       (min(which(flu_trends.city == city_name)) + 168)])
    
    city = as.factor(flu_trends.city[min(which(flu_trends.city == city_name)):
                                       (min(which(flu_trends.city == city_name))+ 168)])
    
    matrix <- data.frame(y = y, month = month, year = year, city = city)
    return(matrix)
                      
}

pre_pool_data <- function(flu_trends, city_name, city_var){
  y = flu_trends.y[min(which(flu_trends.city == city_name)):
                     (min(which(flu_trends.city == city_name))+min(which(city_var == 1)) - 1)]
  
  month = as.factor(flu_trends.month[min(which(flu_trends.city == city_name)):
                                       (min(which(flu_trends.city == city_name))+min(which(city_var == 1)) - 1)])
  
  year = as.factor(flu_trends.year[min(which(flu_trends.city == city_name)):
                                     (min(which(flu_trends.city == city_name))+min(which(city_var == 1)) - 1)])
  
  city = as.factor(flu_trends.city[min(which(flu_trends.city == city_name)):
                                     (min(which(flu_trends.city == city_name))+min(which(city_var == 1)) - 1)])
  
  matrix <- data.frame(y = y, month = month, year = year, city = city)
  return(matrix)
}

post_uber_data <- function(flu_trends, city_name, city_var){
  y = flu_trends.y[min(which(flu_trends.city == city_name))+min(which(city_var == 1)):
                     (max(which(flu_trends.city == city_name)) - 1)]
  
  month = as.factor(flu_trends.month[min(which(flu_trends.city == city_name))+min(which(city_var == 1)):
                                       (max(which(flu_trends.city == city_name)) - 1)])
  
  year = as.factor(flu_trends.year[min(which(flu_trends.city == city_name))+min(which(city_var == 1)):
                                     (max(which(flu_trends.city == city_name)) - 1)])
  
  city = as.factor(flu_trends.city[min(which(flu_trends.city == city_name))+min(which(city_var == 1)):
                                     (max(which(flu_trends.city == city_name)) - 1)])
  
  matrix <- data.frame(y = y, month = month, year = year, city = city)
  return(matrix)
}

get_pre_post_period_dates <- function(flu_trends, city_var){
  
  #c1 = min(which(flu_trends.city == city_name))
  c1 = 1
  c2 = min(which(city_var == 1)) -1
  c3 = c2 + 1
  c4 = 169
  #c2 = min(which(flu_trends.city == city_name)) + min(which(city_var == 1)) -1
  #c3 = c2 + 1
  #c4 = max(which(flu_trends.city == city_name))
  
  return(c(c1,c2,c3,c4))
  
}

get_city_data <- function(flu_trends, flu_trends_city_name, normal_city_name) {
  
  maxtemp <- flu_trends.maxtemp[, normal_city_name]
  mintemp <- flu_trends.mintemp[, normal_city_name]
  prcp    <- flu_trends.prcp[, normal_city_name]
  snow    <- flu_trends.snow[, normal_city_name]
  flu     <- flu_trends.flu[, flu_trends_city_name]
  
  return(cbind(flu, snow, prcp, mintemp,maxtemp))
}

get_combined_city_data <- function( flu_trends, flu_trends_city_name, normal_city_name) {
  
  maxtemp <- as.double(flu_trends.maxtemp[, normal_city_name])
  maxtemp <- do.call(rbind, lapply(maxtemp, as.numeric))
  mintemp <- as.double(flu_trends.mintemp[, normal_city_name])
  mintemp <- do.call(rbind, lapply(mintemp, as.numeric))
  
  prcp    <- as.double(flu_trends.prcp[, normal_city_name])
  prcp <- do.call(rbind, lapply(prcp, as.numeric))
  
  snow    <- as.double(flu_trends.snow[, normal_city_name])
  snow <- do.call(rbind, lapply(snow, as.numeric))
  
  flu     <- as.double(flu_trends.flu[, flu_trends_city_name])
  flu <- do.call(rbind, lapply(flu, as.numeric))
  
  
  
  flu_2   <- double()
  flu_2_names <- vector()
  for (name in names(flu_trends.flu)){ 
    if (name == 'Month') {next;}
    if (name == flu_trends_city_name) {next;}
    flu_2 <- cbind(flu_2, flu_trends.flu[, name])
    flu_2_names <- c(flu_2_names, name)
    }
  #flu_1   <- flu_trends.flu[, city_name_1]
  #flu_2   <- flu_trends.flu[, city_name_2]
  #flu_3   <- flu_trends.flu[, city_name_3]
  
  ret_val = cbind(flu, snow, prcp, mintemp, maxtemp, flu_2)
  return(ret_val)
  print(length(flu_2_names))
  #names(ret_val) <- c("date", "Y", "SNOW", "PRCP", "MINTEMP", "MAXTEMP", flu_2_names)
  df = data.frame(ret_val)
  colnames(df) <- c("date", "Y", "SNOW", "PRCP", "MINTEMP", "MAXTEMP", flu_2_names)
  
  #names(df) = names(ret_val)
  return(df)
}

library(CausalImpact)
train_causal <- function(city_data, dates){
  model<- CausalImpact(city_data, c(dates[1], dates[2]), c(dates[3], dates[4]))
  return(model)
}

ny_city_data = get_city_data(flu_trends, 'Geo..New.York.NY', 'NewYork')
#ny_city_data = get_combined_city_data(flu_trends, 'Geo..New.York.NY', 'NewYork')
ny_dates = get_pre_post_period_dates(flu_trends, NY.datex)
ny_model = train_causal(ny_city_data, ny_dates)
sf_city_data = get_city_data(flu_trends, 'Geo..San.Francisco.Oakland.San.Jose.CA', 'SanFrancisco')
SF.datex = uber_dummy("2013-01", "2004-01", nrow(flu_trends.flu))
sf_dates = get_pre_post_period_dates(flu_trends, SF.datex)
sf_model = train_causal(sf_city_data, sf_dates)
chi_city_data = get_city_data( flu_trends, 'Geo..Chicago.IL', 'Chicago')
chi_dates = get_pre_post_period_dates(flu_trends, CH.datex)
chi_model = train_causal(chi_city_data, chi_dates)
phi_city_data = get_city_data( flu_trends, 'Geo..Philadelphia.PA', 'Philadelphia')
phi_dates = get_pre_post_period_dates(flu_trends, PHI.datex)
phi_model = train_causal(phi_city_data, phi_dates)
la_city_data = get_city_data(flu_trends,  'Geo..Los.Angeles.CA', 'LosAngeles')
LA.datex = uber_dummy("2013-06", "2004-01", nrow(flu_trends.flu))
la_dates = get_pre_post_period_dates(flu_trends, LA.datex)
la_model = train_causal(la_city_data, la_dates)

mi_city_data = get_city_data(flu_trends, 'Geo..Miami.Ft..Lauderdale.FL', 'Miami')
MI.datex = uber_dummy("2014-06", "2004-01", nrow(flu_trends.flu))
mi_dates = get_pre_post_period_dates(flu_trends, MI.datex)
mi_model = train_causal(mi_city_data, mi_dates)
#phx_city_data = get_city_data(flu_trends, 'Geo..Phoenix.AZ', 'Phoenix')
#phx_dates = get_pre_post_period_dates(flu_trends, PH)
sea_city_data = get_city_data(flu_trends, 'Geo..Seattle.Tacoma.WA', 'Seattle')
sea_dates = get_pre_post_period_dates(flu_trends, SEA.datex)
sea_model = train_causal(sea_city_data, sea_dates)

summary(ny_model)
summary(sf_model)
summary(chi_model)
summary(phi_model)
summary(la_model)
summary(mi_model)
summary(sea_model)


