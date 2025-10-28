# ------------------------------------------------------------------------------
# Master thesis
# Author: Elia Scapini
# Date: 21/09/2025
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Connecting GitHub
# ------------------------------------------------------------------------------


# library(usethis)
# use_git()
# use_github()


# ------------------------------------------------------------------------------
# Load packages and functions
# ------------------------------------------------------------------------------


library(stargazer)
library(tsbox)
library(xts)
library(ggplot2)
library(CADFtest)
library(reshape2) 
library(lubridate)
library(writexl)
library(forecast)
library(dplyr)
library(sandwich)
library(broom)

# Delete all objects in the memory
rm(list=ls())

# Set Working Directory
setwd("C:/Users/elias/Documents/Personale/UNINE/Master_Applied_Economics/Master_Thesis")

# Load user-defined commands and packages
source("UserPackages.R")


# ------------------------------------------------------------------------------
# Import data and daily data set
# ------------------------------------------------------------------------------


# Import data frames
df_meteo <- read.csv("Data/Estonia_meteo.csv", header = T, sep = ",")
df_prod <- read.csv("Data/Actual Generation per Production Type_202401010000-202501010000.csv", header = T, sep = ",")
df_load <- read.csv("Data/Total Load - Day Ahead _ Actual_202401010000-202501010000.csv", header = T, sep = ",")

df_meteo$DateTime <- as.POSIXct(df_meteo$DateTime, format = "%Y-%m-%dT%H:%M", tz = "UTC")
df_prod$DateTime <- as.POSIXct(df_prod$DateTime, format = "%d.%m.%Y %H:%M", tz = "UTC")
df_load$DateTime <- as.POSIXct(df_load$DateTime, format = "%d.%m.%Y %H:%M", tz = "UTC")


# ------------------------------------------------------------------------------
# Modify and aggregate data sets
# ------------------------------------------------------------------------------


# Degree celsius to Kelvin
df_meteo$Air_temperature <- df_meteo$Air_temperature + 273.15

# Remove negative values for Irradiation
df_meteo$H_avg_radiation[df_meteo$H_avg_radiation < 0] <- 0

# Spatial aggregate panel across weather stations
estonia_h <- df_meteo %>%
  group_by(DateTime) %>%
  summarise(
    irradiation = mean(H_avg_radiation, na.rm = T),
    air_temp = mean(Air_temperature, na.rm = T),
    wind_speed = mean(X10min_avg_wind_speed, na.rm = T),
    max_wind_speed = mean(Hourly_max_wind_speed, na.rm = T)
  ) %>%
  arrange(DateTime)

# Temporal aggregation across hours
estonia_d <- estonia_h %>%
  group_by(date(DateTime)) %>%
  summarise(irrad = sum(irradiation, na.rm = T),
            speed = mean(wind_speed, na.rm = T),
            temp = mean(air_temp, na.rm = T)
  )

# Modify and reduce time span df_prod and create daily cable variable
df_prod <- df_prod %>%
  select(-c(1,2,4,5,6,7,8,9,10,11,13))

names(df_prod) <- c("DateTime", "Solar", "Wind")

df_prod <- df_prod %>%
  mutate(Solar = as.double(Solar), Wind = as.double(Wind))

prod_d <- df_prod %>%
  group_by(date(DateTime)) %>%
  summarise(solar = sum(Solar, na.rm = T), wind = sum(Wind, na.rm = T))

# Modify and reduce time span df_load
df_load <- df_load %>%
  select(-c(1,3))

names(df_load) <- c("DateTime", "Act_load")

df_load <- df_load %>%
  mutate(Act_load = as.double(Act_load))

load_d <- df_load %>%
  group_by(date(DateTime)) %>%
  summarise(act_load = sum(Act_load, na.rm = T))

# Check NAs
which(is.na(prod_d$solar))
which(is.na(prod_d$wind))
which(is.na(load_d$Act_load))
which(is.na(estonia_d$irrad))
which(is.na(estonia_d$speed))
which(is.na(estonia_d$temp))

rm(df_load, df_meteo, df_prod, estonia_h)


# ------------------------------------------------------------------------------
# Create time data frame to address seasonality and cable dummy
# ------------------------------------------------------------------------------


# Fault on 26th at 00:10 eet (25th at 22:10 UTC), so 25th operational and on 26th out of service
# Back in service on 04-09-2014 at 01:00 UTC, so from 4th onward
# And 25-12-2014 at 13:00 UTC, so from 26th onward

time <- data.frame(DateTime = prod_d$`date(DateTime)`)

init_date <- time$DateTime[1]

time <- time %>%
  mutate(
    weekday = weekdays(as.POSIXct(DateTime, format = "%d-%m-%Y %H:%M:%S", tz = "UTC")),
    hol = as.numeric(DateTime == "2024-01-01" |
                       DateTime == "2024-03-29" |
                       DateTime == "2024-05-01" |
                       DateTime == "2024-06-24" |
                       DateTime == "2024-08-20" ),
    cable = 1 - as.numeric(between(DateTime, as.Date("2024-01-26"), as.Date("2024-09-03")) |
                             between(DateTime, as.Date("2024-12-26"), as.Date("2024-12-31"))),
    mon = as.numeric(weekday == "Monday"),
    tue = as.numeric(weekday == "Tuesday"),
    wed = as.numeric(weekday == "Wednesday"),
    thu = as.numeric(weekday == "Thursday"),
    fri = as.numeric(weekday == "Friday"),
    sat = as.numeric(weekday == "Saturday"),
    sun = as.numeric(weekday == "Sunday"),
    jan = as.numeric(month(DateTime) == 1),
    feb = as.numeric(month(DateTime) == 2),
    mar = as.numeric(month(DateTime) == 3),
    apr = as.numeric(month(DateTime) == 4),
    may = as.numeric(month(DateTime) == 5),
    jun = as.numeric(month(DateTime) == 6),
    jul = as.numeric(month(DateTime) == 7),
    aug = as.numeric(month(DateTime) == 8),
    sep = as.numeric(month(DateTime) == 9),
    oct = as.numeric(month(DateTime) == 10),
    nov = as.numeric(month(DateTime) == 11),
    dec = as.numeric(month(DateTime) == 12)
  )

# Remove first row since regression is in growth rates
time <- tail(time, -1)


# ------------------------------------------------------------------------------
# Creating TS for stationarity test
# ------------------------------------------------------------------------------


solar_ts <- xts(prod_d$solar, order.by = prod_d$`date(DateTime)`)
wind_ts <- xts(prod_d$wind, order.by = prod_d$`date(DateTime)`)
load_ts <- xts(load_d$act_load, order.by = load_d$`date(DateTime)`)
irrad_ts <- xts(estonia_d$irrad, order.by = estonia_d$`date(DateTime)`)
speed_ts <- xts(estonia_d$speed, order.by = estonia_d$`date(DateTime)`)
temp_ts <- xts(estonia_d$temp, order.by = estonia_d$`date(DateTime)`)


# ------------------------------------------------------------------------------
# Plotting and data transformation
# ------------------------------------------------------------------------------


# Plotting the TS
ts_plot(
  solar_ts,
  title = "Daily solar production",
  subtitle = "MW - No transformation"
)
# Discussion: Solar bell shape

ts_plot(
  wind_ts,
  title = "Daily wind production",
  subtitle = "MW - No transformation"
)
# Discussion: Higher in winter

ts_plot(
  load_ts,
  title = "Daily load",
  subtitle = "MW - No transformation"
)
# Discussion: Higher in winter

ts_plot(
  irrad_ts,
  title = "Daily irradiation",
  subtitle = "W/m2 - (mean over hours)"
)
# Discussion: Higher in summer

ts_plot(
  speed_ts,
  title = "Daily wind speed",
  subtitle = "m/s - (mean over hours)"
)
# Discussion: Higher in winter

ts_plot(
  temp_ts,
  title = "Daily temperature",
  subtitle = "MW - (mean over hours)"
)
# Discussion: Higher in summer

# Calculating growth rates, since more likely to be CSP
gr_solar <- tail(diff(log(solar_ts)), -1)
gr_wind <- tail(diff(log(wind_ts)), -1)
gr_load <- tail(diff(log(load_ts)), -1)
gr_irrad <- tail(diff(log(irrad_ts)), -1)
gr_speed <- tail(diff(log(speed_ts)), -1)
gr_temp <- tail(diff(log(temp_ts)), -1)

# Check AutoCorrelation Function
plotACF(gr_solar, lag.max = 20)
plotACF(gr_wind, lag.max = 20)
plotACF(gr_load, lag.max = 20)
plotACF(gr_irrad, lag.max = 20)
plotACF(gr_speed, lag.max = 20)
plotACF(gr_temp, lag.max = 20)
# Discussion : No trend, just seasonality

# Check unit root process
ur_sol_drift = CADFtest(gr_solar, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_sol_drift)
# Discussion: No unit root

ur_wind_drift = CADFtest(gr_wind, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_wind_drift)
# Discussion: No unit root

ur_load_drift = CADFtest(gr_load, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_load_drift)
# Discussion: No unit root

ur_irrad_drift = CADFtest(gr_irrad, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_irrad_drift)
# Discussion: No unit root

ur_speed_drift = CADFtest(gr_speed, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_speed_drift)
# Discussion: No unit root

ur_temp_trend = CADFtest(gr_temp, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_temp_trend)
# Discussion: No unit root

rm(ur_irrad_drift, ur_load_drift, ur_sol_drift, ur_speed_drift, ur_temp_trend, ur_wind_drift)


# ------------------------------------------------------------------------------
# Regression wind - speed
# ------------------------------------------------------------------------------


reg_wind <- lm(gr_wind[,1] ~ gr_speed[,1] + cable  + 
                 mon + tue + thu + fri + sat + sun + jan + feb + mar + may + jun + jul +
                 aug + sep + oct + nov + dec, data = time)
summary(reg_wind)

# Discussion : Cable effect seems irrelevant for wind power

checkresiduals(reg_wind)
# Discussion : Weak autocorrelation in the residuals


# ------------------------------------------------------------------------------
# Regression solar - irradiation
# ------------------------------------------------------------------------------


reg_sol <- lm(gr_solar[,1] ~ gr_irrad[,1] + cable +
                mon + tue + thu + fri + sat + sun + jan + feb + mar + may + jun + jul +
                aug + sep + oct + nov + dec, data = time)
summary(reg_sol)
# Discussion : Cable effect is not significant to affect solar production. Negative coef nevertheless

checkresiduals(reg_sol)
# Discussion : Weak autocorrelation in the residuals


# ------------------------------------------------------------------------------
# Regression load - temperature
# ------------------------------------------------------------------------------


reg_load <- lm(gr_load[,1] ~ gr_temp[,1] + cable + 
                 mon + tue + thu + fri + sat + sun + jan + feb + mar + may + jun + jul +
                 aug + sep + oct + nov + dec, data = time)
summary(reg_load)
# Discussion : Cable effect is not significant to affect production. Endogeneity seems excluded

checkresiduals(reg_load)
# Discussion : Weak autocorrelation in the residuals


# ------------------------------------------------------------------------------
# Export latex table
# ------------------------------------------------------------------------------


names(reg_sol$coefficients) <- c("(Intercept)", "irradiation", "cable", "mon", "tue", "thu", "fri", "sat", "sun",
                                 "jan", "feb", "mar", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
names(reg_wind$coefficients) <- c("(Intercept)", "speed", "cable", "mon", "tue", "thu", "fri", "sat", "sun",
                                 "jan", "feb", "mar", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
names(reg_load$coefficients) <- c("(Intercept)", "temperature", "cable", "mon", "tue", "thu", "fri", "sat", "sun",
                                 "jan", "feb", "mar", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")

stargazer(
  reg_sol, reg_wind, reg_load,
  title = "regession",
  style = "default",
  type = "latex",
  keep = c("cable", "irradiation", "speed", "temperature"),
  no.space = T
)


# ------------------------------------------------------------------------------
# Appendix
# ------------------------------------------------------------------------------

# # transform into time series
# solar_h <- xts(as.double(df_prod$Solar...Actual.Aggregated..MW.), order.by = df_prod$DateTime)
# wind_h <- xts(as.double(df_prod$Wind.Onshore...Actual.Aggregated..MW.), order.by = df_prod$DateTime)
# load_h <- xts(as.double(df_load$Actual.Total.Load..MW....BZN.EE), order.by = df_load$DateTime)
# cable_h <- xts(as.double(df_prod$cable), order.by = df_prod$DateTime)
# irrad_h <- xts(as.double(estonia_h$irradiation), order.by = estonia_h$DateTime)
# speed_h <- xts(as.double(estonia_h$wind_speed), order.by = estonia_h$DateTime)
# temp_h <- xts(as.double(estonia_h$air_temp), order.by = estonia_h$DateTime)

# # Reduce time span to one week where cable fault
# start <- as.POSIXct("2024-01-22 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# end <- as.POSIXct("2024-02-02 23:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# 
# solar_r <- ts_span(solar_h, start = start, end = end)
# wind_r <- ts_span(wind_h, start, end)
# load_r <- ts_span(load_h, start, end)
# irrad_r <- ts_span(irrad_h, start, end)
# speed_r <- ts_span(speed_h, start, end)
# temp_r <- ts_span(temp_h, start, end)
# 
# 
# solar_d <- apply.daily(solar_h, sum)
# wind_d <- apply.daily(wind_h, sum)
# load_d <- apply.daily(load_h, sum)
# cable_d <- apply.daily(cable_h, min)
# irrad_d <- apply.daily(irrad_h, mean)
# speed_d <- apply.daily(speed_h, mean)
# temp_d <- apply.daily(temp_h, mean)