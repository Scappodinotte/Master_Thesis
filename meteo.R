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

# # Reduce time span
# df_prod <- df_prod %>%
#   filter(DateTime < as.POSIXct("2024-02-21 00:00:00", format = "%Y-%m-%d %H:%M", tz = "UTC"))
# df_load <- df_load %>%
#   filter(DateTime < as.POSIXct("2024-02-21 00:00:00", format = "%Y-%m-%d %H:%M", tz = "UTC"))

# Degree celsius to Kelvin
df_meteo$Air_temperature <- df_meteo$Air_temperature + 273.15

# Remove negative values for Irradiation
df_meteo$H_avg_radiation[df_meteo$H_avg_radiation < 0] <- 0

# Aggregate panel
estonia_h <- df_meteo %>%
  group_by(DateTime) %>%
  summarise(
    irradiation = mean(H_avg_radiation, na.rm = T),
    air_temp = mean(Air_temperature, na.rm = T),
    wind_speed = mean(X10min_avg_wind_speed, na.rm = T),
    max_wind_speed = mean(Hourly_max_wind_speed, na.rm = T)
  ) %>%
  arrange(DateTime)


# transform into time series
solar_h <- xts(as.double(df_prod$Solar...Actual.Aggregated..MW.), order.by = df_prod$DateTime)
wind_h <- xts(as.double(df_prod$Wind.Onshore...Actual.Aggregated..MW.), order.by = df_prod$DateTime)
load_h <- xts(as.double(df_load$Actual.Total.Load..MW....BZN.EE), order.by = df_load$DateTime)
irrad_h <- xts(as.double(estonia_h$irradiation), order.by = estonia_h$DateTime)
speed_h <- xts(as.double(estonia_h$wind_speed), order.by = estonia_h$DateTime)
temp_h <- xts(as.double(estonia_h$air_temp), order.by = estonia_h$DateTime)

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


# Aggregate to daily frequency
solar_d <- apply.daily(solar_h, sum)
wind_d <- apply.daily(wind_h, sum)
load_d <- apply.daily(load_h, sum)
irrad_d <- apply.daily(irrad_h, mean)
speed_d <- apply.daily(speed_h, mean)
temp_d <- apply.daily(temp_h, mean)

# 
# ratio_s_i <- solar_d/irrad_d
# 
# ts_plot(
#   ratio_s_i,
#   title = "Hourly solar production",
#   subtitle = "MW - No transformation"
# )
# 
# ratio_w_s <- wind_r/speed_r
# 
# ts_plot(
#   ratio_w_s,
#   title = "Hourly solar production",
#   subtitle = "MW - No transformation"
# )

# Create daily cable variable
# Fault on 26th at 00:10 eet (22:10 UTC), so 25th operational and on 26th our of service
cable <- data.frame(date = index(wind_d), cable = c(rep(1, 25), rep(0, 341)))
# cable_placebo <- data.frame(date = index(wind_d), cable = c(rep(1, 24), rep(0, 17)))

time <- data.frame(DateTime = index(wind_d))

# Create time data frame to address seasonality
init_date <- index(wind_d)[1]

time <- time %>%
  mutate(
    weekday = weekdays(as.POSIXct(DateTime, format = "%d-%m-%Y %H:%M:%S", tz = "UTC")),
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
cable <- tail(cable, -1)

rm(df_load, df_meteo, df_prod, df_meteo, estonia_h, irrad_h, speed_h, temp_h, solar_h, wind_h, load_h)

# ------------------------------------------------------------------------------
# Plotting and data transformation
# ------------------------------------------------------------------------------

# Plotting the TS

ts_plot(
  solar_d,
  title = "Daily solar production",
  subtitle = "MW - No transformation"
)
# Discussion: Solar bell shape

gr_solar <- tail(diff(log(solar_d)), -1)

ts_plot(
  gr_solar,
  title = "Change in daily solar production",
  subtitle = "No unit - No transformation"
)
# Discussion: No more trend but seasonality

ts_plot(
  wind_d,
  title = "Daily wind production",
  subtitle = "MW - No transformation"
)
# Discussion: Higher in winter

gr_wind <- tail(diff(log(wind_d)), -1)

ts_plot(
  gr_wind,
  title = "Change in daily wind production",
  subtitle = "No unit - No transformation"
)
# Discussion: No more trend but seasonality

ts_plot(
  load_d,
  title = "Daily load",
  subtitle = "MW - No transformation"
)
# Discussion: Higher in winter

ts_plot(
  irrad_d,
  title = "Daily irradiation",
  subtitle = "W/m2 - (mean over hours)"
)
# Discussion: Higher in summer

ts_plot(
  speed_d,
  title = "Daily wind speed",
  subtitle = "m/s - (mean over hours)"
)
# Discussion: Higher in winter

ts_plot(
  temp_d,
  title = "Daily temperature",
  subtitle = "MW - (mean over hours)"
)
# Discussion: Higher in summer

gr_load <- tail(diff(log(load_d)), -1)
gr_irrad <- tail(diff(log(irrad_d)), -1)
gr_speed <- tail(diff(log(speed_d)), -1)
gr_temp <- tail(diff(log(temp_d)), -1)



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

reg_wind <- lm(gr_wind[,1] ~ gr_speed[,1] + cable[,2]  + 
                 mon + tue + thu + fri + sat + sun + jan + feb + mar + may + jun + jul +
                 aug + sep + oct + nov + dec, data = time)
summary(reg_wind)

# Discussion : Cable effect seems irrelevant for wind power

checkresiduals(reg_wind)
# Discussion : Weak autocorrelation in the residuals

# ------------------------------------------------------------------------------
# Regression solar - irradiation
# ------------------------------------------------------------------------------
# Hourly data regression
reg_sol <- lm(gr_solar[,1] ~ gr_irrad[,1] + cable[,2] +
                mon + tue + thu + fri + sat + sun + jan + feb + mar + may + jun + jul +
                aug + sep + oct + nov + dec, data = time)
summary(reg_sol)
# Discussion : Cable effect is not significant to affect solar production. Negative coef nevertheless

checkresiduals(reg_sol)
# Discussion : Weak autocorrelation in the residuals

# ------------------------------------------------------------------------------
# Regression load - temperature
# ------------------------------------------------------------------------------

reg_load <- lm(gr_load[,1] ~ gr_temp[,1] + cable[,2] + 
                 mon + tue + thu + fri + sat + sun + jan + feb + mar + may + jun + jul +
                 aug + sep + oct + nov + dec, data = time)
summary(reg_load)
# Discussion : Cable effect is not significant to affect production. Endogeneity seems excluded

checkresiduals(reg_load)
# Discussion : Weak autocorrelation in the residuals


# ------------------------------------------------------------------------------
# Appendix
# ------------------------------------------------------------------------------
