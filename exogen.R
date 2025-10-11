# ------------------------------------------------------------------------------
# Master thesis
# Author: Elia Scapini
# Date: 21/09/2025
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Connecting GitHub
# ------------------------------------------------------------------------------
library(usethis)
# use_git()
use_github()

# ------------------------------------------------------------------------------
# Load packages and functions
# ------------------------------------------------------------------------------
library(tsbox)
library(xts)
library(ggplot2)   
library(seasonal)
library(CADFtest)
library(reshape2) 
library(lubridate)
library(writexl)
library(forecast)
library(dplyr)
library(sandwich)

# Delete all objects in the memory
rm(list=ls())

# Set Working Directory
setwd("C:/Users/elias/Documents/Personale/UNINE/Master_Applied_Economics/Sem3")

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

# Reduce time span
df_prod <- df_prod %>%
  filter(DateTime < as.POSIXct("2024-02-21 00:00:00", format = "%Y-%m-%d %H:%M", tz = "UTC"))
df_load <- df_load %>%
  filter(DateTime < as.POSIXct("2024-02-21 00:00:00", format = "%Y-%m-%d %H:%M", tz = "UTC"))

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
act_solar_h <- xts(as.double(df_prod$Solar...Actual.Aggregated..MW.), order.by = df_prod$DateTime)
act_wind_h <- xts(as.double(df_prod$Wind.Onshore...Actual.Aggregated..MW.), order.by = df_prod$DateTime)
act_load_h <- xts(as.double(df_load$Actual.Total.Load..MW....BZN.EE), order.by = df_load$DateTime)
irrad_h <- xts(as.double(estonia_h$irradiation), order.by = estonia_h$DateTime)
speed_h <- xts(as.double(estonia_h$wind_speed), order.by = estonia_h$DateTime)
temp_h <- xts(as.double(estonia_h$air_temp), order.by = estonia_h$DateTime)

# Aggregate to daily frequency
act_solar_d <- apply.daily(act_solar_h, sum)
act_wind_d <- apply.daily(act_wind_h, sum)
act_load_d <- apply.daily(act_load_h, sum)
irrad_d <- apply.daily(irrad_h, sum)
speed_d <- apply.daily(speed_h, mean)
temp_d <- apply.daily(temp_h, mean)

# Create daily cable variable
# Fault on 26th at 00:10 eet (22:10 UTC), so 25th operational and on 26th our of service
cable <- data.frame(date = index(act_wind_d), cable = c(rep(1, 25), rep(0, 26)))
cable_placebo <- data.frame(date = index(act_wind_d), cable = c(rep(1, 24), rep(0, 17)))


# ------------------------------------------------------------------------------
# Plotting and data transformation
# ------------------------------------------------------------------------------
# Plotting the TS
g <- ggplot(act_solar_d) + geom_line(aes(x = index(act_solar_d), y = act_solar_d)) + theme_minimal()
g <- g + xlab("Days") + ylab("Megawatt [MW]") + ggtitle("actual solar production Estonia", subtitle = "Days, not seasonally adjusted")
g
# Discussion: Upper trend visually spotted

g <- ggplot(act_wind_d) + geom_line(aes(x = index(act_wind_d), y = act_wind_d)) + theme_minimal()
g <- g + xlab("Days") + ylab("Megawatt [MW]") + ggtitle("actual wind production Estonia", subtitle = "Days, not seasonally adjusted")
g
# Discussion: No apparent trend visually spotted

g <- ggplot(act_load_d) + geom_line(aes(x = index(act_load_d), y = act_load_d)) + theme_minimal()
g <- g + xlab("Days") + ylab("Megawatt [MW]") + ggtitle("actual load Estonia", subtitle = "Days, not seasonally adjusted")
g
# Discussion: Downward trend visually spotted

g <- ggplot(irrad_d) + geom_line(aes(x = index(irrad_d), y = irrad_d)) + theme_minimal()
g <- g + xlab("Days") + ylab("Megawatt [MW]") + ggtitle("Irradiation W/m2", subtitle = "Days, not seasonally adjusted")
g
# Discussion: Upward trend visually spotted

g <- ggplot(speed_d) + geom_line(aes(x = index(speed_d), y = speed_d)) + theme_minimal()
g <- g + xlab("Days") + ylab("Megawatt [MW]") + ggtitle("Wind speed m/s", subtitle = "Days, not seasonally adjusted")
g
# Discussion: No trend visually spotted

g <- ggplot(temp_d) + geom_line(aes(x = index(temp_d), y = temp_d)) + theme_minimal()
g <- g + xlab("Days") + ylab("Megawatt [MW]") + ggtitle("Temperature C", subtitle = "Days, not seasonally adjusted")
g
# Discussion: Upward trend visually spotted

# Check AutoCorrelation Function
plotACF(act_solar_d, lag.max = 20)
plotACF(act_wind_d, lag.max = 20)
plotACF(act_load_d, lag.max = 20)
plotACF(irrad_d, lag.max = 20)
plotACF(speed_d, lag.max = 20)
plotACF(temp_d, lag.max = 20)

# Discussion : Solar and irradiation show a small trend and seasonality each 6 days.
# Wind has neither and load and temperature have small trend.

# Adjust seasonality Solar and Irrad


# Check unit root process
# ur_act_sol_drift = CADFtest(act_solar_d, max.lag.y = 10, type = "drift", criterion = "BIC")
# summary(ur_act_sol_drift)
# # Discussion: the Non-stationnarity is rejected at 5% (p-value = 0.04)

ur_act_sol_trend = CADFtest(act_solar_d, max.lag.y = 10, type = "trend", criterion = "BIC")
summary(ur_act_sol_trend)
# Discussion: the Non-stationnarity is rejected at 5% (p-value = 0.00). Solar production seems to have a small trend

ur_act_wind_drift = CADFtest(act_wind_d, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_act_wind_drift)
# Discussion: the TS is CSP


# ur_act_load_drift = CADFtest(act_load_d, max.lag.y = 10, type = "drift", criterion = "BIC")
# summary(ur_act_load_drift)
# # Discussion: the TS is not CSP

ur_act_load_trend = CADFtest(act_load_d, max.lag.y = 10, type = "trend", criterion = "BIC")
summary(ur_act_load_trend)
# Discussion: the TS is not trend CSP but better, so take log difference

ur_act_load_trend_1 = CADFtest(ts_diff(log(act_load_d)), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(ur_act_load_trend_1)
# Discussion: the TS is now CSP

# ur_irrad_drift = CADFtest(irrad_d, max.lag.y = 10, type = "drift", criterion = "BIC")
# summary(ur_irrad_drift)
# # Discussion: the TS is not CSP

ur_irrad_trend = CADFtest(irrad_d, max.lag.y = 10, type = "trend", criterion = "BIC")
summary(ur_irrad_trend)
# Discussion: the TS is trend CSP at 10% level

ur_irrad_trend_1 = CADFtest(ts_diff(log(irrad_d)), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(ur_irrad_trend_1)
# Discussion: the TS is now CSP

ur_speed_drift = CADFtest(speed_d, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_speed_drift)
# Discussion: the TS is CSP


# ur_temp_drift = CADFtest(temp_d, max.lag.y = 10, type = "drift", criterion = "BIC")
# summary(ur_temp_drift)
# # Discussion: the TS is not CSP

ur_temp_trend = CADFtest(temp_d, max.lag.y = 10, type = "trend", criterion = "BIC")
summary(ur_temp_trend)
# Discussion: the TS is not trend CSP

ur_temp_trend_1 = CADFtest(ts_diff(log(temp_d)), max.lag.y = 10, type = "trend", criterion = "BIC")
summary(ur_temp_trend_1)
# Discussion: the TS is not trend CSP

# Transformation
# Load irrad and temp should be transformed into growth rate to remove the trend
gr_act_load_d <- ts_diff(log(act_load_d))
gr_act_solar_d <- ts_diff(log(act_solar_d))
gr_irrad_d <- ts_diff(log(irrad_d))
gr_temp_d <- ts_diff(log(temp_d))

# Actual solar is trend stationary and Wind prod and speed are CSP

# Check AutoCorrelation Function
plotACF(gr_act_solar_d, lag.max = 35)
plotACF(act_wind_d, lag.max = 35)
plotACF(gr_act_load_d, lag.max = 35)
plotACF(gr_irrad_d, lag.max = 35)
plotACF(speed_d, lag.max = 35)
plotACF(gr_temp_d, lag.max = 35)

# ------------------------------------------------------------------------------
# Regression wind - speed
# ------------------------------------------------------------------------------
# Daily data regression
reg_wind <- lm(act_wind_d[,1] ~ speed_d[,1] + cable[,2])
summary(reg_wind)

# Discussion : Cable effect seems relevant for wind power

checkresiduals(reg_wind)
# Discussion : No autocorrelation in the residuals

# ------------------------------------------------------------------------------
# Regression solar - irradiation
# ------------------------------------------------------------------------------
# Hourly data regression
reg_sol <- lm(gr_act_solar_d[,1] ~ gr_irrad_d[,1] + cable[,2])
summary(reg_sol)
# Discussion : Cable effect is not significant to affect production. Endogeneity seems excluded

checkresiduals(reg_sol)
# Discussion : No autocorrelation in the residuals

# ------------------------------------------------------------------------------
# Regression load - temperature
# ------------------------------------------------------------------------------
# Daily data regression
reg_load <- lm(gr_act_load_d[,1] ~ gr_temp_d[,1] + cable[,1])
summary(reg_load)
# Discussion : Cable effect is not significant to affect production. Endogeneity seems excluded

checkresiduals(reg_load)
# Discussion : No autocorrelation in the residuals

# ------------------------------------------------------------------------------
# Appendix
# ------------------------------------------------------------------------------

# maxP <- 8   # Maximum number of AR lags
# maxQ <- 8   # Maximum number of MA lags
# 
# # Auto Arima
# model_arma <- auto.arima(act_wind, max.p = maxP, max.q = maxQ, d = 0, ic = c("bic"), allowmean = TRUE, seasonal = FALSE, stepwise = FALSE)
# summary(model_arma)
# # Discussion : The best model from the auto arima function is ARMA(4,0)
# 
# checkresiduals(model_arma)
# res_act_wind <- residuals(model_arma)