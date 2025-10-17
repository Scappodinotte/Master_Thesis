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

# Create time data frame to address seasonality
init_date <- df_prod$DateTime[1]
time <- data.frame(DateTime = df_prod$DateTime)
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
    dec = as.numeric(month(DateTime) == 12),
    run = floor(as.double(difftime(DateTime, init_date, units = "hour")))
  )

time <- tail(time, -1)

# transform into time series
solar_h <- xts(as.double(df_prod$Solar...Actual.Aggregated..MW.), order.by = df_prod$DateTime)
wind_h <- xts(as.double(df_prod$Wind.Onshore...Actual.Aggregated..MW.), order.by = df_prod$DateTime)
load_h <- xts(as.double(df_load$Actual.Total.Load..MW....BZN.EE), order.by = df_load$DateTime)
irrad_h <- xts(as.double(estonia_h$irradiation), order.by = estonia_h$DateTime)
speed_h <- xts(as.double(estonia_h$wind_speed), order.by = estonia_h$DateTime)
temp_h <- xts(as.double(estonia_h$air_temp), order.by = estonia_h$DateTime)

# Reduce time span to one week where cable fault
start <- as.POSIXct("2024-01-22 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
end <- as.POSIXct("2024-02-02 23:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

solar_r <- ts_span(solar_h, start = start, end = end)
wind_r <- ts_span(wind_h, start, end)
load_r <- ts_span(load_h, start, end)
irrad_r <- ts_span(irrad_h, start, end)
speed_r <- ts_span(speed_h, start, end)
temp_r <- ts_span(temp_h, start, end)


# Aggregate to daily frequency
solar_d <- apply.daily(solar_h, sum)
wind_d <- apply.daily(wind_h, sum)
load_d <- apply.daily(load_h, sum)
irrad_d <- apply.daily(irrad_h, mean)
speed_d <- apply.daily(speed_h, mean)
temp_d <- apply.daily(temp_h, mean)

ratio_s_i <- solar_d/irrad_d

ts_plot(
  ratio_s_i,
  title = "Hourly solar production",
  subtitle = "MW - No transformation"
)

ratio_w_s <- wind_r/speed_r

ts_plot(
  ratio_w_s,
  title = "Hourly solar production",
  subtitle = "MW - No transformation"
)

# Create daily cable variable
# Fault on 26th at 00:10 eet (22:10 UTC), so 25th operational and on 26th our of service
cable <- data.frame(date = index(wind_d), cable = c(rep(1, 25), rep(0, 341)))
# cable_placebo <- data.frame(date = index(wind_d), cable = c(rep(1, 24), rep(0, 17)))


# ------------------------------------------------------------------------------
# Plotting and data transformation
# ------------------------------------------------------------------------------
# Plotting the TS
g <- ggplot(ts_diff(log(solar_d))) + geom_line(aes(x = index(ts_diff(log(solar_d))), y = ts_diff(log(solar_d)))) + theme_minimal()
g <- g + xlab("Days") + ylab("Megawatt [MW]") + ggtitle("actual solar production Estonia", subtitle = "Days, not seasonally adjusted")
g
# Discussion: 

g <- ggplot(ts_diff(log(wind_d))) + geom_line(aes(x = index(ts_diff(log(wind_d))), y = ts_diff(log(wind_d)))) + theme_minimal()
g <- g + xlab("Days") + ylab("Megawatt [MW]") + ggtitle("actual wind production Estonia", subtitle = "Days, not seasonally adjusted")
g
# Discussion: No apparent trend visually spotted

g <- ggplot(load_d) + geom_line(aes(x = index(load_d), y = load_d)) + theme_minimal()
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
plotACF(ts_diff(log(solar_d)), lag.max = 20)
plotACF(ts_diff(log(wind_d)), lag.max = 20)
plotACF(ts_diff(log(load_d)), lag.max = 20)
plotACF(ts_diff(log(irrad_d)), lag.max = 20)
plotACF(ts_diff(log(speed_d)), lag.max = 20)
plotACF(ts_diff(log(temp_d)), lag.max = 20)

# Discussion : Solar and irradiation show a small trend and seasonality each 6 days.
# Wind has neither and load and temperature have small trend.

# Check unit root process
ur_sol_drift = CADFtest(ts_diff(log(solar_d)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_sol_drift)
# Discussion: the Non-stationnarity is rejected at 5% (p-value = 0.04)

ur_wind_drift = CADFtest(ts_diff(log(wind_d)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_wind_drift)
# Discussion: the TS is CSP

ur_load_drift = CADFtest(ts_diff(log(load_d)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_load_drift)
# Discussion: the TS is not CSP

ur_irrad_drift = CADFtest(ts_diff(log(irrad_d)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_irrad_drift)
# Discussion: the TS is not CSP

ur_speed_drift = CADFtest(ts_diff(log(speed_d)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_speed_drift)
# Discussion: the TS is CSP

ur_temp_trend = CADFtest(ts_diff(log(temp_d)), max.lag.y = 10, type = "drift", criterion = "BIC")
summary(ur_temp_trend)
# Discussion: the TS is not trend CSP

# Actual solar is trend stationary and Wind prod and speed are CSP

# ------------------------------------------------------------------------------
# Regression wind - speed
# ------------------------------------------------------------------------------
# Daily data regression
reg_wind <- lm(ts_diff(log(wind_d[,1])) ~ ts_diff(log(speed_d[,1])) + cable[,2] +
                 mon + tue + thu + fri + sat + sun + jan + feb + mar + apr + may + jun + jul +
                 aug + sep + oct + nov, data = time)
summary(reg_wind)

# Discussion : Cable effect seems relevant for wind power

checkresiduals(reg_wind)
# Discussion : No autocorrelation in the residuals

# ------------------------------------------------------------------------------
# Regression solar - irradiation
# ------------------------------------------------------------------------------
# Hourly data regression
reg_sol <- lm(gr_solar_d[,1] ~ gr_irrad_d[,1] + cable[,2])
summary(reg_sol)
# Discussion : Cable effect is not significant to affect production. Endogeneity seems excluded

checkresiduals(reg_sol)
# Discussion : No autocorrelation in the residuals

# ------------------------------------------------------------------------------
# Regression load - temperature
# ------------------------------------------------------------------------------
# Daily data regression
reg_load <- lm(gr_load_d[,1] ~ gr_temp_d[,1] + cable[,1])
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