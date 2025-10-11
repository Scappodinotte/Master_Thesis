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
library(tidyr)
library(tseries)

# Delete all objects in the memory
rm(list=ls())

# Set Working Directory
setwd("C:/Users/elias/Documents/Personale/UNINE/Master_Applied_Economics/Master_Thesis/")

# Load user-defined commands and packages
source("UserPackages.R")

# Creates the folder in the current directory
mainDir <- getwd()
outDir <- makeOutDir(mainDir, "/Results")

# ------------------------------------------------------------------------------
# Import, merge data and set time format
# ------------------------------------------------------------------------------

# Import and merge
gen_23 <- read.csv("Data/Generation Forecasts for Wind and Solar_202301010000-202401010000.csv", header = T, sep = ",")
gen_24 <- read.csv("Data/Generation Forecasts for Wind and Solar_202401010000-202501010000.csv", header = T, sep = ",")

gen_df <- rbind(gen_23, gen_24)
rm(gen_23, gen_24)

load_23 <- read.csv("Data/Total Load - Day Ahead _ Actual_202301010000-202401010000.csv", header = T, sep = ",")
load_24 <- read.csv("Data/Total Load - Day Ahead _ Actual_202401010000-202501010000.csv", header = T, sep = ",")

load_df <- rbind(load_23, load_24)
rm(load_23, load_24)

price_23 <- read.csv("Data/GUI_ENERGY_PRICES_202301010000-202401010000.csv", header = T, sep = ",")
price_24 <- read.csv("Data/GUI_ENERGY_PRICES_202401010000-202501010000.csv", header = T, sep = ",")

price_df <- rbind(price_23, price_24)
rm(price_23, price_24)

# Change time format
gen_df$DateTime <- as.POSIXct(gen_df$DateTime, format = "%d.%m.%Y %H:%M", tz = "UTC")
load_df$DateTime <- as.POSIXct(load_df$DateTime, format = "%d.%m.%Y %H:%M", tz = "UTC")
price_df$DateTime <- as.POSIXct(price_df$DateTime, format = "%d/%m/%Y %H:%M", tz = "UTC")

# ------------------------------------------------------------------------------
# Remove useless variables and december 2024
# ------------------------------------------------------------------------------

# Remove variables
gen_df <- gen_df %>%
  select(-c(MTU..UTC., Generation...Solar..MW..Intraday...BZN.EE, Generation...Solar..MW..Current...BZN.EE,
            Generation...Wind.Offshore..MW..Day.Ahead..BZN.EE, Generation...Wind.Offshore..MW..Intraday...BZN.EE,
            Generation...Wind.Offshore..MW..Current...BZN.EE, Generation...Wind.Onshore..MW..Intraday...BZN.EE,
            Generation...Wind.Onshore..MW..Current...BZN.EE))
names(gen_df) <- c("DateTime", "Solar", "Wind")

load_df <- load_df %>%
  select(-c(Time..UTC., Actual.Total.Load..MW....BZN.EE))
names(load_df) <- c("DateTime", "Load")

price_df <- price_df %>%
  select(-c(MTU..UTC., Area, Sequence, Intraday.Period..UTC., Intraday.Price..EUR.MWh.))
names(price_df) <- c("DateTime", "Day-Ahead")

# Reduce time span
gen_df <- gen_df %>%
  filter(DateTime < as.POSIXct("2024-12-01 00:00:00", format = "%Y-%m-%d %H:%M", tz = "UTC"))

load_df <- load_df %>%
  filter(DateTime < as.POSIXct("2024-12-01 00:00:00", format = "%Y-%m-%d %H:%M", tz = "UTC"))

price_df <- price_df %>%
  filter(DateTime < as.POSIXct("2024-12-01 00:00:00", format = "%Y-%m-%d %H:%M", tz = "UTC"))

# ------------------------------------------------------------------------------
# Check NAs and treat them
# ------------------------------------------------------------------------------

which(gen_df$Solar == "N/A") #2023-03-26 21:00:00 - 2023-03-27 20:00:00
which(gen_df$Wind == "N/A") #2023-03-26 21:00:00 - 2023-03-27 20:00:00
which(load_df$Load == "N/A") #No NA
which(price_df$`Day-Ahead` == "N/A") #No NA

# Replace NAs with actual values
act_df <- read.csv("Data/Actual_Gen_NA.csv", header = T, sep = ",")

gen_df$Solar[gen_df$Solar == "N/A"] <- act_df$Solar...Actual.Aggregated..MW.
gen_df$Wind[gen_df$Wind == "N/A"] <- act_df$Wind.Onshore...Actual.Aggregated..MW.

rm(act_df)

# ------------------------------------------------------------------------------
# Transform in time series and plotting
# ------------------------------------------------------------------------------

solar <- xts(as.double(gen_df$Solar), order.by = gen_df$DateTime)
wind <- xts(as.double(gen_df$Wind), order.by = gen_df$DateTime)
load <- xts(as.double(load_df$Load), order.by = load_df$DateTime)
price <- xts(as.double(price_df$`Day-Ahead`), order.by = price_df$DateTime)

rm(gen_df, load_df, price_df)

# Reduce to daily values

solar_d <- apply.weekly(solar, sum)
wind_d <- apply.weekly(wind, sum)
load_d <- apply.weekly(load, sum)
price_d <- apply.weekly(price, mean)

# Reduce to weekly values

solar_w <- apply.weekly(solar, sum)
wind_w <- apply.weekly(wind, sum)
load_w <- apply.weekly(load, sum)
price_w <- apply.weekly(price, mean)

# Reduce to monthly values

solar_m <- apply.monthly(solar, sum)
wind_m <- apply.monthly(wind, sum)
load_m <- apply.monthly(load, sum)
price_m <- apply.monthly(price, mean)


# Reduce time span to one week where cable fault
start <- as.POSIXct("2024-02-22 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
end <- as.POSIXct("2024-03-02 23:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

solar_p <- ts_span(solar, start, end)
wind_p <- ts_span(wind, start, end)
load_p <- ts_span(load, start, end)
price_p <- ts_span(price, start, end)


ts_plot(
    solar_p,
    title = "Hourly solar production Estonia",
    subtitle = "MW - No transformation"
)
ts_save(filename = paste(outDir, "/solar_week_feb.pdf", sep = ""), width = 8, height = 5, open = FALSE)

ts_plot(
  wind_p,
  title = "Hourly wind production Estonia",
  subtitle = "MW - No transformation"
)
ts_save(filename = paste(outDir, "/wind_week_feb.pdf", sep = ""), width = 8, height = 5, open = FALSE)

ts_plot(
  load_p,
  title = "Hourly load Estonia",
  subtitle = "MW - No transformation"
)
ts_save(filename = paste(outDir, "/load_week_feb.pdf", sep = ""), width = 8, height = 5, open = FALSE)

ts_plot(
  price_p,
  title = "Hourly day-ahead power price Estonia",
  subtitle = "EUR/MWh - No transformation"
)
ts_save(filename = paste(outDir, "/price_week_feb.pdf", sep = ""), width = 8, height = 5, open = FALSE)

rm(solar_p, wind_p, load_p, price_p)

# Reduce time span to 6 months
start <- as.POSIXct("2023-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
end <- as.POSIXct("2023-07-31 23:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

solar_y <- ts_span(solar, start, end)
wind_y <- ts_span(wind, start, end)
load_y <- ts_span(load, start, end)
price_y <- ts_span(price, start, end)


ts_plot(
  solar_y,
  title = "Hourly solar production Estonia",
  subtitle = "MW - No transformation"
)

ts_plot(
  wind_y,
  title = "Hourly wind production Estonia",
  subtitle = "MW - No transformation"
)

ts_plot(
  load_y,
  title = "Hourly load Estonia",
  subtitle = "MW - No transformation"
)

ts_plot(
  price_y,
  title = "Hourly day-ahead power price Estonia",
  subtitle = "EUR/MWh - No transformation"
)

rm(solar_y, wind_y, load_y, price_y)

# Discussion: No trend

# ------------------------------------------------------------------------------
# Check stationary
# ------------------------------------------------------------------------------

plotACF(solar, lag.max = 30) # Short term seasonality (12h and 24) + low persistence
plotACF(wind, lag.max = 30) # Trend/Persistence
plotACF(load, lag.max = 30) # High trend/persistence + Short term seasonality (at 24)
plotACF(price, lag.max = 30) # Short term seasonality (12h and 24) + medium persistence

plotACF(solar_d, lag.max = 12) # No seasonality + just persistence
plotACF(wind_d, lag.max = 12) # No seasonality + just persistence
plotACF(load_d, lag.max = 12) # No seasonality + just persistence
plotACF(price_d, lag.max = 12) # Short term seasonality (5 days) + no persistence

plotACF(solar_w, lag.max = 30) # 6 month seasonality + persistence
plotACF(wind_w, lag.max = 30) # Low persistence + seasonality each 3 weeks
plotACF(load_w, lag.max = 30) # 6 month seasonality + persistence
plotACF(price_w, lag.max = 30) # no more persistence! + evidence of some week autocorrelation

plotACF(solar_m, lag.max = 24) # 6 month seasonality confirmed
plotACF(wind_m, lag.max = 24) # Stationary, seasonality and persistence no more present
plotACF(load_m, lag.max = 24) # 6 month seasonality confirmed + weak persistence
plotACF(price_m, lag.max = 24) # no more persistence! + evidence of some week autocorrelation

# Conclusion: 
# Short term seasonality at 12h and 24h for load, solar and price as expected
# Long term seasonality at 6 months for solar and load
# TS are highly persistent, like trend, include time running variable 

rm(solar_d, solar_w, solar_m, wind_d, wind_w, wind_m, 
   load_d, load_w, load_m, price_d, price_w, price_m)

# KPSS Test, Phillips-Perron and ADF

# Solar 

# Null: TS is mean stationary (CSP)
kpss.test(solar, null = "Level")
# Discussion: Reject the null at 1%, ts is not level stationary

# Null: TS has a unit root (non stationary, random walk)
sol_drift <- CADFtest(solar, max.lag.y = 24, type = "drift")
summary(sol_drift)
# Discussion: reject at 1% that ts is non stationary

# Null:  TS has a unit root (non stationary, random walk)
pp.test(solar, type = "Z(t_alpha)")
# Discussion: rejects the non-stationnarity. 

# Conclusion: the ts is CSP 2/3


# Wind 

# Null: TS is mean stationary (CSP)
kpss.test(wind, null = "Level")
# Discussion: Reject the null at 1%, ts is not level stationary

# Null: TS has a unit root (non stationary, random walk)
wind_drift <- CADFtest(wind, max.lag.y = 24, type = "drift")
summary(wind_drift)
# Discussion: reject at 1% that ts is non stationary

# Null:  TS has a unit root (non stationary, random walk)
pp.test(wind, type = "Z(t_alpha)")
# Discussion: rejects the non-stationnarity. 

# Conclusion: the ts is CSP 2/3


# Load 

# Null: TS is mean stationary (CSP)
kpss.test(load, null = "Level")
# Discussion: Reject the null at 1%, ts is not level stationary

# Null: TS has a unit root (non stationary, random walk)
load_drift <- CADFtest(load, max.lag.y = 24, type = "drift")
summary(load_drift)
# Discussion: reject at 1% that ts is non stationary

# Null:  TS has a unit root (non stationary, random walk)
pp.test(load, type = "Z(t_alpha)")
# Discussion: rejects the non-stationnarity. 

# Conclusion: the ts is CSP 2/3


# Price 

# Null: TS is mean stationary (CSP)
kpss.test(price, null = "Level")
# Discussion: Does not reject the null at 5%, ts is level stationary

# Null: TS has a unit root (non stationary, random walk)
price_drift <- CADFtest(price, max.lag.y = 24, type = "drift")
summary(price_drift)
# Discussion: reject at 1% that ts is non stationary

# Null:  TS has a unit root (non stationary, random walk)
pp.test(price, type = "Z(t_alpha)")
# Discussion: rejects the non-stationarity. 

# Conclusion: the ts is CSP 3/3

rm(price_drift, load_drift, wind_drift, sol_drift)



# ------------------------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------------------------
summary(solar)



summary(wind)
summary(load)
summary(price)


# ------------------------------------------------------------------------------
# Appendix
# ------------------------------------------------------------------------------

# # Null: TS is trend stationary
# kpss.test(solar, null = "Trend")
# # Discussion: Reject the null at 5%, ts is not trend stationary
# 
# # Null: TS has a unit root (non stationary, deterministic trend)
# sol_trend <- CADFtest(solar, max.lag.y = 24, type = "trend")
# summary(sol_trend)
# # Discussion: the unit root is rejected at 1%


