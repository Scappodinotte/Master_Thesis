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
# use_github()


# ------------------------------------------------------------------------------
# Load packages and functions
# ------------------------------------------------------------------------------


library(tsbox)
library(xts)
library(ggplot2) # plot  
library(cowplot) # stack multiple plots together
library(CADFtest)
library(reshape2) 
library(lubridate)
library(writexl)
library(dplyr)
library(sandwich) # heteroskedastic robust SE
library(tidyr)
library(broom) # export coefficient from regression
library(tseries)
library(moments) # kurtosis and skewness
library(xtable) # write latex table

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

flow_23_fi <- read.csv("Data/Cross-Border Physical Flow_202301010000-202401010000_fi.csv", header = T, sep = ",")
flow_24_fi <- read.csv("Data/Cross-Border Physical Flow_202401010000-202501010000_fi.csv", header = T, sep = ",")

flow_fi_df <- rbind(flow_23_fi, flow_24_fi)

flow_23_ru <- read.csv("Data/Cross-Border Physical Flow_202301010000-202401010000_ru.csv", header = T, sep = ",")
flow_24_ru <- read.csv("Data/Cross-Border Physical Flow_202401010000-202501010000_ru.csv", header = T, sep = ",")

flow_ru_df <- rbind(flow_23_ru, flow_24_ru)

flow_df <- cbind(flow_fi_df, flow_ru_df)
rm(flow_23_fi, flow_24_fi, flow_23_ru, flow_24_ru, flow_fi_df, flow_ru_df)

price_22 <- read.csv("Data/GUI_ENERGY_PRICES_202212310000-202301010000.csv", header = T, sep = ",")
price_23 <- read.csv("Data/GUI_ENERGY_PRICES_202301010000-202401010000.csv", header = T, sep = ",")
price_24 <- read.csv("Data/GUI_ENERGY_PRICES_202401010000-202501010000.csv", header = T, sep = ",")

price_df <- rbind(price_22, price_23, price_24)
rm(price_22, price_23, price_24)

# Change time format
flow_df$DateTime <- as.POSIXct(gen_df$DateTime, format = "%d.%m.%Y %H:%M", tz = "UTC")
gen_df$DateTime <- as.POSIXct(gen_df$DateTime, format = "%d.%m.%Y %H:%M", tz = "UTC")
load_df$DateTime <- as.POSIXct(load_df$DateTime, format = "%d.%m.%Y %H:%M", tz = "UTC")
price_df$DateTime <- as.POSIXct(price_df$DateTime, format = "%d/%m/%Y %H:%M", tz = "UTC")


# ------------------------------------------------------------------------------
# Remove useless variables
# ------------------------------------------------------------------------------


# Remove variables
gen_df <- gen_df %>%
  select(-c(MTU..UTC., Generation...Solar..MW..Intraday...BZN.EE, Generation...Solar..MW..Current...BZN.EE,
            Generation...Wind.Offshore..MW..Day.Ahead..BZN.EE, Generation...Wind.Offshore..MW..Intraday...BZN.EE,
            Generation...Wind.Offshore..MW..Current...BZN.EE, Generation...Wind.Onshore..MW..Intraday...BZN.EE,
            Generation...Wind.Onshore..MW..Current...BZN.EE))
names(gen_df) <- c("DateTime", "Solar", "Wind")

load_df <- load_df %>%
  select(-(Time..UTC.))
names(load_df) <- c("DateTime", "Load", "Act_load")

price_df <- price_df %>%
  select(-c(MTU..UTC., Area, Sequence, Intraday.Period..UTC., Intraday.Price..EUR.MWh.))
names(price_df) <- c("DateTime", "Day-Ahead")

names(flow_df) <- c("UTC","DateTime", "Imp_fi", "Exp_fi", "UTC","DateTime", "Imp_ru", "Exp_ru")
flow_df <- flow_df %>%
  select(-c(1, 5, 6)) %>%
  mutate(Imp_ru = as.numeric(Imp_ru), Exp_ru = as.numeric(Exp_ru),
    Ime_fi = Imp_fi - Exp_fi, Ime_ru = Imp_ru - Exp_ru)


# ------------------------------------------------------------------------------
# Check NAs and treat them
# ------------------------------------------------------------------------------


which(gen_df$Solar == "N/A") #2023-03-26 21:00:00 - 2023-03-27 20:00:00
which(gen_df$Wind == "N/A") #2023-03-26 21:00:00 - 2023-03-27 20:00:00
which(load_df$Load == "N/A") #No NA
which(price_df$`Day-Ahead` == "N/A") #No NA
which(is.na(flow_df$Imp_fi)) # No NA
which(is.na(flow_df$Exp_fi)) # No NA
which(is.na(flow_df$Imp_ru)) # No NA
which(is.na(flow_df$Exp_ru)) # No NA

# Replace NAs with actual values
act_df <- read.csv("Data/Actual_Gen_NA.csv", header = T, sep = ",")

gen_df$Solar[gen_df$Solar == "N/A"] <- act_df$Solar...Actual.Aggregated..MW.
gen_df$Wind[gen_df$Wind == "N/A"] <- act_df$Wind.Onshore...Actual.Aggregated..MW.

rm(act_df)

gen_df$Solar <- as.double(gen_df$Solar)
gen_df$Wind <- as.double(gen_df$Wind)
load_df$Load <- as.double(load_df$Load)
load_df$Act_load <- as.double(load_df$Act_load)
price_df$`Day-Ahead` <- as.double(price_df$`Day-Ahead`)


# ------------------------------------------------------------------------------
# Replace load errors 
# ------------------------------------------------------------------------------


load_df <- load_df %>%
  mutate(
    new_load = if_else(
      between(DateTime, ymd_hms("2023-03-12 23:00:00"), ymd_hms("2023-03-13 22:00:00")) |
        between(DateTime, ymd_hms("2023-11-04 22:00:00"), ymd_hms("2023-11-05 22:00:00")) |
        between(DateTime, ymd_hms("2024-04-06 22:00:00"), ymd_hms("2024-04-07 21:00:00")) |
        between(DateTime, ymd_hms("2024-06-26 22:00:00"), ymd_hms("2024-06-27 21:00:00")) |
        between(DateTime, ymd_hms("2024-12-20 23:00:00"), ymd_hms("2024-12-22 21:00:00")) |
        DateTime == ymd_hms("2024-12-30 07:00:00") |
        DateTime == ymd_hms("2024-12-30 09:00:00") |
        DateTime == ymd_hms("2024-12-30 11:00:00") |
        DateTime == ymd_hms("2024-12-30 12:00:00"),
      Act_load,
      Load
    )
  )

g <- ggplot(load_df, aes(x = DateTime, y = Load)) + 
  geom_line(colour = "blue") +
  geom_line(aes(x = DateTime, y = new_load), colour = "red", alpha = 0.5) +
  theme_minimal()
g

load_df <- load_df %>%
  select(-c(2, 3))
names(load_df) <- c("DateTime", "Load")


# ------------------------------------------------------------------------------
# Define variables for panel
# ------------------------------------------------------------------------------


init_date <- as.POSIXct("2023-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

price_df <- price_df %>%
  mutate(
    hour = hour(DateTime),
    day = floor(as.double(difftime(DateTime, init_date, units = "days") + 1)),
    cable = 1 - as.numeric(between(DateTime, ymd_hms("2024-01-25 22:10:00"), ymd_hms("2024-09-04 00:00:00")) |
                           between(DateTime, ymd_hms("2024-12-25 13:00:00"), ymd_hms("2024-12-31 23:00:00"))),
    cable_pla = 1 - as.numeric(between(DateTime, ymd_hms("2023-08-01 00:00:00"), ymd_hms("2024-03-31 23:00:00"))),
    pskov = 1 - as.numeric(DateTime >= ymd_hms("2024-07-18 10:15:00")),
  )

# Create lagged price
price_df <- price_df %>%
  group_by(hour) %>%
  mutate(lag_p = lag(`Day-Ahead`, order_by = day)) %>%
  arrange(DateTime) %>%
  filter(DateTime >= init_date)

# Create time for seasonality control
time <- data.frame(DateTime = gen_df$DateTime)
time <- time %>%
  mutate(
    weekday = weekdays(as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")),
    hol = as.numeric(date(DateTime) == "2023-02-24" |
                       date(DateTime) == "2023-04-07" |
                       date(DateTime) == "2023-05-01" |
                       date(DateTime) == "2023-06-23" |
                       date(DateTime) == "2023-12-25" |
                       date(DateTime) == "2023-12-26" |
                       date(DateTime) == "2024-01-01" |
                       date(DateTime) == "2024-03-29" |
                       date(DateTime) == "2024-05-01" |
                       date(DateTime) == "2024-06-24" |
                       date(DateTime) == "2024-08-20" ),
    h0 = as.numeric(hour(DateTime) == 0),
    h1 = as.numeric(hour(DateTime) == 1),
    h2 = as.numeric(hour(DateTime) == 2),
    h3 = as.numeric(hour(DateTime) == 3),
    h4 = as.numeric(hour(DateTime) == 4),
    h5 = as.numeric(hour(DateTime) == 5),
    h6 = as.numeric(hour(DateTime) == 6),
    h7 = as.numeric(hour(DateTime) == 7),
    h8 = as.numeric(hour(DateTime) == 8),
    h9 = as.numeric(hour(DateTime) == 9),
    h10 = as.numeric(hour(DateTime) == 10),
    h11 = as.numeric(hour(DateTime) == 11),
    h12 = as.numeric(hour(DateTime) == 12),
    h13 = as.numeric(hour(DateTime) == 13),
    h14 = as.numeric(hour(DateTime) == 14),
    h15 = as.numeric(hour(DateTime) == 15),
    h16 = as.numeric(hour(DateTime) == 16),
    h17 = as.numeric(hour(DateTime) == 17),
    h18 = as.numeric(hour(DateTime) == 18),
    h19 = as.numeric(hour(DateTime) == 19),
    h20 = as.numeric(hour(DateTime) == 20),
    h21 = as.numeric(hour(DateTime) == 21),
    h22 = as.numeric(hour(DateTime) == 22),
    h23 = as.numeric(hour(DateTime) == 23),
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


# ------------------------------------------------------------------------------
# Clustering load
# ------------------------------------------------------------------------------

set.seed(2)

load_df <- load_df %>%
  group_by(month(DateTime)) %>%
  mutate(base_lim = min(as.numeric(kmeans(Load, centers = 2, iter.max = 20, nstart = 25)$centers)),
         peak_lim = max(as.numeric(kmeans(Load, centers = 2, iter.max = 20, nstart = 25)$centers)),
         base = as.numeric(Load <= base_lim),
         inter = as.numeric(Load <= peak_lim & Load > base_lim),
         peak = as.numeric(!base & !inter))


# ------------------------------------------------------------------------------
# Create STATA data set 
# ------------------------------------------------------------------------------


stata_df <- data.frame(hour = price_df$hour,
                       day = price_df$day,
                       price = price_df$`Day-Ahead`,
                       solar = gen_df$Solar,
                       wind = gen_df$Wind,
                       load = load_df$Load,
                       cable = price_df$cable,
                       exp_fi = flow_df$Exp_fi,
                       imp_fi = flow_df$Imp_fi,
                       ime_fi = flow_df$Ime_fi,
                       exp_ru = flow_df$Exp_ru,
                       imp_ru = flow_df$Imp_ru,
                       ime_ru = flow_df$Ime_ru,
                       base = load_df$base,
                       inter = load_df$inter,
                       peak = load_df$peak,
                       pskov = price_df$pskov,
                       lag_p = price_df$lag_p,
                       holiday = time$hol,
                       mon = time$mon,
                       tue = time$tue,
                       wed = time$wed,
                       thu = time$thu,
                       fri = time$fri,
                       sat = time$sat,
                       sun = time$sun,
                       jan = time$jan,
                       feb = time$feb,
                       mar = time$mar,
                       apr = time$apr,
                       may = time$may,
                       jun = time$jun,
                       jul = time$jul,
                       aug = time$aug,
                       sep = time$sep,
                       oct = time$oct,
                       nov = time$nov,
                       dec = time$dec
)

write.csv(stata_df, file = "Data/Stata_df.csv", row.names = F)

rm(stata_df)


# ------------------------------------------------------------------------------
# Create STATA data set reduced
# ------------------------------------------------------------------------------


price_dfs <- price_df %>%
  filter(DateTime < ymd_hms("2024-07-18 00:00:00"))

gen_dfs <- gen_df %>%
  filter(DateTime < ymd_hms("2024-07-18 00:00:00"))

load_dfs <- load_df %>%
  filter(DateTime < ymd_hms("2024-07-18 00:00:00"))

time_s <- time %>%
  filter(DateTime < ymd_hms("2024-07-18 00:00:00"))

flow_dfs <- flow_df %>%
  filter(DateTime < ymd_hms("2024-07-18 00:00:00"))

stata_dfs <- data.frame(hour = price_dfs$hour,
                       day = price_dfs$day,
                       price = price_dfs$`Day-Ahead`,
                       solar = gen_dfs$Solar,
                       wind = gen_dfs$Wind,
                       load = load_dfs$Load,
                       cable = price_dfs$cable,
                       cable_pla = price_dfs$cable_pla,
                       exp_fi = flow_dfs$Exp_fi,
                       imp_fi = flow_dfs$Imp_fi,
                       ime_fi = flow_dfs$Ime_fi,
                       exp_ru = flow_dfs$Exp_ru,
                       imp_ru = flow_dfs$Imp_ru,
                       ime_ru = flow_dfs$Ime_ru,
                       base = load_dfs$base,
                       inter = load_dfs$inter,
                       peak = load_dfs$peak,
                       lag_p = price_dfs$lag_p,
                       holiday = time_s$hol,
                       mon = time_s$mon,
                       tue = time_s$tue,
                       wed = time_s$wed,
                       thu = time_s$thu,
                       fri = time_s$fri,
                       sat = time_s$sat,
                       sun = time_s$sun,
                       jan = time_s$jan,
                       feb = time_s$feb,
                       mar = time_s$mar,
                       apr = time_s$apr,
                       may = time_s$may,
                       jun = time_s$jun,
                       jul = time_s$jul,
                       aug = time_s$aug,
                       sep = time_s$sep,
                       oct = time_s$oct,
                       nov = time_s$nov,
                       dec = time_s$dec
)

write.csv(stata_dfs, file = "Data/Stata_dfs.csv", row.names = F)

rm(stata_dfs)


# ------------------------------------------------------------------------------
# Plotting entire ts
# ------------------------------------------------------------------------------


g <- ggplot(gen_df, aes(x = DateTime, y = Solar)) + 
  geom_line() +
  labs(
    title = "",
    y = "",
    x = ""
  ) +
  scale_x_date(expand = expansion(add = c(0, 50))) +
  theme_minimal() +
  theme(text = element_text(size = 16))
g
ggsave(filename = "Solar.pdf", path = outDir, width = 7)

g <- ggplot(gen_df, aes(x = DateTime, y = Wind)) + 
  geom_line() +
  labs(
    title = "",
    y = "",
    x = ""
  ) +
  scale_x_date(expand = expansion(add = c(0, 50))) +
  theme_minimal() +
  theme(text = element_text(size = 16))
g
ggsave(filename = "Wind.pdf", path = outDir, width = 7)

g <- ggplot(load_df, aes(x = DateTime, y = Load)) + 
  geom_line() +
  labs(
    title = "",
    y = "",
    x = ""
  ) +
  scale_x_date(expand = expansion(add = c(0, 50))) +
  theme_minimal() +
  theme(text = element_text(size = 16))
g
ggsave(filename = "Load.pdf", path = outDir, width = 7)

g <- ggplot(price_df, aes(x = DateTime, y = `Day-Ahead`)) + 
  geom_line() +
  labs(
    title = "",
    y = "",
    x = ""
  ) +
  ylim(-70, 1000) +
  scale_x_date(expand = expansion(add = c(0, 50))) +
  theme_minimal() +
  theme(text = element_text(size = 16))
g
ggsave(filename = "Price.pdf", path = outDir, width = 7)


# ------------------------------------------------------------------------------
# Check stationarity and seasonality
# ------------------------------------------------------------------------------


model_p <- lm(price_df$`Day-Ahead` ~ h0 + h1 + h2 + h3 + h4 + h5 + h6 + h8 + h9 + h10 + h11 
              + h12 + h13 + h14 + h15 + h16 + h17 + h18 + h19 + h20 + h21 + h22
              + mon + tue + thu + fri + sat + sun
              + jan + feb + mar + may + jun + jul + aug + sep + oct + nov + dec, data = time)

summary(model_p)
coef_p <- tidy(model_p)
res_p <- model_p$residuals

g1 <- coef_p[seq(2, 23),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g1

g2 <- coef_p[c(24, 25, 26, 27, 28, 29),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g2

g3 <- coef_p[seq(30, 40),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g3

plot_grid(g1, plot_grid(g2, g3, ncol = 1, nrow = 2, align = "v"), ncol = 2, align = "v", hjust = 0)
ggsave(filename = "seas_p.pdf", path = outDir, width = 6, height = 7)

price_drift <- CADFtest(res_p, max.lag.y = 24, type = "drift")
summary(price_drift)

# checkresiduals(res_p)
plotACF(price_df$`Day-Ahead`, lag.max = 40)
plotACF(res_p, lag.max = 40)



model_s <- lm(gen_df$Solar ~ h0 + h1 + h2 + h3 + h4 + h5 + h6 + h8 + h9 + h10 + h11 
              + h12 + h13 + h14 + h15 + h16 + h17 + h18 + h19 + h20 + h21 + h22
              + mon + tue + thu + fri + sat + sun
              + jan + feb + mar + may + jun + jul + aug + sep + oct + nov + dec, data = time)

summary(model_s)
coef_s <- tidy(model_s)
res_s <- model_s$residuals

g1 <- coef_s[seq(2, 23),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g1

g2 <- coef_s[c(24, 25, 26, 27, 28, 29),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g2

g3 <- coef_s[seq(30, 40),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g3

plot_grid(g1, plot_grid(g2, g3, ncol = 1, nrow = 2, align = "v"), ncol = 2, align = "v", hjust = -0.01)
ggsave(filename = "seas_s.pdf", path = outDir, width = 6, height = 7)

sol_drift <- CADFtest(res_s, max.lag.y = 24, type = "drift")
summary(sol_drift)

# checkresiduals(res_s)
plotACF(gen_df$Solar, lag.max = 40)
plotACF(res_s, lag.max = 40)



model_w <- lm(gen_df$Wind ~ h0 + h1 + h2 + h3 + h4 + h5 + h6 + h8 + h9 + h10 + h11 
              + h12 + h13 + h14 + h15 + h16 + h17 + h18 + h19 + h20 + h21 + h22
              + mon + tue + thu + fri + sat + sun
              + jan + feb + mar + may + jun + jul + aug + sep + oct + nov + dec, data = time)

summary(model_w)
coef_w <- tidy(model_w)
res_w <- model_w$residuals

g1 <- coef_w[seq(2, 23),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g1

g2 <- coef_w[c(24, 25, 26, 27, 28, 29),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g2

g3 <- coef_w[seq(30, 40),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g3

plot_grid(g1, plot_grid(g2, g3, ncol = 1, nrow = 2, align = "v"), ncol = 2, align = "v", hjust = -0.01)
ggsave(filename = "seas_w.pdf", path = outDir, width = 6, height = 7)

wind_drift <- CADFtest(res_w, max.lag.y = 24, type = "drift")
summary(wind_drift)

# checkresiduals(res_w)
plotACF(gen_df$Wind, lag.max = 40)
plotACF(res_w, lag.max = 40)



model_l <- lm(load_df$Load ~ h0 + h1 + h2 + h3 + h4 + h5 + h6 + h8 + h9 + h10 + h11 
              + h12 + h13 + h14 + h15 + h16 + h17 + h18 + h19 + h20 + h21 + h22
              + mon + tue + thu + fri + sat + sun
              + jan + feb + mar + may + jun + jul + aug + sep + oct + nov + dec, data = time)

summary(model_l)
coef_l <- tidy(model_l)
res_l <- model_l$residuals

g1 <- coef_l[seq(2, 23),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g1

g2 <- coef_l[c(24, 25, 26, 27, 28, 29),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g2

g3 <- coef_l[seq(30, 40),] %>%
  mutate(term = factor(term, levels = term)) %>%
  ggplot(aes(x = term, y = estimate)) + 
  geom_pointrange(aes(ymin = estimate - 2 * std.error,
                      ymax = estimate + 2 * std.error)) + 
  coord_flip() + 
  geom_hline(aes(yintercept = 0), colour = "red") + 
  theme_minimal()
g3

plot_grid(g1, plot_grid(g2, g3, ncol = 1, nrow = 2, align = "v"), ncol = 2, align = "v", hjust = -0.01)
ggsave(filename = "seas_l.pdf", path = outDir, width = 6, height = 7)

load_drift <- CADFtest(res_l, max.lag.y = 24, type = "drift")
summary(load_drift)

# checkresiduals(res_l)
plotACF(load_df$Load, lag.max = 40)
plotACF(res_l, lag.max = 40)

rm(g1, g2, g3, model_l, model_p, model_s, model_w, coef_l, coef_p, coef_s, coef_w)


# ------------------------------------------------------------------------------
# Create latex table
# ------------------------------------------------------------------------------


t <- data.frame(c("Variable", "Solar", "Wind", "Load", "Price"),
                c("t-statistic", round(sol_drift$statistic, 2), round(wind_drift$statistic, 2),
                  round(load_drift$statistic, 2), round(price_drift$statistic, 2)),
                c("p-value", "<0.01", "<0.01", "<0.01", "<0.01"),
                c("", "", "", "", ""),
                c("t-statistic", "-22.67", "-23.69", "-23.69" , "-23.69"),
                c("p-value", "<0.01", "<0.01", "<0.01", "<0.01")
)

names(t) <- c("", "CADF","" ,"" ,"PCADF","")

print(xtable(t, type = "latex"))

rm(price_drift, load_drift, wind_drift, sol_drift, t)


# ------------------------------------------------------------------------------
# Summary statistics
# ------------------------------------------------------------------------------


# # Summary statistics for Solar
summary(gen_df$Solar)
mean_s <- mean(gen_df$Solar)
median_s <- median(gen_df$Solar)
min_s <- min(gen_df$Solar)
max_s <- max(gen_df$Solar)
sk_s <- skewness(gen_df$Solar)
ku_s <- kurtosis(gen_df$Solar)
sd_s <- sd(gen_df$Solar)

g <- ggplot(gen_df, aes(x = Solar)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 20, colour="black", fill = "white") + 
  geom_density(alpha = .2, fill = "red") + 
  geom_vline(aes(xintercept = mean(Solar),
                 color = "Mean"), linetype = "dashed", linewidth = 1) + 
  geom_vline(aes(xintercept = median(Solar),
                 color = "Median"), linetype = "dashed", linewidth = 1) +
  scale_color_manual(name = "Statistics", 
                     values = c("Mean" = "blue", "Median" = "green")) +
  labs(title = "Hourly solar production",
       subtitle = "In megawatts",
       x = "Solar power",
       y = "Density") +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.8),
    text = element_text(size = 14))
g
ggsave(filename = "solar_distrib.pdf", path = outDir, width = 8)
# Discussion: Right skewed distribution

jarque.bera.test(gen_df$Solar)
# Reject normality



# Summary statistics for Wind
summary(gen_df$Wind)
mean_w <- mean(gen_df$Wind)
median_w <- median(gen_df$Wind)
min_w <- min(gen_df$Wind)
max_w <- max(gen_df$Wind)
sk_w <- skewness(gen_df$Wind)
ku_w <- kurtosis(gen_df$Wind)
sd_w <- sd(gen_df$Wind)

g <- ggplot(gen_df, aes(x = Wind)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 20, colour="black", fill = "white") + 
  geom_density(alpha = .2, fill = "red") + 
  geom_vline(aes(xintercept = mean(Wind),
                 color = "Mean"), linetype = "dashed", linewidth = 1) + 
  geom_vline(aes(xintercept = median(Wind),
                 color = "Median"), linetype = "dashed", linewidth = 1) +
  scale_color_manual(name = "Statistics", 
                     values = c("Mean" = "blue", "Median" = "green")) +
  labs(title = "Hourly wind production",
       subtitle = "In megawatts", 
       x = "Wind power", 
       y = "Density") +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.8),
    text = element_text(size = 14))
g
ggsave(filename = "wind_distrib.pdf", path = outDir, width = 8)
# Discussion: Right skewed distribution

jarque.bera.test(gen_df$Wind)
# Reject normality



# Summary statistics for Load
summary(load_df$Load)
mean_l <- mean(load_df$Load)
median_l <- median(load_df$Load)
min_l <- min(load_df$Load)
max_l <- max(load_df$Load)
sk_l <- skewness(load_df$Load)
ku_l <- kurtosis(load_df$Load)
sd_l <- sd(load_df$Load)

g <- ggplot(load_df, aes(x = Load)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 50, colour="black", fill = "white") + 
  geom_density(alpha = .2, fill = "red") + 
  geom_vline(aes(xintercept = mean(Load),
                 color = "Mean"), linetype = "dashed", linewidth = 1) + 
  geom_vline(aes(xintercept = median(Load),
                 color = "Median"), linetype = "dashed", linewidth = 1) +
  scale_color_manual(name = "Statistics", 
                     values = c("Mean" = "blue", "Median" = "green")) +
  labs(title = "Hourly load", 
       subtitle = "In megawatts", 
       x = "Load", 
       y = "Density") +
  theme_minimal() + 
  theme(
    legend.position = c(0.8, 0.8),
    text = element_text(size = 14))
g
ggsave(filename = "load_distrib.pdf", path = outDir, width = 8)
# Discussion: Slightly right skewed

jarque.bera.test(load_df$Load)
# Reject normality



# Summary statistics for Price
summary(price_df$`Day-Ahead`)
mean_p <- mean(price_df$`Day-Ahead`)
median_p <- median(price_df$`Day-Ahead`)
min_p <- min(price_df$`Day-Ahead`)
max_p <- max(price_df$`Day-Ahead`)
sk_p <- skewness(price_df$`Day-Ahead`)
ku_p <- kurtosis(price_df$`Day-Ahead`)
sd_p <- sd(price_df$`Day-Ahead`)

g <- ggplot(price_df, aes(x = `Day-Ahead`)) +
  geom_histogram(aes(y = after_stat(density)), 
                 binwidth = 20, colour="black", fill = "white") + 
  geom_density(alpha = .2, fill = "red") + 
  geom_vline(aes(xintercept = mean(`Day-Ahead`),
                 color = "Mean"), linetype = "dashed", linewidth = 1) + 
  geom_vline(aes(xintercept = median(`Day-Ahead`),
                 color = "Median"), linetype = "dashed", linewidth = 1) +
  scale_color_manual(name = "Statistics", 
                     values = c("Mean" = "blue", "Median" = "green")) +
  xlim(-70, 500) +
  labs(title = "Hourly day-ahead electricity price", 
       subtitle = "In euro per megawatt-hour - x axis truncated for visibility purpose",
       x = "Day-ahead price",
       y = "Density") +
  theme_minimal() +
  theme(
    legend.position = c(0.8, 0.8),
    text = element_text(size = 14))
g
ggsave(filename = "price_distrib.pdf", path = outDir, width = 8)
# Discussion: Near normal distribution

jarque.bera.test(price_df$`Day-Ahead`)
# Reject normality


# ------------------------------------------------------------------------------
# Create latex table
# ------------------------------------------------------------------------------


t <- data.frame(c("Solar", "Wind", "Load", "Price"),
                c(mean_s, mean_w, mean_l, mean_p),
                c(median_s, median_w, median_l, median_p),
                c(min_s, min_w, min_l, min_p),
                c(max_s, max_w, max_l, max_p),
                c(sd_s, sd_w, sd_l, sd_p),
                c(sk_s, sk_w, sk_l, sk_p),
                c(ku_s, ku_w, ku_l, ku_p),
                c("<0.01", "<0.01", "<0.01", "<0.01")
)

names(t) <- c("Variable", "Mean", "Median", "Min", "Max", "Standard deviation", "Skewness", "Kurtosis", "JB test pvalue")

t <- t %>%
  mutate_if(is.numeric, ~ scales::number(., accuracy = 0.01))

print(xtable(t, type = "latex"))



# ------------------------------------------------------------------------------
# Distribution across hours
# ------------------------------------------------------------------------------


# Reduce 
# Rename Cable variable
price_dfs <- price_df %>%
  mutate(
    new_cable = if_else(
      cable == 1,
      "Operational",
      "Out-of-service"
    )
  )

# Price
g <- ggplot(price_dfs, aes(x = factor(hour), y = `Day-Ahead`, fill = factor(new_cable))) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 1)) +
  labs(
    title = "Day-ahead price distribution across hours - before and after cable fault",
    subtitle = "In euro per megawatt-hour - y axis truncated for visibility purpose",
    x = "Hour",
    y = "Price",
    fill = "Estlink-2"
  ) + 
  ylim(-70, 600) +
  theme_minimal() + 
  theme(
    legend.position = c(0.9, 0.9),
    text = element_text(size = 14))
g
ggsave(filename = "price_distrib_hours.pdf", path = outDir, width = 9)

# Wind
g <- ggplot(price_df, aes(x = factor(hour), y = gen_df$Wind, fill = factor(new_cable))) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 1)) +
  labs(
    title = "Wind distribution across hours - before and after cable fault",
    subtitle = "In megawatt",
    x = "Hour",
    y = "Wind Power",
    fill = "Estlink-2"
  ) + 
  # ylim(-70, 600) +
  theme_minimal() + 
  theme(
    legend.position = c(0.9, 0.9),
    text = element_text(size = 14))
g

# Solar
g <- ggplot(price_df, aes(x = factor(hour), y = gen_df$Solar, fill = factor(new_cable))) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 1)) +
  labs(
    title = "Solar distribution across hours - before and after cable fault",
    subtitle = "In megawatt",
    x = "Hour",
    y = "Solar Power",
    fill = "Estlink-2"
  ) + 
  # ylim(-70, 600) +
  theme_minimal() + 
  theme(
    legend.position = c(0.9, 0.9),
    text = element_text(size = 14))
g

# Cross-border flows
g <- ggplot(price_df, aes(x = factor(hour), y = flow_df$Ime, fill = factor(new_cable))) +
  geom_boxplot(alpha = 0.6, position = position_dodge(width = 1)) +
  labs(
    title = "Cross-border flows - before and after cable fault",
    subtitle = "Inport - exports, in megawatt",
    x = "Hour",
    y = "Import - Export",
    fill = "Estlink-2"
  ) + 
  # ylim(-70, 600) +
  theme_minimal() + 
  theme(
    legend.position = c(0.9, 0.9),
    text = element_text(size = 14))
g


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

# g <- ggplot(gen_df, aes(y = Solar)) + 
#   geom_boxplot() + 
#   scale_x_discrete() +
#   coord_flip() + 
#   labs(title = "Solar production")
# g

# # Reduce time span to 6 months
# start <- as.POSIXct("2023-01-01 00:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# end <- as.POSIXct("2023-07-31 23:00:00", format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# 
# solar_y <- ts_span(solar, start, end)
# wind_y <- ts_span(wind, start, end)
# load_y <- ts_span(load, start, end)
# price_y <- ts_span(price, start, end)
# rm(solar_y, wind_y, load_y, price_y)


# # Reduce to daily values
# 
# solar_d <- apply.weekly(solar, sum)
# wind_d <- apply.weekly(wind, sum)
# load_d <- apply.weekly(load, sum)
# price_d <- apply.weekly(price, mean)
# 
# # Reduce to weekly values
# 
# solar_w <- apply.weekly(solar, sum)
# wind_w <- apply.weekly(wind, sum)
# load_w <- apply.weekly(load, sum)
# price_w <- apply.weekly(price, mean)
# 
# # Reduce to monthly values
# 
# solar_m <- apply.monthly(solar, sum)
# wind_m <- apply.monthly(wind, sum)
# load_m <- apply.monthly(load, sum)
# price_m <- apply.monthly(price, mean)

# plotACF(solar, lag.max = 30) # Short term seasonality (12h and 24) + low persistence
# plotACF(wind, lag.max = 30) # Trend/Persistence
# plotACF(load, lag.max = 30) # High trend/persistence + Short term seasonality (at 24)
# plotACF(price, lag.max = 30) # Short term seasonality (12h and 24) + medium persistence
# 
# plotACF(solar_d, lag.max = 12) # No seasonality + just persistence
# plotACF(wind_d, lag.max = 12) # No seasonality + just persistence
# plotACF(load_d, lag.max = 12) # No seasonality + just persistence
# plotACF(price_d, lag.max = 12) # Short term seasonality (5 days) + no persistence
# 
# plotACF(solar_w, lag.max = 30) # 6 month seasonality + persistence
# plotACF(wind_w, lag.max = 30) # Low persistence + seasonality each 3 weeks
# plotACF(load_w, lag.max = 30) # 6 month seasonality + persistence
# plotACF(price_w, lag.max = 30) # no more persistence! + evidence of some week autocorrelation
# 
# plotACF(solar_m, lag.max = 24) # 6 month seasonality confirmed
# plotACF(wind_m, lag.max = 24) # Stationary, seasonality and persistence no more present
# plotACF(load_m, lag.max = 24) # 6 month seasonality confirmed + weak persistence
# plotACF(price_m, lag.max = 24) # no more persistence! + evidence of some week autocorrelation
# 
# # Conclusion: 
# # Short term seasonality at 12h and 24h for load, solar and price as expected
# # Long term seasonality at 6 months for solar and load
# # TS are highly persistent, like trend, include time running variable 
# 
# rm(solar_d, solar_w, solar_m, wind_d, wind_w, wind_m, 
#    load_d, load_w, load_m, price_d, price_w, price_m)

# # ADF
# 
# # Solar 
# 
# # Null: TS has a unit root (non stationary, random walk)
# sol_drift <- CADFtest(solar, max.lag.y = 24, type = "drift")
# summary(sol_drift)
# # Discussion: reject at 1% that ts is non stationary
# 
# 
# # Wind 
# 
# # Null: TS has a unit root (non stationary, random walk)
# wind_drift <- CADFtest(wind, max.lag.y = 24, type = "drift")
# summary(wind_drift)
# # Discussion: reject at 1% that ts is non stationary
# 
# 
# # Load 
# 
# # Null: TS has a unit root (non stationary, random walk)
# load_drift <- CADFtest(load, max.lag.y = 24, type = "drift")
# summary(load_drift)
# # Discussion: reject at 1% that ts is non stationary
# 
# 
# # Price 
# 
# # Null: TS has a unit root (non stationary, random walk)
# price_drift <- CADFtest(price, max.lag.y = 24, type = "drift")
# summary(price_drift)
# # Discussion: reject at 1% that ts is non stationary

# gen_df <- gen_df %>%
#   mutate(
#     # hour = as.numeric(hour(DateTime)),
#     # day = day(DateTime),
#     # day = as.numeric(floor(as.double(difftime(DateTime, init_date, units = "days") + 1))),
#   )
# 
# 
# load_df <- load_df %>%
#   mutate(
#     # hour = as.character(hour(DateTime)),
#     # day = day(DateTime),
#     # day = as.numeric(floor(as.double(difftime(DateTime, init_date, units = "days") + 1)))
#   )