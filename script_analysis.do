clear all

cd "C:\Users\elias\Documents\Personale\UNINE\Master_Applied_Economics\Master_Thesis\Data"

* import data sets and save in dta format
import delimited Stata_df.csv
save data, replace

* summarise variables
summarize solar wind load price

* specify cross sectional and time dimension
tsset hour day

* execute Pesaran (2003) unit root test for panel data with cross-section dependence. NULL: that all series are non-stationary

pescadf solar, lags(1)

pescadf wind, lags(1)

pescadf load, lags(1)

pescadf price, lags(1)

* Null is rejected for all ts at p-value lower than 1%
