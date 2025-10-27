 
* ------------------------------------------------------------------------------
* Master Thesis 
* Elia Scapini
* Neuchâtel, 24/10/2025
* Analysis STATA script
* ------------------------------------------------------------------------------


* ------------------------------------------------------------------------------
* Clear memory
* ------------------------------------------------------------------------------
clear
cls
eststo clear


* ------------------------------------------------------------------------------
* Set working directory and seed
* ------------------------------------------------------------------------------
cd "C:\Users\elias\Documents\Personale\UNINE\Master_Applied_Economics\Master_Thesis\Data"

global path "C:/Users/elias/Documents/Personale/UNINE/Master_Applied_Economics/Master_Thesis/Results"

set seed 123


* ------------------------------------------------------------------------------
* Import data sets and save in dta format
* ------------------------------------------------------------------------------
import delimited Stata_df.csv
save data, replace


* ------------------------------------------------------------------------------
* Specify cross sectional and time dimension
* ------------------------------------------------------------------------------
xtset hour day


* ------------------------------------------------------------------------------
* Summarise variables and create one-day-lagged price 
* ------------------------------------------------------------------------------
xtsum solar wind load price cable
by hour gen lag_p = price[_n-1]


* ------------------------------------------------------------------------------
* Panel unit root test
* ------------------------------------------------------------------------------
* execute Pesaran (2003) unit root test for panel data with cross-section dependence. NULL: that all series are non-stationary
pescadf solar, lags(1)

pescadf wind, lags(1)

pescadf load, lags(1)

pescadf price, lags(1)

* Discussion: Null is rejected for all ts at p-value lower than 1%


* ------------------------------------------------------------------------------
* Reset cross sectional
* ------------------------------------------------------------------------------
xtset hour
* Discussion : time dimension is not set otherwise xtqreg wont work


* ------------------------------------------------------------------------------
* apply plain MM-QR
* ------------------------------------------------------------------------------
* gives location and scale but no bootstrap and clustered se
xtqreg price solar wind load cable holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec, ls q(0.1)


* ------------------------------------------------------------------------------
* apply MM-QR using bootstrap
* ------------------------------------------------------------------------------
* gives bootstrap and clustered se but coef for quantile
foreach q in 1 2 3 4 5 6 7 8 9 {
	bootstrap, cluster(hour) rep(200) seed(123): xtqreg price solar wind load cable holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec, ls quantile(0.`q')
	
	*save results into eststo
	eststo q_`q'
}

*erase "${path}/quantile.tex"

esttab q_1 q_2 q_3 q_4 q_5 q_6 q_7 q_8 q_9 using "${path}/quantile.tex", replace ///
  se(3) b(2) label ///
  alignment(lp{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}) ///
  star(* 0.10 ** 0.05 *** 0.01) ///
  title("Estimates of quantiles") ///
  mtitles("0.1" "0.2" "0.3" "0.4" "0.5" "0.6" "0.7" "0.8" "0.9") /// 
  keep(solar wind load cable) ///
  coeflabels(solar "Solar" wind "Wind" load "Load" cable "Cable") /// 
  nonum ///
  noobs ///
  booktabs

* clear eststo memory
eststo clear

* ------------------------------------------------------------------------------
* apply MM-QR using bootstrap on location
* ------------------------------------------------------------------------------
* Extract LOCATION coefficient and clustered and bootstrapped se
capture program drop xtqreg_loc_boot 	// delete previous similar programs
program define xtqreg_loc_boot, eclass 	// new propgram on e class
	syntax varlist(min=2) 				// store y and x in varlist
	gettoken y xvars : varlist			// split y and x

	quietly xtqreg `y' `xvars', quantile(0.5) ls	// run xtqreg on varlist 
	
	* Extract coefficient matrices
    matrix b_loc = e(b_location)		// 1 x k_loc point est. location
	ereturn post b_loc
end

* Bootstrap (clustered) over the wrapper
bootstrap _b, reps(200) cluster(hour) seed(123): xtqreg_loc_boot price solar wind load cable

* Save location results
eststo loc

* ------------------------------------------------------------------------------
* apply MM-QR using bootstrap on scale
* ------------------------------------------------------------------------------
* Extract SCALE coefficient and clustered and bootstrapped se
capture program drop xtqreg_sca_boot	// delete previous similar programs
program define xtqreg_sca_boot, eclass	// new propgram on e class
syntax varlist(min=2)					// store y and x in varlist
	gettoken y xvars : varlist			// split y and x

	quietly xtqreg `y' `xvars', quantile(0.5) ls
	
	* Extract coefficient matrices
	matrix b_sca = e(b_scale)      		// 1 x k_sca point est. scale
	ereturn post b_sca
end

* Bootstrap (clustered) over the wrapper
bootstrap _b, reps(200) cluster(hour) seed(123): xtqreg_sca_boot price solar wind load cable

* Save scale results
eststo sca


* ------------------------------------------------------------------------------
* Produce table for location and scale
* ------------------------------------------------------------------------------
esttab loc sca using "${path}/location_scale.tex", replace ///
  se(3) b(2) label ///
  alignment(lp{1.5cm}p{1.5cm}) ///
  star(* 0.10 ** 0.05 *** 0.01) ///
  title("Location and scale") ///
  mtitles("Location" "Scale") /// 
  keep(solar wind load cable) ///
  coeflabels(solar "Solar" wind "Wind" load "Load" cable "Cable") /// 
  nonum ///
  noobs ///
  booktabs

* clear eststo memory
eststo clear


