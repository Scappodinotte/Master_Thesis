 
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
* Summarise variables
* ------------------------------------------------------------------------------
xtsum solar wind load price cable


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
* Model 1
* ------------------------------------------------------------------------------

* --------- apply MM-QR model 1 using bootstrap --------- 

foreach q in 1 2 3 4 5 6 7 8 9 {
	bootstrap, cluster(hour) rep(200) seed(123): xtqreg price solar wind load cable lag_p holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec, quantile(0.`q')
	
	* save results into eststo
	eststo q_`q'
}


* --------- Produce table for quantile ---------

esttab q_1 q_2 q_3 q_4 q_5 q_6 q_7 q_8 q_9 using "${path}/quantile_1.tex", replace ///
  se(3) b(3) label ///
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


* --------- apply MM-QR model 1 using bootstrap on location --------- 

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
bootstrap _b, reps(200) cluster(hour) seed(123): xtqreg_loc_boot price solar wind load cable holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec lag_p

* Save location results
eststo loc


* --------- apply MM-QR model 1 using bootstrap on scale --------- 

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
bootstrap _b, reps(200) cluster(hour) seed(123): xtqreg_sca_boot price solar wind load cable holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec lag_p

* Save scale results
eststo sca


* --------- Produce table for location and scale --------- 

esttab loc sca using "${path}/location_scale_1.tex", replace ///
  se(3) b(3) label ///
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



* ------------------------------------------------------------------------------
* Model 2
* ------------------------------------------------------------------------------

* --------- apply MM-QR model 2 using bootstrap --------- 

* gives bootstrap and clustered se but coef for quantile
foreach q in 1 2 3 4 5 6 7 8 9 {
	bootstrap, cluster(hour) rep(200) seed(123): xtqreg price c.solar##i.cable c.wind##i.cable load lag_p holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec, quantile(0.`q')
	eststo q_`q'
	xlincom solar + 1.cable#c.solar
	xlincom wind + 1.cable#c.wind
}


* --------- Produce table for quantile ---------
 
esttab q_1 q_2 q_3 q_4 q_5 q_6 q_7 q_8 q_9 using "${path}/quantile_2.tex", replace ///
  se(3) b(3) label ///
  alignment(lp{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}p{1.5cm}) ///
  star(* 0.10 ** 0.05 *** 0.01) ///
  title("Estimates of quantiles") ///
  mtitles("0.1" "0.2" "0.3" "0.4" "0.5" "0.6" "0.7" "0.8" "0.9") /// 
  keep(solar wind load 1.cable 1.cable#c.solar 1.cable#c.wind) ///
  coeflabels(solar "Solar" wind "Wind" load "Load" 1.cable "Cable" 1.cable#c.solar "Solar x Cable" 1.cable#c.wind "Wind x Cable") /// 
  nonum ///
  noobs ///
  booktabs

* clear eststo memory
eststo clear


* --------- apply MM-QR model 2 using bootstrap on location --------- 

gen sol_cable = solar * cable
gen wind_cable = wind * cable

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
bootstrap _b, reps(200) cluster(hour) seed(123): xtqreg_loc_boot price solar wind load cable sol_cable wind_cable lag_p holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec

* Save location results
eststo loc


* --------- apply MM-QR model 2 using bootstrap on scale --------- 

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
bootstrap _b, reps(200) cluster(hour) seed(123): xtqreg_sca_boot price solar wind load cable sol_cable wind_cable lag_p holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec

* Save scale results
eststo sca


* --------- Produce table for location and scale --------- 

esttab loc sca using "${path}/location_scale_2.tex", replace ///
  se(3) b(3) label ///
  alignment(lp{1.5cm}p{1.5cm}) ///
  star(* 0.10 ** 0.05 *** 0.01) ///
  title("Location and scale") ///
  mtitles("Location" "Scale") /// 
  keep(solar wind load cable sol_cable wind_cable) ///
  coeflabels(solar "Solar" wind "Wind" load "Load" cable "Cable" sol_cable "Solar x Cable" wind_cable "Wind x Cable") /// 
  nonum ///
  noobs ///
  booktabs

* clear eststo memory
eststo clear



* ------------------------------------------------------------------------------
* Model 3
* ------------------------------------------------------------------------------

* clear eststo memory
eststo clear

gen sol_b = solar * base
gen sol_i = solar * inter
gen sol_p = solar * peak

gen wind_b = wind * base
gen wind_i = wind * inter
gen wind_p = wind * peak

gen cable_b = cable * base
gen cable_i = cable * inter
gen cable_p = cable * peak


* --------- apply MM-QR model 3 using bootstrap on location --------- 

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
bootstrap _b, reps(200) cluster(hour) seed(123): xtqreg_loc_boot price sol_b sol_i sol_p wind_b wind_i wind_p cable_b cable_i cable_p load lag_p holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec

* Save location results
eststo loc


* --------- apply MM-QR model 3 using bootstrap on location ---------

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
bootstrap _b, reps(200) cluster(hour) seed(123): xtqreg_sca_boot price sol_b sol_i sol_p wind_b wind_i wind_p cable_b cable_i cable_p load lag_p holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec

* Save scale results
eststo sca



* ------------------------------------------------------------------------------
* Rubustness check
* ------------------------------------------------------------------------------

* Get residuals and plot
xtqreg price solar wind load cable lag_p holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec, ls q(0.5) predict(yhat)

gen residuals = price - yhat__5
gen residuals_p2 = residuals^2
gen residuals_p3 = residuals^3

scatter residuals solar 
scatter residuals_p2 solar if residuals_p2 <= 100000
scatter residuals_p3 solar if residuals_p3 <= 5000000 & residuals_p3 >= -5000000

scatter residuals wind
scatter residuals_p2 wind if residuals_p2 <= 100000
scatter residuals_p3 wind if residuals_p3 <= 5000000 & residuals_p3 >= -5000000

scatter residuals load
scatter residuals_p2 load if residuals_p2 <= 200000
scatter residuals_p3 load

graph combine s1 s2 s3 w1 w2 w3 l1 l2 l3, rows(3) name(gg)

graph drop _all

* Discussion : Autocorrelation of the residuals

* Driscoll-Kraay standard errors
tsset hour day
xtscc price solar wind load cable lag_p holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec, fe lag(4)

predict yhat_dk
gen res_dk = price - yhat_dk

foreach i in 0 3 5 12 15 18 23 {
	ac residuals if hour == `i', ytitle("ac_mmqr hour `i'") name(ac_mmqr_`i')
	ac res_dk if hour == `i', ytitle("ac_dk hour `i'") name(ac_dk_`i')
}

graph combine ac_0 ac_2 ac_4 ac_6 ac_8 ac_10 ///
 ac_12 ac_14 ac_16 ac_18 ac_20 ac_22, rows(6) name(ac)

graph export ac_res.pdf, name(ac) replace

graph drop _all

* compare with Discroll Krai regression 

twoway (scatter residuals solar  if residuals < 100 & residuals > -100) (scatter res_dk solar if res_dk < 100 & res_dk > -100)

twoway (scatter residuals solar) (scatter res_dk solar)

twoway (scatter residuals wind) (scatter res_dk wind)

twoway (scatter residuals load) (scatter res_dk load)

* Short sample - Power flow instead of dummy
bootstrap, cluster(hour) rep(200) seed(123): xtqreg price solar wind load ime lag_p holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec, ls quantile(0.5)
* Discussion: model 1 with Finland IME (import – export) : cable difference in power effect of -11.52 instead of -3.62 at Q0.5

* Full sample - Pskov cable
foreach q in 1 2 3 4 5 6 7 8 9 {
	bootstrap, cluster(hour) rep(200) seed(123): xtqreg price solar wind load cable lag_p holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec, quantile(0.`q')
}
* Discussion: cable effect insignificant for each quantile

* Full sample - Russian import & exports
foreach q in 1 2 3 4 5 6 7 8 9 {
	bootstrap, cluster(hour) rep(200) seed(123): xtqreg price solar wind load cable ime_ru lag_p holiday mon tue thu fri sat sun jan feb mar may jun jul aug sep oct nov dec, quantile(0.`q')
}
* Discussion: Negative effect on low quantile and high positive effect on high quantile