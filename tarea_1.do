/*
***** TAREA 1 ECONOMETRIA *****

Grupo conformado por:
Francisca Villegas
Cristóbal Pérez
Jose Carlo Bermúdez jojo
*/

log using log_tarea1.smcl, replace

cap which estout
if (_rc) ssc install estout														// Paquete necesario para generar tablas en formato de Latex

* Cargando directorios de los participantes del grupo
if "`c(username)'" == "crist" {
	cd "C:\Users\crist\OneDrive - Universidad Católica de Chile\Cristóbal\ME\1º Semestre\Econometría\Tareas\Tarea 1"
}
else if "`c(username)'" == "Jose Carlo Bermúdez" {
	cd "C:\Users\bermu\OneDrive - Universidad Católica de Chile\Clases\Econometría\Tareas\Tarea1"
}
else if "`c(username)'" == "franc" {
	cd "C:\Tarea1_Econometría"
}

use dataset.billonaires.dta, clear

********************************************
*-- Pregunta 1
********************************************

* Inciso a
sort country
drop if year!=2013

* Inciso b
replace	numbil=0 if numbil==.

* Inciso c
merge 1:1 country using pop.dta, keepusing(population)

* Inciso d
drop if population == .
drop _merge

* Inciso e
gen bill_permill = (numbil / population) * 1000000

* Inciso f
keep year numbil bill_permil country midinc08 lowinc08 yearsinWTO wtoyear totppb9008 population



********************************************
*-- Pregunta 2
********************************************

histogram bill_permill, bin(10) frequency ytitle(Frecuencia) xtitle(Número de Billonario por Millon de Habitantes) color(green)

list country if bill_permill >= 1 & !missing(bill_permill)

gen category = cond( lowinc08 == 1, "Low Income", cond(midinc08 == 1, "Meddle Income", "High Income"))

tabstat bill_permill, s(mean median p75 p90) by(category)




********************************************
*-- Pregunta 3
********************************************

* a

gen lpop = log(population)

forvalues i = 1995/2015 {
    gen tlc_`i' = (wtoyear == `i')
}

eststo drop *

eststo: reg numbil lpop yearsinWTO

esttab using "pregunta_3_a.tex", replace f booktabs nonumbers mtitles("numbill") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
        scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(lpop "log(population)" _cons "Constante") 

* b

eststo drop *

eststo: reg numbil lpop yearsinWTO tlc*

eststo: reg numbil lpop tlc*

eststo: reg numbil lpop yearsinWTO tlc_1995 - tlc_2014

eststo: reg numbil lpop tlc_1995 - tlc_2014

esttab using "pregunta_3_b.tex", replace f booktabs nonumbers mtitles("numbill" "numbill" "numbill" "numbill") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
        scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(lpop "log(population)" _cons "Constante") 


********************************************
*-- Pregunta 4
********************************************

* Inciso a
eststo drop *
eststo: reg numbil lpop yearsinWTO
eststo: reg numbil lpop yearsinWTO totppb9008 

esttab using "pregunta_4a.tex", replace f booktabs nonumbers mtitles("numbill" "numbill") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
        scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(lpop "log(population)" _cons "Constante") 

* Inciso b
replace yearsinWTO = 59   if country == "Chile"
replace totppb9008 = 2.75 if country == "Chile"

reg numbil lpop yearsinWTO totppb9008
predict numbil_predicho_1, xb
predict numbil_std_1, stdp

global t_student = invttail(e(df_r), 0.025)										

gen error_estimacion_1 = ${t_student} * (numbil_std_1 / sqrt(e(N)))

gen limite_inf_1 = numbil_predicho_1 - error_estimacion_1
gen limit_sup_1  = numbil_predicho_1 + error_estimacion_1

list limite_inf_1 numbil_predicho_1 limit_sup_1 if country == "Chile"



********************************************
*-- Pregunta 5
********************************************

* Inciso a
clear

import excel "names.xlsx", sheet("Billionaires 2015") firstrow
gen year_2015 = 2015
order name year_2015 privatization
save "Billionaires 2015.dta", replace

clear

import excel "names.xlsx", sheet("Billonaires 1995") firstrow clear
gen year_1995 = 1995
gen privatization = .
order name year_1995 privatization
save "Billonaires 1995.dta", replace

clear

use "Billonaires 1995.dta"
merge 1:1 (name) using "Billionaires 2015.dta", update
order name year_1995 year_2015 privatization
save "Billionaires_Rusia.dta", replace

* Inciso b
gen old_billionare = 0
replace old_billionare = 1 if year_1995 == 1995 
replace old_billionare = 0 if (year_2015 != 2015)

* Inciso c
replace privatization = 0 if privatization==.

* Inciso d
eststo drop *

eststo: reg old_billionare privatization

esttab using "pregunta_5_d.tex", replace f booktabs nonumbers mtitles("old billionaire") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
        scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(privatization "privatization" _cons "Constante") 


log close
translate log_tarea1.smcl log_tarea1.pdf, translator(smcl2pdf)


