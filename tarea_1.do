/*
***** TAREA 1 ECONOMETRIA *****

Grupo conformado por:
Francisca Villegas
Cristóbal Pérez
Jose Carlo Bermúdez
*/

if "`c(username)'" == "crist" {
	cd "C:\Users\crist\OneDrive - Universidad Católica de Chile\Cristóbal\ME\1º Semestre\Econometría\Tareas\Tarea 1"
}
else if "`c(username)'" == "Jose Carlo Bermúdez" {
	cd "C:\Users\bermu\OneDrive - Universidad Católica de Chile\Clases\Econometría\Tareas\Tarea1"
}
else if "`c(username)'" == "franc" {
	cd "C:\Tarea1_Econometría"
}

*log using log_tarea1.smcl, replace
use dataset.billonaires.dta, clear

********************************************
*-- Pregunta 1
********************************************

sort country

drop if year!=2013

replace	numbil=0 if numbil==.

merge 1:1 country using pop.dta, keepusing(population)
drop if population == .
drop _merge

gen bill_permill = (numbil / population) * 1000000

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

esttab using "pregunta_3_a.tex", replace f booktabs nomtitles se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
        scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(lpop "log(population)" _cons "Constante") 

* b

eststo drop *

eststo: reg numbil lpop yearsinWTO tlc*

eststo: reg numbil lpop tlc*

esttab using "pregunta_3_b.tex", replace f booktabs nomtitles se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
        scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(lpop "log(population)" _cons "Constante") 


********************************************
*-- Pregunta 4
********************************************

* a
eststo drop *
eststo: reg numbil lpop yearsinWTO
eststo: reg numbil lpop yearsinWTO totppb9008 

esttab using "pregunta_4a.tex", replace f booktabs nomtitles se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
        scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(lpop "log(population)" _cons "Constante") 

* b
replace yearsinWTO = 59   if country == "Chile"
replace totppb9008 = 2.75 if country == "Chile"

* Forma "sencilla" 
reg numbil lpop yearsinWTO totppb9008
predict numbil_predicho_1, xb
predict numbil_std_1, stdp

global t_student = invttail(e(df_r), 0.025)										// Esta "t-student" es la misma para cualquiera de las dos formas

gen error_estimacion_1 = ${t_student} * (numbil_std_1 / sqrt(e(N)))

gen limite_inf_1 = numbil_predicho_1 - error_estimacion_1
gen limit_sup_1  = numbil_predicho_1 + error_estimacion_1

* Forma "manual"
reg numbil lpop yearsinWTO totppb9008
global b_pop = _b[lpop]
global b_wto = _b[yearsinWTO]
global b_tot = _b[totppb9008]
global const = _b[_cons]
mat V = e(V)

gen numbil_predicho_2 = ${const} + (${b_pop} * lpop) + (${b_wto} * yearsinWTO) + (${b_tot} * totppb9008)

gen numbil_std_2 = V[4,4] + ((16.68453)^2 * V[1,1]) + ((59)^2 * V[2,2]) + ((2.41)^2 * V[3,3]) + ///
				   ((2*16.68453) * V[4,1]) + ((2*59) * V[4,2]) + ((2*2.41) * V[4,3]) + 		 ///
				   ((2*16.68453*59) * V[2,1]) + ((2*16.68453*2.41) * V[3,1]) + ((2*59*2.41) * V[3,2])
replace numbil_std_2 = sqrt(numbil_std_2)

gen error_estimacion_2 = ${t_student} * (numbil_std_2 / sqrt(e(N)))

gen limite_inf_2 = numbil_predicho_2 - error_estimacion_2
gen limit_sup_2  = numbil_predicho_2 + error_estimacion_2

list limite_inf_1 numbil_predicho_1 limit_sup_1 if country == "Chile"
list limite_inf_2 numbil_predicho_2 limit_sup_2 if country == "Chile"			// Ambas formas conducen a la misma estimación :)




********************************************
*__Pregunta 5
********************************************

*a
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
merge 1:1 (name) using "Billionaires 2015.dta"
order name year_1995 year_2015 privatization
save "Billionaires_Rusia.dta", replace

*b
gen old_billionare = 0
replace old_billionare = 1 if year_1995 == 1995 
replace old_billionare = 0 if (year_2015 != 2015)

*c
replace privatization = 0 if privatization==.

*d
eststo drop *

eststo: reg old_billionare privatization

esttab using "pregunta_5_d.tex", replace f booktabs nomtitles se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
        scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(privatization "privatization" _cons "Constante") 


log close
translate log_tarea1.smcl log_tarea1.pdf, translator(smcl2pdf)


