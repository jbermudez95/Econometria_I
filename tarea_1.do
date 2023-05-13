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

histogram bill_permill, bin(5) frequency ytitle(Frecuencia) xtitle(Número de Billonario por Millon de Habitantes) title(Histograma del Número de Billonarios por Millón de Habitantes)

list country if bill_permill >= 1 & !missing(bill_permill)

gen category = cond( lowinc08 == 1, "Low Income", cond(midinc08 == 1, "Meddle Income", "High Income"))

tabstat bill_permill, s(mean median p75 p90) by(category)




********************************************
*-- Pregunta 3
********************************************

gen lpop = log(population)

reg numbil lpop yearsinWTO

forvalues i = 1995/2015 {
    gen tlc_`i' = (wtoyear == `i')
}


local tlc_dummies ""

forvalues year = 1995/2014 {
    local tlc_dummies "`tlc_dummies' tlc_`year'"
}


reg numbil lpop yearsinWTO `tlc_dummies'



********************************************
*-- Pregunta 4
********************************************

reg numbil lpop yearsinWTO totppb9008
global b_pop = _b[lpop]
global b_wto = _b[yearsinWTO]
global b_tot = _b[totppb9008]
global const = _b[_cons]

replace yearsinWTO = 59   if country == "Chile"
replace totppb9008 = 2.75 if country == "Chile"

replace numbil = ${const} + (${b_pop} * lpop) + (${b_wto} * yearsinWTO) + (${b_tot} * totppb9008) if country == "Chile" 
replace numbil = round(numbil) if country == "Chile"



********************************************
*__Pregunta 5
********************************************

import excel "C:\Tarea1_Econometría\names.xlsx", sheet("Billionaires 2015") firstrow
save "C:\Tarea1_Econometría\Billionaires 2015.dta", replace

clear

import excel "C:\Tarea1_Econometría\names.xlsx", sheet("Billonaires 1995") firstrow clear
save "C:\Tarea1_Econometría\Billonaires 1995.dta", replace






















