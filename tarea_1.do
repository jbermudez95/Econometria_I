***** TAREA 1 ECONOMETRIA *****

cd "C:\Users\crist\OneDrive - Universidad Católica de Chile\Cristóbal\ME\1º Semestre\Econometría\Tareas\Tarea 1"

use dataset.billonaires.dta, clear

*-- Pregunta 1

sort country

drop if year!=2013

replace	numbil=0 if numbil==.

merge 1:1 country using pop.dta, keepusing(population)

drop _merge

drop if country=="Chile"
drop if population==. 

gen bill_permill = (numbil / population) * 1000000

keep year numbil bill_permil country midinc08 lowinc08 yearsinWTO wtoyear totppb9008 population

*-- Pregunta 2

histogram bill_permill, bin(5) frequency ytitle(Frecuencia) xtitle(Número de Billonario por Millon de Habitantes) title(Histograma del Número de Billonarios por Millón de Habitantes)


list country if bill_permill >= 1

gen category = cond( lowinc08 == 1, "Low Income", cond(midinc08 == 1, "Meddle Income", "High Income"))

tabstat bill_permill, s(mean median p75 p90) by(category)


*-- Pregunta 3

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

*hola hola hola






















