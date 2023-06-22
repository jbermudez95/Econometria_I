
**Tarea 2 
log using log_tarea2.smcl, replace

cap which estout
if (_rc) ssc install estout														// Paquete necesario para generar tablas en formato de Latex

* Cargando directorios de los participantes del grupo
if "`c(username)'" == "crist" {
	cd "C:\Users\crist\OneDrive - Universidad Católica de Chile\Cristóbal\ME\1º Semestre\Econometría\Tareas\Tarea 1"
}
else if "`c(username)'" == "Jose Carlo Bermúdez" {
	cd "C:\Users\bermu\OneDrive - Universidad Católica de Chile\Clases\Econometría\Tareas\Tarea2"
}
else if "`c(username)'" == "franc" {
	cd "C:\Users\franc\Desktop\Magíster en Economía\Econometría\Tareas\Tarea 2"
}

**log(ventas)= B0+B1log(Creditos)+ui**

****************************
**Ejercicio 1
****************************

use "tarea_ej1_alumnos-1.dta"

gen log_ventas   = ln(ventas)
gen log_creditos = ln(creditos)

*1 Scatter Plot
qui reg log_ventas log_creditos
loc b0: dis %4.2fc _b[_cons]
loc b1: dis %4.2fc _b[log_creditos]
twoway (scatter log_ventas log_creditos, mcolor(blue%20)) (lfit log_ventas log_creditos, lc(dknavy)), text(7 2 "log(Ventas)=`b0'+`b1'log(Créditos)") ytitle("Log(Ventas)") legend(off) ///
	   yscale(titlegap(3)) xtitle("Log(Créditos)") xscale(titlegap(3)) yscale(titlegap(3)) graphr(color(white))
graph export "scatter.pdf", replace


*2
eststo drop *
eststo: reg log_ventas log_creditos 
esttab using "pregunta_1_1.tex", replace f booktabs nonumbers mtitles("log(Ventas)") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
       scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(log_creditos "log(Créditos)" _cons "Constante") 

*4
gen log_numberescalones = ln(numberescalones)

eststo drop *
eststo m1: reg log_creditos exporter
eststo m2: reg log_creditos log_numberescalones
esttab m1 m2 using "pregunta_1_4.tex", replace f booktabs nonumbers mtitles("log(Créditos)" "log(Créditos)") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
       scalars("N N" "r2 R$^2$" "F F-Statistic") coeflabels(log_numberescalones "log(\# Escalones)" exporter "Exporter = 1" _cons "Constante") 

	   
*5

*6

*7

*8

*9

*10

