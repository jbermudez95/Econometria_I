
**Tarea 2 
clear

*log using log_tarea2.smcl, replace

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
twoway (scatter log_ventas log_creditos, mcolor(blue%20))     ///
		(lfit log_ventas log_creditos, lc(dknavy)) 		   	  ///
		(lfit log_ventas log_ventas, lc(red) lp(dash)),       ///
		ytitle("Log(Ventas)") yscale(titlegap(3)) 			  ///
	    xtitle("Log(Créditos)") xscale(titlegap(3)) yscale(titlegap(3)) graphr(color(white)) ///
		xlabel(0(1.4)7) ylabel(0(1.4)7) legend(pos(4) ring(0) col(1) ///
		order(2 "Función de Regresión Muestral" 3 "Recta 45°") region(fcolor(white)))
graph export "scatter_1.pdf", replace


*2q
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
gen log_distbank = ln(dist_bank)
twoway (scatter log_distbank log_creditos , mcolor(blue%20)) (lfit log_distbank log_creditos, lc(dknavy)), ytitle("Log(Créditos)") legend(off) ///
	   yscale(titlegap(3)) xtitle("Log(Distancia)") xscale(titlegap(3)) yscale(titlegap(3)) graphr(color(white))
graph export "scatter_5.pdf", replace

*6

eststo drop *
eststo m1: reg log_creditos log_distbank, robust
predict log_creditos_predict, xb
eststo m2: reg log_ventas log_creditos_predict, robust
esttab m1 m2 using "pregunta_1_6.tex", replace f booktabs nonumbers mtitles("1era Etapa" "2da Etapa") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
       scalars("N N" "r2 R$^2$" "F F-Statistic") coeflabels(log_distbank "log(Distancia)" log_creditos_predict "\hat{Log(Creditos)}" _cons "Constante") 

*7
eststo drop *
eststo: ivregress 2sls log_ventas (log_creditos=log_distbank), robust
esttab using "pregunta1_7.tex", replace f booktabs nonumbers mtitles (log_distbank "log(Distancia)") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
        scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels("log_creditos" "\hat{Log(Creditos)}" _cons "Constante")
		
*8
eststo drop *
eststo m1: reg log_creditos log_distbank, robust
*predict res_hausman, residuals 
eststo m2: reg log_ventas log_creditos res_hausman, robust
esttab m1 m2 using "pregunta_1_8.tex", replace f booktabs nonumbers mtitles("1era Etapa" "2da Etapa") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
       scalars("N N" "r2 R$^2$" "F F-Statistic") coeflabels(log_distbank "log(Distancia)" log_creditos_predict "{Log(Creditos)}" _cons "Constante") 
	   
ivregress 2sls log_ventas (log_creditos=log_distbank), robust
estat endogenous


*9

*10

