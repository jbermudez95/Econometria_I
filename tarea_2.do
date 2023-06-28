
**Tarea 2 
clear

*log using log_tarea2.smcl, replace

cap which estout
if (_rc) ssc install estout														// Paquete necesario para generar tablas en formato de Latex

* Cargando directorios de los participantes del grupo
if "`c(username)'" == "crist" {
	cd "C:\Users\crist\OneDrive - Universidad Católica de Chile\Cristóbal\ME\1º Semestre\Econometría\Tareas\Tarea 2"
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
       scalars("N N" "r2 R$^2$" "F F-Statistic") coeflabels(log_distbank "log(Distancia)" log_creditos_predict "$\hat{Log(Creditos)}$" _cons "Constante") 

*7
eststo drop *
eststo: ivregress 2sls log_ventas (log_creditos=log_distbank), robust
esttab using "pregunta_1_7.tex", replace f booktabs nonumbers mtitles("log(Ventas)") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
        scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(log_creditos "$\hat{Log(Creditos)}$" _cons "Constante")
		
*8
eststo drop *
eststo m1: reg log_creditos log_distbank, robust
predict res_hausman, residuals 
eststo m2: reg log_ventas log_creditos res_hausman, robust
esttab m1 m2 using "pregunta_1_8.tex", replace f booktabs nonumbers mtitles("Log(Creditos)" "Log(Ventas)") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
       scalars("N N" "r2 R$^2$" "F F-Statistic") coeflabels(log_distbank "log(Distancia)" log_creditos "Log(Creditos)" res_hausman "$\hat{Residuos}$" _cons "Constante") 
	   
ivregress 2sls log_ventas (log_creditos=log_distbank), robust
estat endogenous


****************************
**Ejercicio 2
****************************

* 1)

use tarea_ej2_alumnos-1, clear

sort empresa year

gen tratamiento = 0

replace tratamiento = 1 if MW_2014 >= 50

preserve

keep if year == 2019

gen paga_impuesto = tratamiento

eststo drop *
eststo: reg lnemisiones paga_impuesto
esttab using "pregunta_2_1.tex", replace f booktabs nonumbers mtitles("log(Emisiones)") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
       scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(paga_impuesto "Paga Impuesto" _cons "Constante")

* 2)	   
	   
* 3)

tabulate sector, generate(dummies)

rename dummies1 agricultura
rename dummies2 electricidad
rename dummies3 manufacturas
rename dummies4 servicios


** a)

eststo drop *
eststo: reg tratamiento agricultura manufacturas servicios, robust
esttab using "pregunta_2_3_a.tex", replace f booktabs nonumbers mtitles("Tratamiento") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
       scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado" "F F-Statistic") coeflabels(agricultura "agricultura" manufacturas "manufacturas" servicios "servicios" _cons "Constante")

** b)

eststo drop *
eststo: reg lnemisiones agricultura manufacturas servicios, robust
esttab using "pregunta_2_3_b.tex", replace f booktabs nonumbers mtitles("log(emisiones)") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
       scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado" "F F-Statistic") coeflabels(agricultura "agricultura" manufacturas "manufacturas" servicios "servicios" _cons "Constante")

** c) 

eststo drop *
eststo: reg lnemisiones paga_impuesto agricultura manufacturas servicios, robust
esttab using "pregunta_2_3_c.tex", replace f booktabs nonumbers mtitles("log(emisiones)") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
       scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado" "F F-Statistic") coeflabels(paga_impuesto "paga_impuesto" agricultura "agricultura" manufacturas "manufacturas" servicios "servicios"  _cons "Constante")


restore

* 4) 

xtset empresa year

* I)

gen post = 0
replace post = 1 if year == 2019

gen int_trat_post = tratamiento*post

eststo drop *
eststo: reg lnemisiones tratamiento post int_trat_post, cluster(empresa)
esttab using "pregunta_2_4.tex", replace f booktabs nonumbers mtitles("log(emisiones)") se(2) b(3) star(* 0.10 ** 0.05 *** 0.01) ///
       scalars("N N" "r2 R$^2$" "r2_a R$^2$-Ajustado") coeflabels(tratamiento "tratamiento" post "post" int_trat_post "int_trat_post" _cons "Constante")

* VI)

use tarea_ej2_alumnos-1, clear

append using tarea_ej2_alumnos_parte2-1.dta

sort empresa year

gen tratamiento = 0

replace tratamiento = 1 if MW_2014 >= 50

collapse (mean) lnemisiones, by(tratamiento year)

twoway (line lnemisiones year if tratamiento == 1, lpattern(dash) lcolor(blue)) ///
       (line lnemisiones year if tratamiento == 0, lpattern(dash) lcolor(red)), ///
       xline(2014, lcolor(gray)) ///
       xtitle("Año") xscale(titlegap(3)) yscale(titlegap(3)) graphr(color(white)) ///
	   ytitle("Emisiones Promedio en Log") yscale(titlegap(3)) ///
       legend(label(1 "Empresas Tratadas") label(2 "Empresas de Control"))
graph export "line_2.png", replace
	   
	   
	   
	   
