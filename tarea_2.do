
**Tarea 2 

**log(ventas)= B0+B1log(Creditos)+ui**

**Ejercicio 1

**#
clear

use "C:\Users\franc\Desktop\Magíster en Economía\Econometría\Tareas\Tarea 2\tarea_ej1_alumnos-1.dta"

gen log_ventas = ln(ventas)
gen log_creditos = ln(creditos)

*1 Scatter Plot

twoway (scatter log_ventas log_creditos)

*2

reg log_ventas log_creditos 

*3 *solo requiere argumentación en texto?"

*4

*5

*6

*7

*8

*9

*10

