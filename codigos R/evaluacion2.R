# Laboratorio 2 de Estadística Aplicada I

#Para instalar ggplot2 (realizar gráficos de alta calidad)
install.packages("ggplot2")

plot(x)

sum(xx)


Pb <- 46.7 
Cr <- 81.0
n <- 10

xP <- 46.5
xC <- 108.6

sdP <- 21
sdC <- 37.24

# Prueba 1:
#   HA: xP > Pb
#   H0: xP <= Pb
  
# Criterio rechazo H0 tobs > tcri con alfa 0.05, una cola positiva
tobsP <- (xP - Pb)/(sdP/sqrt(n))
tcrt <- qt(0.05, n-1, lower.tail = FALSE)
tobsP > tcrt
No hay evidencia para rechazar H0. 
  
Prueba 2
  HA: xC > Cr
  H0: xC <= Cr

# Criterio rechazo H0 tobs > tcri con alfa 0.05, una cola positiva
  tobsC <- (xC - Cr)/(sdC/sqrt(n))
  tcrt <- qt(0.05, n-1, lower.tail = FALSE)
  tobsC > tcrt
# Sí hay evidencia para rechazar H0. 
