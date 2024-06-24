#Script del ejercicio 1 del laboratorio 2
#Nombre y Apellido:

#Preguntas: 

#1. Importa los datos al ambiente con el nombre **datos** y verifica sus características y estructura.

# HAy dos formas de importar datos con extensión .csv, una es usando el paquete base de R, otra usando el 
#paquete readr. AMbos están en el menú "import dataset" en el Environment.

#Con el paquete base, funcion read.csv()
dat1 <- read.csv("~/UNAM/ENES/Lic. Ecologia/Estadística Aplicada I/LE-EAI/Lab3/datos1.csv")

#con el paquete readr, funcion read_csv()
dat1_readr <- read_csv("Lab3/datos1.csv")

#2. Calcula la media (con la función `mean`) y desviación estándar (con la función `sd`) de la DBO en la muestra y 
#   obtén un histograma (`hist`) para ver su distribución. ¿Te parece que tienen una distribución normal?
promedio <- mean(dat1$DBO) # calculo el promedio de la muestra
de <- sd(dat1$DBO) # calculo la desviación estándar de la muestra

#Creación del histograma
hist(dat1$DBO)
abline(v = promedio, col = "red", lwd = 2)

#  ¿cuál es el error estándar de la media? ¿Es un valor grande o pequeño?

ee <- de / sqrt(nrow(dat1))

dat_i <- rnorm(10000, mean = 141, sd = 33)

dario <- function(datos){
  
  de = sd(datos)
  n = length(datos)
  ee = de / sqrt(n)
  return(ee)
}


#3. Define la hipótesis alternativa y luego la Hipótesis nula. Aplica una prueba de t para una sola muestra, usando el 
#   comando `t.test`, pero antes de efectuar la prueba debes definir el alfa para rechazar la Hipótesis nula.
#   Utiliza la ayuda `help` para escribir los argumentos correctos de `t.test` (tip: los mas importantes son *mu* y *alternative*)

t.manual<- (promedio - 30)/ee

t.test(x = dat1$DBO, alternative = "greater", mu = 30)

+#4. A partir del resultado obtenido (output o salida) responde a las siguientes preguntas:
#    a) ¿Cuál fue el valor de *t* que obtuviste? 

#    b) ¿Cuál es el valor de probabilidad asociado a ese valor de *t*?

#    c) ¿Existen evidencias suficientes para considerar que DBO de las aguas redisuales superan los valores de la norma?

#    d) ¿Cuáles son los límites inferior y superior del intervalo de confianza al 95%? 


#5. Vamos a calcular el intervalo de confianza paso por paso. Para ello requerimos contar con el valor de *t* que separa la curva en zonas de baja y alta probabilidad de ocurrir (dos colas, cada una de prob alfa/2). Este valor se obtiene con la función `qt` y requiere de tres argumentos esenciales (p = 0.025, df = 49, lower.tail = TRUE. Usa el `help` para tener más detalles de esta función).
#    a) ¿Son un error los valores negativos de esta función? ¿Por qué? 

#    b) ¿Cómo puedes obtener un valor positivo?

#    c) ¿Por qué razón usamos una probabilidade de 0.025, si el intervalo de confianza es de 95%?

#    d) ¿Por qué usamos n-1 en vez de n?

#    Ahora, multiplica el valor de t-student (absoluto) por el valor de error estándar calculado antes.

#    e) ¿Qué obtuviste con esta operación? 

#    f) ¿Cuáles son las unidades de esta medida?

#    Finalmente suma y resta el intervalo (al 95%) a la media para conocer los límites superior e inferior. 

tv <- qt(p = 0.025, df = 49, lower.tail = F)

ics = promedio + (ee * tv)
ici = promedio - (ee * tv)

#    g) Compara los valores obtenidos con el resultado de la función usada para hacer la prueba de hipótesis.
ic <- c(ici, promedio, ics)
# Manual: 39.42947 40.76358 42.09770
# Funcion t.test = 39.65056 a infinito

#8. Calcula el intervalo de confianza al 99%? 
#    a) ¿En cuál de los dos intervalos (95% o 99%) tengo más confianza de encontrar la media si repito el experimento?

#    b) ¿Cuál de los dos intervalos es más ancho?

#    c) ¿Qué le sucedería al intervalo si en vez de 50 mediciones, tuviese solo 5 mediciones?

#    d) Verifica tu conclusión (PISTA: usa la función `qt`)

#9. ¿Qué sucedería con el IC al 95% si la DBO de los efluentes fuese mas variable? 

#    Obtén un histograma y verifica tu conclusión mediante una simulación usando la función `rnorm` (PISTA: mantén la muestra simulada en n=50, y la media en 40).

sim <- rnorm(50, mean = mean(dat1$DBO), sd = 3*sd(dat1$DBO))
des <- sd(sim)
ee2 <- des / sqrt(50)

ics2 = promedio + (ee2 * tv)
ici2 = promedio - (ee2 * tv)
ic2 <- c(ici2, promedio, ics2)

