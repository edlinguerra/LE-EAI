#Script del ejercicio 2 del laboratorio 2
#Nombre y Apellido:

#Preguntas: 

#1. Importa los datos del archivo **datos2.csv** al ambiente R con el nombre **datos** y verifica sus características y estructura. Asegúrate que el archivo esté en el ambiente de trabajo.

library(readr)
datos2 <- read_csv("lab3/datos2.csv")

#2. Obtén los nombres de las variables en el archivo y sigue las siguientes instrucciones:
colnames(datos2)

#   a) Identifica el vector que contiene la variable que se pretende analizar, y establece si ésta es contínua o discreta.
#

datos2$DBO

#   b) Identifica el vector que contiene el factor 'origen'. ¿Cuántos niveles tiene dicho factor? 
datos2$Momento

#
#   c) ¿Cuál es la unidad experimental en este estudio?
#
datos2$muestra

#   d) ¿Cuántas réplicas fueron medidas en cada nivel? ¿Consideras este diseño uno balanceado?
#   
library(dplyr)

datos2|>
  group_by(Momento)|>
  summarise(n = length(muestra))

#   3. Contruye un histograma con `hist` para para la variable `DBO` sin distinguir el momento, pero agregue dos líneas verticales para cada promedio. Luego obtén un gráfico de cajas-bigotes agrupando datos según el *Momento*. PISTA: usa la función `boxplot`, busca cómo se usa esta función. ¿cuál representación gráfica te parece más informativa? Explica la respuesta. Confirma que obtienes estos gráficos: 

#Histograma con código base de R
hist(datos2$DBO, main = "Histograma de la DBO según el Momento")|>  
abline(v = mean(datos2$DBO[1:25]), col = "blue")|>
abline(v = mean(datos2$DBO[26:50]), col = "red")
 
#Histograma con ggplot2
library(ggplot2)

estadisticos <- datos2|>
  group_by(Momento)|>
  summarise(promedio = mean(DBO),
            desv = sd(DBO),
            varianza = var(DBO),
            n = length(muestra))


ggplot(datos2, aes(x = DBO))+
  geom_histogram(bins = 10, fill = "gray75", colour = "black")+
  geom_vline(data = estadisticos, aes(xintercept = promedio, colour = Momento))


# 4. Describe las características de ambos momentos en términos de sus medias, dispersiones y la forma aproximada de sus distribuciones. Ayudate calculando estos estadísticos con las funciones 'summary', 'mean' y 'var'. ¿Cuáles de estas características son parecidas y cuáles diferentes entre las dos muestras?
# 
estadisticos <- datos2|>
  group_by(Momento)|>
  summarise(promedio = mean(DBO),
            desv = sd(DBO),
            varianza = var(DBO),
            n = length(muestra))

datos2|>
  filter(Momento == "antes")


#  
# 5. Formula la hipótesis del modelo y la hipótesis nula, y define si se trata de una prueba de **una** o **dos** colas. 
# Obten el valor crítico de la distribución de *t*-student para rechazar la hipótesis nula. 
# Considera un alfa = 0.05, y grados de libertad = n<sub>1</sub> + n<sub>2</sub>-2.
#
# H0 = Mu.antes >= Mu.despues 
# Ha = Mu.antes < Mu. despues
# La prueba t es de dos colas, para dos poblaciones
# t critico para rechazar H0
qt(0.05, df = 25+25-2, lower.tail = T)



# 
# 6. Aplica una prueba de *t*-student para dos muestras independientes (*S3 method for class formula*), y responde a las siguientes preguntas:

# Prueba t para dos poblaciones manual
ee.prueba = sqrt((((24 * 8.19) + (24 * 35.4))/48)*((1/15)+(1/25)))
t.obs = (11.6 - 16.5)/ee.prueba

# Prueba t con funciones

antes <- datos2[1:25,3]
despues <- datos2[26:50,3]

t.test(x = antes, y = despues, alternative = "less", var.equal = T)

# a) ¿Cuál es el valor de t obtenido? ¿Es grande o pequeño?
#   b) ¿Cuál es la probabilidad asociada al valor de t obtenido? ¿Es grande o pequeña?
#

-
#   c) ¿Cuál es la conclusión de la prueba que aplicaste?
#
#   d) ¿Podrías enlistar tres de las condiciones más importantes que deben cumplir los datos para interpretar adecuadamente e resultado de una prueba *t* clásica?
#   
#   7. Considera que las varianzas de estas dos muestras son lo suficientemente similares como para asumirlas iguales,
#    y aplica una prueba de *t*-student para varianzas iguales. (PISTA: revisa la informacion del `help` de `t.test`)




# 8. Compara los resultados de ambas pruebas y responde a las siguientes preguntas:
#   a) ¿Qué ocurre con el valor de t cuando asumes homogeneidad de varianzas aún cuando no la hay?
#
#   b) ¿Qué ocurre con el valor de probabilidad asociado al estadístico t cuando asumes homogeneidad de varianzas y no la hay?
#
#   c) ¿Qué ocurre con los grado de libertad?
#
#   d) ¿Cuál de las dos pruebas es más conservadora (i.e., tiene menos probabilidad de caer en errores tipo I)?
#
#   e) ¿Confías más en el resultado de la primera prueba o en el de la segunda? Explica tu respuesta.    

-3.73 
#o
t.obs 

#diferencias por redondeo

