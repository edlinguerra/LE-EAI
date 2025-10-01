# Puedo usar el símbolo # para crear texto que no lea R

a <- log(10) # esto calcula el logaritmo de 10


#Vamos a cargar la base de datos Iris

data(iris)


boxplot(Sepal.Length ~ Species, # formula de variables
        data = iris, #base de datos
        ylab = "Largo del Sépalo", # modifico título eje y
        xlab = "Especie") # modifico título eje X
