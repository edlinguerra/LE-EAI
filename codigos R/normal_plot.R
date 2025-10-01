# Simulamos datos normales
set.seed(123)
x <- rnorm(50)

# Q-Q plot clásico
qqnorm(x, main = "Q-Q plot clásico")
qqline(x, col = "blue")

# Gráfico de probabilidad tipo Blom
# Usamos la fórmula de Blom: (i - 0.375) / (n + 0.25)
n <- length(x)
i <- 1:n
blom_pos <- (i - 0.375) / (n + 0.25)
blom_quantiles <- qnorm(blom_pos)

# Ordenamos los datos
x_sorted <- sort(x)

# Graficamos
plot(blom_quantiles, x_sorted, main = "Gráfico de probabilidad de Blom",
     xlab = "Cuantiles teóricos (Blom)", ylab = "Datos ordenados")
abline(lm(x_sorted ~ blom_quantiles), col = "red")
