# Simulación de datos para ANOVA de índice de Simpson

set.seed(123)  # Para reproducibilidad

# Definir niveles de zinc
niveles_zinc <- c("Referencia", "Baja", "Media", "Alta")

# Número de estaciones por arroyo
n <- 5

# Promedios esperados
media_ref <- 0.95
media_baja <- 0.93
media_media <- 0.93
media_alta <- round(media_ref * (1 - 0.20), 2)  # 23% menos que referencia

# Desviación estándar común
sd_simpson <- 0.10

# Simular datos
simpson <- c(
  rnorm(n, mean = media_ref, sd = sd_simpson),
  rnorm(n, mean = media_baja, sd = sd_simpson),
  rnorm(n, mean = media_media, sd = sd_simpson),
  rnorm(n, mean = media_alta, sd = sd_simpson)
)

grupo <- factor(rep(niveles_zinc, each = n))

# Crear data frame
datos <- data.frame(Indice_Simpson = simpson, Grupo_Zinc = grupo)

# Ajustar modelo lineal
modelo <- lm(Indice_Simpson ~ Grupo_Zinc, data = datos)

# Tabla ANOVA
anova(modelo)

# Estadísticos descriptivos por grupo
aggregate(Indice_Simpson ~ Grupo_Zinc, data = datos, 
          FUN = function(x) c(Promedio = round(mean(x),2), DesvEst = round(sd(x),2)))

          