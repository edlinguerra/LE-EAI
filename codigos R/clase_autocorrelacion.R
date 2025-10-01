library(ggplot2)
library(patchwork)

# Autocorrelación positiva de residuos
set.seed(1)
grupo <- rep(c("A", "B", "C"), each = 10)
residuales <- unlist(lapply(1:3, function(i) cumsum(rnorm(10, 0, 0.1))))
valor <- rep(c(5, 5.5, 5), each = 10) + residuales

df1 <- data.frame(Grupo = grupo, Valor = valor, Orden = rep(1:10, 3))

p1 <- ggplot(df1, aes(x = Grupo, y = Valor, color = Grupo)) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(2,8,1), limits = c(2,8))+
  geom_hline(yintercept = 5.5)+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", color = "gray40")
  )+
  ggtitle("Autocorrelación positiva de residuales", 
          subtitle = "Se subestima la σ2 dentro, F-relación excesivo.
Se incremente Error tipo I: Diferencias espurias."
       )
p1


# Autocorrelación negativa de residuos
set.seed(1)
grupo <- rep(c("A", "B", "C"), each = 10)
residuales <- unlist(lapply(1:3, function(i) cumsum(rnorm(10, 0, 1))))
valor <- rep(c(5, 6, 5), each = 10) + residuales

df2 <- data.frame(Grupo = grupo, Valor = valor, Orden = rep(1:10, 3))

p2 <- ggplot(df2, aes(x = Grupo, y = Valor, color = Grupo)) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(2,8,1), limits = c(2,8))+
  geom_hline(yintercept = 5.5)+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", color = "gray40")
  )+
  ggtitle("Autocorrelación negativa de residuales", 
          subtitle = "Se sobrestima la σ2 dentro, F-relación bajo.
Se incremente Error tipo II: Diferencias reales no detectadas."
  )

p2

# Autocorrelación positiva de efectos
set.seed(1)
grupo <- rep(c("A", "B", "C"), each = 10)
residuales <- unlist(lapply(1:3, function(i) cumsum(rnorm(10, 0, 0.5))))
valor <- rep(c(5, 5.5, 6), each = 10) + residuales

df3 <- data.frame(Grupo = grupo, Valor = valor, Orden = rep(1:10, 3))

p3 <- ggplot(df3, aes(x = Grupo, y = Valor, color = Grupo)) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(2,10,1), limits = c(2,10))+
  geom_hline(yintercept = 5.5)+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", color = "gray40")
  )+
  ggtitle("Autocorrelación positiva de efectos", 
          subtitle = "Se subestima la σ2 entre, F-relación bajo.
Se incremente Error tipo II: Diferencias reales no detectadas"
  )
p3


# Autocorrelación negativa de efectos
set.seed(1)
grupo <- rep(c("A", "B", "C"), each = 10)
residuales <- unlist(lapply(1:3, function(i) cumsum(rnorm(10, 0, 0.5))))
valor <- rep(c(4.5, 5.5, 6.5), each = 10) + residuales

df4 <- data.frame(Grupo = grupo, Valor = valor, Orden = rep(1:10, 3))

p4 <- ggplot(df4, aes(x = Grupo, y = Valor, color = Grupo)) +
  geom_point(size = 2) +
  scale_y_continuous(breaks = seq(2,10,1), limits = c(2,10))+
  geom_hline(yintercept = 5.5)+
  theme_bw()+
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12, face = "italic", color = "gray40")
  )+
  ggtitle("Autocorrelación negativa de efectos", 
          subtitle = "Se sobrestima la σ2 entre, F-relación bajo.
Se incremente Error tipo II: Diferencias reales no detectadas"
  )

p4

(p1 / p2) | (p3 / p4) + plot_layout(guides = 'collect')
