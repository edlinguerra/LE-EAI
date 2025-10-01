library(GAD)
library(dplyr)
library(ggplot2)

datos5b <- read.csv("Lab5/datos5b.csv")

BA <- as.fixed(datos5b$BA)
CI <- as.fixed(datos5b$CI)
DBO <- datos5b$DBO

mod1 <- lm(DBO ~ BA + CI + BA*CI)

estimates(mod1)
gad(mod1)
anova(mod1)

ps <- position_dodge(width = 0.4)

datos5b %>% 
  group_by(BA, CI) %>%
  summarise(prom = mean (DBO), desv = sd(DBO)) %>% 
  ggplot(aes(x = BA, y = prom, group = CI, colour = CI))+
  geom_errorbar(aes(ymin = prom-desv, ymax = prom+desv),
                position = ps, width = 0.2)+
  geom_point(position = ps, size = 3)+
  theme_bw()+
  ylab("DBO")+
  xlab("Momento")

#Parte B
zona <- as.random(datos5b$Zona)
mod2 <- lm(DBO ~ BA + CI + BA*CI + Zona%in%(BA*CI))
estimates(mod2)
gad(mod2)

#Parte C
library(GAD)
library(dplyr)
library(ggplot2)
library(readxl)

datos5c <- read_excel("Lab5/datos5c.xlsx")
View(datos5c)

area2 <- rep(datos5c$area[1:10],8)
sitio <- rep(datos5c$sitio[1:20], 4)

loc <- as.random(datos5c$localidad)
sitio <- as.random(sitio)
area <- as.random(area2)
S <- datos5c$S

mod3 <- lm(S~loc + sitio%in%loc + area%in%sitio%in%loc)
estimates(mod3)
gad(mod3)
anova(mod3)
