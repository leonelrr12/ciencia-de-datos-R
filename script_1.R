#install.packages("datos")
#install.packages("data.table")
#install.packages("readxl")
#install.packages("tidyverse")

library(datos)
library(data.table)
library(readxl)
library(tidyverse)


# Manejo del directorio 
# Identificar directorio de trabajo
getwd()
# Definimos el directoro de trabajo
setwd("")


# cargamos datos de paises
paises <- datos::paises
paises %>% View()
View(paises)


# importar CSV
dat1 <- fread("datos_paises.csv")
View(dat1)


# importamos XLSX
dat2 <- read_xlsx("datos_paises.xlsx")
View(dat2)


# Select
paises %>% 
  select(pais)


# Filter + Select
paises %>% 
  filter(pais == "Colombia") %>%
  select(pais, esperanza_de_vida)


# Ordenamiento ascendente
paises %>%
  arrange(poblacion)


# Ordenamiento descendente
paises %>%
  arrange(desc(poblacion))


# Filter + Ordenamiento + Select
paises %>% 
  filter(continente == "Europa") %>%
  arrange(desc(poblacion)) %>%
  select(pais, anio, poblacion)  


# Filter + group_by + Summarise + mutate + arrange desc
paises %>%
  filter(anio ==2007) %>%
  group_by(continente) %>%
  summarise(pob_continente = sum(poblacion)) %>%
  ungroup() %>% 
  mutate(pob_mundial = sum(pob_continente)) %>%
  mutate(pje_pob_mundial = pob_continente/pob_mundial*100) %>%
  arrange(desc(pje_pob_mundial))
  

  
### Dibujar el porcentual de la poblacion de los continentes en forma de punto y despues de barra
### guardar la base de datos (BD) ordenada en una variable

dat <- paises %>%
  filter(anio ==2007) %>%
  group_by(continente) %>%
  summarise(pob_continente = sum(poblacion)) %>%
  ungroup() %>% 
  mutate(pob_mundial = sum(pob_continente)) %>%
  mutate(pje_pob_mundial = pob_continente/pob_mundial*100) %>%
  arrange(desc(pje_pob_mundial))


### Mapear la BD ordenada a caracteristicas de la grafica (aesthetics)
p <- ggplot(data = dat,
              mapping = aes(x = continente,
                            y = pje_pob_mundial,
                            col = continente))
  

### Definir la geometria de la grafica
p <- p + geom_point()
  

### Definir los labels de la grafica
p <- p + labs(title = "Percentaje Población Mundial",
              x = "Continentes",
              y = "%")


### Dibujar la grafica
print(p)

