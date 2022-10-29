

# Importem les llibreries
# for loading our data
library(raster)
library(readr)
library(readxl)
library(sf)
library(sp)
# for datasets
library(maps)
library(spData)
# for creating animations
library(magick)
# for plotting
library(grid)
library(tmap)
library(viridis)
library(spdep)
library(rgdal)
library(tidyverse)
library(rayshader)
library(magick)
library(av)
library(ggplot2)
library(rgl)
library(RColorBrewer)

# Importem el nostre arxiu

Barcelona=readOGR("bllo.shp")
Barcelona2 <- read_sf("bllo.shp")
barcelona_nbq <- poly2nb(Barcelona)  #queen's neighborhood
barcelona_nbq_w <- nb2listw(barcelona_nbq) # Llista de polígons continus


# moran test general
moran.test(Barcelona$a2020,barcelona_nbq_w, alternative="greater", zero.policy = TRUE,  na.action=na.omit, adjust.n = TRUE )

# moran test local (es a dir per a cada seccio censal respecte llurs veins)
locm <- localmoran(Barcelona$a2020, barcelona_nbq_w)

moran.map <- cbind(Barcelona, locm)
tm_shape(moran.map) +
  tm_fill(col = "Ii",
          style = "pretty",
          palette = "RdBu",
          title = "local moran statistics")+
  tm_borders(alpha = 0.5)

# estandaritzem la variable any2020 (a2020): li restem la mitjana i la dividim per la sd amb scale()
Barcelona$sa2020 <- scale(Barcelona$a2020)  #nova columna

# creem una nova lag.listw (és a dir, estandaritzem els preus dels lloguers dels veins)
Barcelona$lag_sa2020 <- lag.listw(barcelona_nbq_w, Barcelona$sa2020)


# Podem fer un plot
plot(x = Barcelona$sa2020, y = Barcelona$lag_sa2020, main = " Test de Moran", xlab="Mitjana secció censal", ylab="Mitjana secc censal veïnes")
abline(h = 0, v = 0)
abline(lm(Barcelona$lag_sa2020 ~ Barcelona$sa2020), lty = 3, lwd = 4, col = "red")
# Què estem comparant aquí? Doncs en l'eix d'x la mitjana de preu de la nostra secc censal
# en l y hi ha la mitjana de preu dels veins de la seccio censal

# Identifiquem els quadrants a on pertanynen

quadrant <- vector(mode = "numeric", length = nrow(locm))
signif <- 0.05

quadrant[(Barcelona$sa2020 >= 0 & Barcelona$lag_sa2020 >= 0) & (locm[, 5] <= signif)] <- 1
quadrant[(Barcelona$sa2020 <= 0 & Barcelona$lag_sa2020 >= 0) & (locm[, 5] <= signif)] <- 2
quadrant[(Barcelona$sa2020 <= 0 & Barcelona$lag_sa2020 <= 0) & (locm[, 5] <= signif)] <- 3
quadrant[(Barcelona$sa2020 >= 0 & Barcelona$lag_sa2020 <= 0) & (locm[, 5] <= signif)] <- 4
quadrant[locm[, 5]>= signif] <- 0

#colors <- c("#ffffff", "#2c7bb6", "#abd9e9", "#fdae61", "#d7191c")
colors <- c("#F3FAE1","#005F73", "#94D2BD", "#AE2012", "#EE9B00")

clusters <- c("Insignificant", "Alt-Alt", "Baix-Alt", "Baix-Baix", "Alt-Baix")

moran.map$quadrant <- quadrant 
tm_shape(moran.map) +
  tm_fill(col = "quadrant", style = "cat", palette = colors[c(sort(unique(quadrant)))+1], labels = clusters[c(sort(unique(quadrant)))+1], popup.vars = c("Postal.Code")) +
  tm_view(set.zoom.limits = c(11,17)) +
  tm_borders(alpha=0, col="white")

