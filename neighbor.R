# this script aims at computing an averaged value based on neighbour pixels

setwd("D:/Perle/R_rep/ZSL/hedgehog")

library(sp)
library(rgdal)
library(raster)
library(rgeos)

GS <- raster('raster_pl_cov.TIF')

# plot(GS)

#500m N = 11x11
#1km N = 21x21
#2km N = 41x41

neigh_500m <- focal(GS,w=matrix(1,nrow=11,ncol=11), fun = mean, na.rm = TRUE)
neigh_1km <- focal(GS,w=matrix(1,nrow=21,ncol=21), fun = mean, na.rm = TRUE)
neigh_2km <- focal(GS,w=matrix(1,nrow=41,ncol=41), fun = mean, na.rm = TRUE)
# plot(neigh_500m)
# plot(neigh_1km)
# plot(neigh_2km)

writeRaster(neigh_500m, filename = 'N_pl_500.TIF', overwrite = TRUE)
writeRaster(neigh_1km, filename = 'N_pl_1.TIF', overwrite = TRUE)
writeRaster(neigh_2km, filename = 'N_pl_2.TIF', overwrite = TRUE)

#neigh <- crop(neigh_2km, border)
#plot(neigh)

# #Check the changes
# GSn.df <- as.data.frame(neigh_2km, xy = TRUE, na.rm = TRUE)
# length(GS.df$raster_al_cov)
# 
# GS.df <- as.data.frame(GS, xy = TRUE)
# GS.df$neigh <- GSn.df$layer
# 
# 
# writeRaster(neigh_2km, filename = 'neighbor_al.TIF', overwrite = TRUE)
# writeOGR()
# 
# # SpatialPolygonsDataFrame(grid, GSn.df)
# #have to remove all 0
# #or matching coordinates from grid to keep only the coordinates which are into the GreaterLondon
# c1 <- coordinates(grid)
# x1 <- c1[,1]
# c2 <- coordinates(neigh_2km$layer)
# c3 <- coordinates(GS)
# str(c1)
# summary(c3 %in% c2)
# 
# 

# ####### OTHER WAY ###########
# grid <- readOGR("C:/ZSL/Hedgehog", "grid")
# plot(grid)
# coordinates(grid)
# grid
# GS <- grid$GS_cover
# id <- grid$id
# i <- c(seq(1,90,1))
# j <- c(seq(1,120,1))
# 
# as.matrix(grid$GS_cover)
# 
# summary(a %in% b)