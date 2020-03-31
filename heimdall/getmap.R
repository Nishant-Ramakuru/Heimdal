setwd("C:/Users/Akshay Gaikwad/Documents/NewUi")
library(RgoogleMaps)

for (zoom in 10:16)
  GetMapTiles(center = getGeoCode("Washington Square Park;NY"),zoom=zoom,nTiles=round(c(20,20)/(17-zoom)),urlBase = "http://mt1.google.com/vt/lyrs=m", tileDir= "~/NewUi/www/Google/")

library(leaflet)
library(servr)

m=leaflet() 
m<-addTiles(m,urlTemplate="Google/{z}_{x}_{Y}.png",option = tileOptions(tms = TRUE)) 
m
 
