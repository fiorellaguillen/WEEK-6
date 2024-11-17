library(spatstat)
library(here)
library(sp)
library(tmap)
library(sf)
library(tmaptools)
library(stringr)
library(tidyverse)

#Load london borough spatial data
LondonBoroughs <- st_read(here::here("DATA", "London_Borough_Excluding_MHW.shp"))

#Reproject it to UK National Grid and select only London boroughs
BoroughMap <- LondonBoroughs %>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(.,27700)
qtm(BoroughMap) #Create a map

#Load Blue Plaques spatial data
BluePlaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")%>%
  st_transform(.,27700)


#get info of Blue Plaques
summary(BluePlaques)

tmap_mode("plot")
tm_shape(BoroughMap)+
  tm_polygons(fill = NA,fill_alpha=0.5)+
tm_shape(BluePlaques)+
  tm_dots(fill="blue")

#remove duplicate points
BluePlaques <- distinct(BluePlaques)

#select only points inside London
BluePlaquesSub <- BluePlaques[BoroughMap,]

tm_shape(BoroughMap)+
  tm_polygons(fill=NA, fill_alpha = 0.5)+
tm_shape(BluePlaquesSub)+
  tm_dots(fill="blue")

#get the indices of points intersecting with each borough
intersect_indices <- st_intersects(BoroughMap, BluePlaques)

#extract only one borough
Harrow <- BoroughMap %>%
  filter(.,NAME=="Harrow")

tm_shape(Harrow)+
  tm_polygons(fill="grey", fill_alpha=.5)

BluePlaquesSub <- BluePlaques[Harrow,]

#plot only polygon and of Harrow and plaques inside
tmap_mode("plot")

tm_shape(Harrow)+
  tm_polygons(fill = "grey",fill_alpha=0.5)+
  tm_shape(BluePlaquesSub)+
  tm_dots(fill="blue")

window <- as.owin(Harrow)
plot(window)