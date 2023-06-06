library("move")
library("openxlsx")
library("ggplot2")
library("ggmap")
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library("measurements")
library("geosphere")
library("foreach")
library("shiny")

#fork test from caetano ;)

#login once#
loginStored <- movebankLogin(username="xxx", password="xxx")

#set max.print to a huge number
options(max.print = 99999)

#new dep for specific tag
df_tag <- getMovebankLocationData(study="Cathartid ecology - Dénes - Brazil", sensorID="GPS", animalName="8840", timestamp_start="20230601000000000", login=loginStored, removeDuplicatedTimestamps=TRUE)
df_cropt<- df_tag[,c("timestamp", "location.long", "location.lat", "tag.local.identifier", "individual.taxon.canonical.name", "eobs.battery.voltage", "sensor.type")]

View(df_cropt)

####from morning rep####

activesense <- function(df) {
  coords <- df[c("location.long","location.lat")]
  meanlon <- mean(coords[["location.long"]])
  meanlat <- mean(coords[["location.lat"]])
  
  #8.8 = 20m*0.44, to reflect the difference in coords in meters.  
  out1 <- ifelse(distVincentyEllipsoid(coords,c(meanlon,meanlat))>(8.8), yes = "movement", no=  "dead")
  
  return(list(out1=out1,coords=coords, mean_coords = c(meanlon,meanlat)))
}

x <- activesense(df_cropt)

x$out1


####ggmap####

#define location (location=c(left,bottom,right,top) or location= c(meanlon,meanlat))
#defining objects outside activesense()
coords <- df_cropt[c("location.long","location.lat")]
meanlon <- mean(coords[["location.long"]])
meanlat <- mean(coords[["location.lat"]])

#API key
register_google("xxx")

MapSat <- get_map(location= x$mean_coords, zoom= 15, source="google", maptype="satellite", crop=FALSE)
MapTerr <- get_map(location= x$mean_coords, zoom= 14, source="google", maptype="terrain", crop=FALSE)
#add points
ggmap(MapSat) + geom_point(data = df_cropt, aes(x = location.long, y = location.lat), size = 2, shape = 23, fill = "darkred")
ggmap(MapTerr) + geom_point(data = df_cropt, aes(x = location.long, y = location.lat), size = 2, shape = 23, fill = "darkred")
