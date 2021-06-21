library(sf)
library(leaflet)

data <- read_sf("https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.geojson")

leaflet(data) %>% 
  addTiles() %>% 
  addPolygons()