
# Link: https://rstudio.github.io/leaflet/
#
# Basic Usage
# You create a Leaflet map with these basic steps:
#   
# 1- Create a map widget by calling leaflet().
# 2- Add layers (i.e., features) to the map by using layer functions (e.g. addTiles, addMarkers, addPolygons) to modify the map widget.
# 3- Repeat step 2 as desired.
# 4- Print the map widget to display it

library(leaflet)

m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  leaflet::addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
m  # Print the map

# add some circles to a map
df = data.frame(Lat = 1:10, Long = rnorm(10))
leaflet(df) %>% addCircles()

# You can also explicitly specify the Lat and Long columns (see below for more info on the ~ syntax):
leaflet(df) %>% addCircles(lng = ~Long, lat = ~Lat)

# A map layer may use a different data object to override the data provided in leaflet(). We can rewrite the above example as:

leaflet() %>% addCircles(data = df)
leaflet() %>% addCircles(data = df, lat = ~ Lat, lng = ~ Long)

# Below are examples of using sp and maps, respectively:
library(sp)
Sr1 = Polygon(cbind(c(2, 4, 4, 1, 2), c(2, 3, 5, 4, 2)))
Sr2 = Polygon(cbind(c(5, 4, 2, 5), c(2, 3, 2, 2)))
Sr3 = Polygon(cbind(c(4, 4, 5, 10, 4), c(5, 3, 2, 5, 5)))
Sr4 = Polygon(cbind(c(5, 6, 6, 5, 5), c(4, 4, 3, 3, 4)), hole = TRUE)
Srs1 = Polygons(list(Sr1), "s1")
Srs2 = Polygons(list(Sr2), "s2")
Srs3 = Polygons(list(Sr4, Sr3), "s3/4")
SpP = SpatialPolygons(list(Srs1, Srs2, Srs3), 1:3)
leaflet(height = "300px") %>% addPolygons(data = SpP)

library(maps)
mapStates = map("world2", fill = TRUE, plot = FALSE)
leaflet(data = mapStates) %>% addTiles() %>%
  addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE)

m = leaflet() %>% addTiles()
df = data.frame(
  lat = rnorm(100),
  lng = rnorm(100),
  size = runif(100, 5, 20),
  color = sample(colors(), 100)
)
m = leaflet(df) %>% addTiles()
m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))


# subset the data frame 
df_map <- df_testdata %>%
  filter( client_id == "client_0",
          machine_id == "M_001") 

df_map2 <- df_testdata %>%
  filter( #client_id == "client_0",
          machine_id == "M_001")  

names(df_map)
# [1] "id"                       "client_id"                "machine_id"               "date"                     "lot_health_index"        
# [6] "fixed_price"              "dynamic_price"            "avg_market_premium_price" "localization_lat"         "localization_lon"        
# [11] "year_"

table(df_map2$localization_lat, useNA = "always")

# Show first 20 rows from the `quakes` dataset
leaflet(data = df_map2[1:5000, ]) %>% addTiles() %>%
  addMarkers(lng = ~localization_lon, lat = ~localization_lat, popup = ~as.character(round(dynamic_price, 3)), label = ~as.character(round(dynamic_price, 3))) %>%
  setView(lng = 10.4515, lat = 51.1657, zoom = 6)
#addMarkers(lng = ~Longitude, lat = ~Latitude

m <- leaflet(states) %>%
  setView(-96, 37.8, 4) %>%
  addProviderTiles("MapBox", options = providerTileOptions(
    id = "mapbox.light",
    accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))

# # Show first 20 rows from the `quakes` dataset
# leaflet(data = quakes[1:20,]) %>% addTiles() %>%
#   addMarkers(~long, ~lat, popup = ~as.character(mag), label = ~as.character(mag))
# 
# m = leaflet(df_map) %>% addTiles()
# m %>% addCircleMarkers(radius = ~size, color = ~color, fill = FALSE)
# 
# m %>% addCircleMarkers(radius = runif(100, 4, 10), color = c('red'))


