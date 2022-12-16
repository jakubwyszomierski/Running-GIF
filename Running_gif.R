

# gif and circle - The High

library(tmap)
library(gganimate)
library(osmdata)
library(gganimate)
library(sf)

setwd("~/Desktop/dom/R/projekty/OpenStreetMap_vis/Running_gif")
#load run
#run <- rgdal::readOGR("~/Desktop/The_High.gpx", layer="track_points")
run <- rgdal::readOGR("The_beer.gpx", layer="track_points")
#run <- spTransform(run, CRS("+init=epsg:27700"))
tm_shape(run) + tm_dots()


### get coordinates
geodf <- run
geodf$lon <- geodf@coords[,1]
geodf$lat <- geodf@coords[,2]  
geodf <- geodf@data
class(geodf$time)
geodf$time <- strptime(geodf$time, format = "%Y/%m/%d %H:%M:%OS")
geodf <- geodf[,c( "track_seg_point_id","lon", "lat", "time", "ele")]
nrow(geodf)


### set the theme of a map
my_theme <- theme(panel.background = element_rect(fill = "black"), 
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  axis.title.x=element_blank(),
                  axis.text.x=element_blank(),
                  axis.ticks.x=element_blank(),
                  axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank()
)

#get data from Open Street Map
bb <- getbb("Warsaw Poland")
bb_run <- bb
# bb_run[1] <- min(geodf$lon) -  0.003
# bb_run[2] <-  min(geodf$lat) - 0.003
# bb_run[3] <-  max(geodf$lon) + 0.003
# bb_run[4] <-  max(geodf$lat) + 0.003


## specify CRS
crs1 <- 27700 #2178
crs2 <- 4326

#centre of the circle
center <- c(long = mean(geodf$lon),
            lat = mean(geodf$lat))

#get the osm street data | increase timeout 
street <- bb_run %>%
  opq(timeout=200)%>%
  add_osm_feature(key = "highway",
                  value = c("motorway", "primary", 
                         "motorway_link", "primary_link",
                         "secondary", "tertiary", "secondary_link",
                         "tertiary_link"
                           )) %>%
  osmdata_sf()

small_streets <- bb_run%>%
  opq(timeout = 100)%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

### create a circle for streets
#circle
circle_streets <- dplyr::tibble(lat = center["lat"]+0.008, long = center["long"]+0.018) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%  
  st_transform(st_crs(27700)) %>% 
  st_buffer(dist = 5000) %>% 
  st_transform(st_crs(4326))

street_cropped <-  st_intersection(street$osm_lines, circle_streets)
small_street_cropped <-  st_intersection(small_streets$osm_lines, circle_streets)

river <- bb%>%
  opq(timeout=100)%>%
  add_osm_feature(key=c("natural", "water"), value = c("water", "river")) %>%
  osmdata_sf()

### create a circle for river
circle_river <- dplyr::tibble(lat = center["lat"], long = center["long"]) %>% 
  st_as_sf(coords = c("long", "lat"), crs = 4326) %>%  
  st_transform(st_crs(27700)) %>% 
  st_buffer(dist = 7000) %>% 
  st_transform(st_crs(4326))


#### crop OSM data by circles
river_cropped <- st_intersection(river$osm_polygons, circle_river)%>% dplyr::filter(osm_id!="109714750")
river_cropped2 <- st_intersection(st_make_valid(river$osm_multipolygons), st_make_valid(circle_river))



### create ggplot
g <-  ggplot() +
  geom_sf(data=river_cropped,
          fill = "steelblue", lwd=0, alpha = 1) + 
  geom_sf(data=river_cropped2,
          fill = "steelblue", lwd=0, alpha = 1) + 
  geom_sf(data = street_cropped,
          col="white", size=0.3, alpha=0.7)+
  geom_sf(data = small_street_cropped ,
          col="white", size=0.08, alpha=0.5)+
  geom_path(data=geodf, aes(lon, lat),col="#ff9eae", size=1.5)+
  # coord_sf(xlim = c(bb_run[1], bb_run[3]),
  #          ylim = c(bb_run[2], bb_run[4]))+
  my_theme
g


#### save the route as png
ggsave(filename="Strava_run.png", plot=g, device="png", height=6, width=6, units="in", dpi=300)

### animate the file
gi <- g +  geom_point(data=geodf, aes(lon, lat),col="#ff9eae", size=2.5)+
  transition_reveal(track_seg_point_id) #+ 
gi




animate(gi, height = 6,
        width = 6, units = "in", res = 300)
anim_save("Strava_run.gif")
