library(osmdata)
library(sf)
library(ggplot2)
library(tidyverse)
library(sf)
library(ggspatial)
###### el area de estudio 
ambito <- mapedit::drawFeatures()       # Creamos el objeto 
ambito <- ambito %>% st_as_sf()         # Convertimos el objeto sf_ee 

gye = opq(bbox= c(-69.22298, -12.64463, -69.15867, -12.55004)) %>%
  add_osm_feature(key= "highway") %>%
  osmdata_sf()

gye1 = opq(bbox= c(-69.22298, -12.64463, -69.15867, -12.55004)) %>%
  add_osm_feature(key= "leisure", value= "park") %>%
  osmdata_sf()

gye2 = opq(bbox= c(-69.22298, -12.64463, -69.15867, -12.55004)) %>%
  add_osm_feature(key= "aeroway") %>%
  osmdata_sf()

building = opq(bbox= c(-69.22298, -12.64463, -69.15867, -12.55004)) %>%
  add_osm_feature(key= "building") %>%
  osmdata_sf()

shop  = opq(bbox= c(-69.22298, -12.64463, -69.15867, -12.55004)) %>%
  add_osm_feature(key= "shop") %>%
  osmdata_sf()

osm_rivers.sf <- opq(bbox = c(-69.22298, -12.64463, -69.15867, -12.55004)) %>%
  add_osm_feature(key = 'waterway', value = 'river') %>%
  osmdata_sf()

osm_rivers  = osm_rivers.sf$osm_polygons %>% select(name) %>% st_transform(crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
gye_vias = gye$osm_lines %>% select(highway) %>% st_transform(crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
gye_park = gye1$osm_polygons %>% select(leisure) %>% st_transform(crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
gye_aero = gye2$osm_polygons %>% select(aeroway) %>% st_transform(crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
gye_edif = building$osm_polygons %>% select(name) %>% st_transform(crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
gye_tienda_p = shop$osm_points %>% select(name) %>% st_transform(crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
gye_tienda_p = gye_tienda_p[!is.na(gye_tienda_p$name),]

ggplot()+
  geom_sf(data = gye_vias, col= "grey40", size=0.1)+
  geom_sf(data = gye_park, col= "springgreen")+
  geom_sf(data = gye_aero, fill= "#b08968")+
  geom_sf(data = gye_edif, col= "orange")+
  geom_sf(data = gye_tienda_p, col= "red", size=2, alpha=0.4)+
  geom_sf(data = gye_vias, col= "grey40")+
  geom_sf(data = osm_rivers, colour = '#9ecae1', fill="#9ecae1", size=3, alpaha=0.9)+
  coord_sf(xlim = c(-69.24  ,-69.16), ylim = c(-12.65 ,-12.55))+
  theme_void() +
  theme( plot.background = element_rect(fill = "black"),
        plot.title = element_text(size = 16, hjust = 0.5, color = "#4e4d47", family="serif", face = "italic"),
        plot.subtitle = element_text(size = 11, hjust = 0.8, face = "italic", color = "#4e4d47", family="serif"),
        plot.caption = element_text(size = 10, hjust = 0.95, color = "springgreen", family="serif", face = "italic"))+
  ggtitle("Mapa del catastro Urbano de \nPuerto Maldonado")+
  labs(subtitle="Madre de Dios capital de la Biodiversidad del Peru", 
       caption="Fuente: https://osmdata.openstreetmap.de/")+
  ggspatial::annotation_north_arrow(
    location = "tl", which_north = "true",
    pad_x = unit(0.4, "in"), pad_y = unit(0.4, "in"),
    style = ggspatial::north_arrow_nautical(
      fill = c("grey10", "white"),
      line_col = "grey10", text_family = "ArcherPro Book" , text_col="white"))+
  ggspatial::annotation_scale(location = "bl",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book", text_col="white")+
  annotate(geom = "text", x = -69.18, y = -12.62, hjust = 0, vjust = 1, 
           label = "Ing.Gorky Florez \n     Castillo",size = 3, family="serif", color = "springgreen",  fontface="italic")






