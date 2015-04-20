# code to ggmap MacMon countries and print A1 posters
# for WIO workshop, Nosy Be, Madagascar - April 2015

# Kenya and Tanzania
geocode("Kenya")  
map <- get_map(location=c(lon = 39.35, lat = -4), zoom=6, maptype="hybrid", source="google", crop = FALSE)         
ggmap(map, extent = 'device') 

#set heigh and width, otherwise defaults to incorrect value
setwd("/Users/emilydarling/Documents/Work/GitHub/MacMon/Map_exports")
ggsave("Kenya_Tanzania.pdf",width = 594, heigh = 841, units = "mm", dpi = 300)

# N Mozambique
# Pemba, Vamizi, Quirimbas
geocode("Pemba, Mozambique")  
map <- get_map(location=c(lon = 40.52, lat = -12.17), 
               zoom=8, maptype="hybrid", source="google", crop = FALSE)         
ggmap(map, extent = 'device') 

setwd("/Users/emilydarling/Documents/Work/GitHub/MacMon/Map_exports")
ggsave("Mozambique_Pemba.pdf",width=594, height=841, units = "mm", dpi = 300)

# Madagascar
geocode("Madagascar")  
map <- get_map(location=c(lon = 46.87, lat = -18.77), 
               zoom=6, maptype="hybrid", source="google", crop = FALSE)         
ggmap(map, extent = 'device') 

setwd("/Users/emilydarling/Documents/Work/GitHub/MacMon/Map_exports")
ggsave("Madagascar.pdf",width=594, height=841, units = "mm", dpi = 300)

#Whole WIO
geocode("Madagascar")  
map <- get_map(location=c(lon = 44.87, lat = -13.07), 
               zoom=5, maptype="hybrid", source="google", crop = FALSE)         
ggmap(map, extent = 'device') 

setwd("/Users/emilydarling/Documents/Work/GitHub/MacMon/Map_exports")
ggsave("WIO.pdf",width=594, height=841, units = "mm", dpi = 300)
ggsave("WIO_small.jpeg")
