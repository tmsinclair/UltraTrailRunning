#libraries
library(here)
library(tidyverse)
library(ggmap)
library(maps)
library(viridis)

#####

###
#Process the race locations data
###


#read in the race locations data
races <- read.csv(here("input", "features.csv"))

#remove any duplicate values
races <- filter(races, !duplicated(select(races, City, Country)))

#remove NA values, but keep them if they are the only one in the country
races <- filter(races, !is.na(Country))

#list the coutries with NAs for cities
nas <- filter(races, is.na(City))

for(i in unique(nas$Country)){
  #if there are any other races with a city
  if(nrow(filter(races, Country == i, !is.na(City))) > 0){
    #then remove the NAs for that country from the file
    races <- filter(races, Country != i | Country == i & !is.na(City))
  }
}
  
#create blank latitude and longitude
races$lat <- NA
races$lon <- NA

#create a loop to get the latitude and longitude data for the races
for(i in 1:nrow(races)){
  #use google's API to get lat and long
  lat_lon <- geocode(paste(races$City[i], races$Country[i]))
  races$lat[i] <- lat_lon$lat
  races$lon[i] <- lat_lon$lon
}

#remove any that didn't get a match
races <- filter(races, !is.na(races$lat) | !is.na(races$lon))

#save it
write.csv(races, here("output","race_locations.csv"), row.names = FALSE)
   
  

#####

###
#Map the race locations data
###

#read in the race data
races <- read.csv(here("output","race_locations.csv"))

#get a map of the world
world <- map_data('world')

#plot the world map with the races on coloured by distance

#remove any races without distance data
races_distance <- filter(races, Distance > 50)

#plot the map of the world
plot_world <- ggplot(world, aes(x=long, y=lat, group = group)) +
  geom_polygon(fill="#dda15e", colour="#886633", lwd = 0.25)+
  geom_point(data = races_distance,inherit.aes=FALSE, aes(x = lon, y = lat, col = Distance), size = 1, alpha = 0.95)+
  scale_fill_viridis(name = "Distance (km)", option = "inferno", begin = 0.1, end = 0.9)+
  scale_colour_gradientn(colours = c("#332d40", "#394373", "#327fa6", "#49d1ba"), name = "Distance (km)", 
                         limits = c(149, 181), 
                         breaks = seq(150, 180, 10),
                         labels = seq(150, 180, 10),
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(5, units = "mm"),
                           barwidth = unit(75, units = "mm"),
                           ticks.colour = "886633",
                           ticks.linewidth = 0,
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5
                         ))+
  labs(x = "Longitude", y = "Latitude", title = "Ultra trail running races of the world")+
  theme(
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#e9c587"),
    plot.title = element_text(colour = "#886633", hjust = 0.5, size = 22, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#e9c587"),
    legend.background = element_rect(fill = "#e9c587"),
    legend.text =  element_text(colour = "#886633", size = 14, face = "bold"),
    legend.title =  element_text(colour = "#886633", size = 14, face = "bold"),
    legend.title.align = 1,
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )
plot_world

ggsave(plot = plot_world, here("figs", "world.png"), width = 12, height = 8, dpi = 300)


#focus in on Europe
europe <- c("Albania", "Austria", "Belarus", "Belgium", "Bosnia", "Bulgaria", "Croatia", "Cyprus", "Czech Republic", 
        "Denmark", "Estonia", "Finland", "France", "Germany", "Greece", "Georgia",
        "Hungary","Iceland", "Ireland", "Italy", "Kosovo", "Latvia", "Lithuania", "Luxembourg", 
        "Malta", "Moldova", "Netherlands", "Montenegro", "North Macedonia", "Norway", "Poland", "Portugal", "Romania", "Russia", "Serbia", "Slovakia", "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey", "UK", "Ukraine")

world_europe <- map_data('world', region = europe)

plot_europe <- ggplot(world_europe, aes(x=long, y=lat, group = group)) +
  geom_polygon(fill="#dda15e", colour="#886633")+
  xlim(-25, 50) + ylim(30 ,75) +
  geom_point(data = races_distance,inherit.aes=FALSE, aes(x = lon, y = lat, col = Distance), size = 2.5, alpha = 0.95)+
  scale_fill_viridis(name = "Distance (km)", option = "inferno", begin = 0.1, end = 0.9)+
  scale_colour_gradientn(colours = c("#332d40", "#394373", "#327fa6", "#49d1ba"), name = "Distance (km)", 
                         limits = c(149, 181), 
                         breaks = seq(150, 180, 10),
                         labels = seq(150, 180, 10),
                         guide = guide_colorbar(
    direction = "horizontal",
    barheight = unit(5, units = "mm"),
    barwidth = unit(75, units = "mm"),
    ticks.colour = "886633",
    ticks.linewidth = 0,
    title.position = 'top',
    title.hjust = 0.5,
    label.hjust = 0.5
  ))+
  labs(x = "Longitude", y = "Latitude", title = "Ultra trail running races around Europe")+
  theme(
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#e9c587"),
    plot.title = element_text(colour = "#886633", hjust = 0.5, size = 22, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#e9c587"),
    legend.background = element_rect(fill = "#e9c587"),
    legend.text =  element_text(colour = "#886633", size = 14, face = "bold"),
    legend.title =  element_text(colour = "#886633", size = 14, face = "bold"),
    legend.title.align = 1,
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

plot_europe

ggsave(plot = plot_europe, here("figs", "europe.png"), width = 8, height = 8, dpi = 300)

#alternate: map of NAM
NAm <- c("USA", "Canada", "Mexico", "Costa Rica", "Belize", "Guatemala", "El Salvador", "Nicaragua", "Panama", "Honduras", "Cuba", "Jamaica","Bahamas", "Haiti", "Dominican Republic", "Puerto Rico", "UK", "Netherlands", "Barbados", "Grenada", "Trinidad", "Saint Vincent", "Saint Lucia", "Guadeloupe", "Antigua", "Martinique", "Dominica", "Anguilla")

world_NAm <- map_data('world', region = NAm)

plot_NAm <- ggplot(world_NAm, aes(x=long, y=lat, group = group)) +
  geom_polygon(fill="#dda15e", colour="#886633")+
  xlim(-180, -50) + ylim(7 ,74) +
  geom_point(data = races_distance,inherit.aes=FALSE, aes(x = lon, y = lat, col = Distance), size = 2.5, alpha = 0.95)+
  scale_fill_viridis(name = "Distance (km)", option = "inferno", begin = 0.1, end = 0.9)+
  scale_colour_gradientn(colours = c("#332d40", "#394373", "#327fa6", "#49d1ba"), name = "Distance (km)", 
                         limits = c(149, 181), 
                         breaks = seq(150, 180, 10),
                         labels = seq(150, 180, 10),
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(5, units = "mm"),
                           barwidth = unit(75, units = "mm"),
                           ticks.colour = "886633",
                           ticks.linewidth = 0,
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5
                         ))+
  labs(x = "Longitude", y = "Latitude", title = "Ultra trail running races around North America")+
  theme(
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#e9c587"),
    plot.title = element_text(colour = "#886633", hjust = 0.5, size = 22, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#e9c587"),
    legend.background = element_rect(fill = "#e9c587"),
    legend.text =  element_text(colour = "#886633", size = 14, face = "bold"),
    legend.title =  element_text(colour = "#886633", size = 14, face = "bold"),
    legend.title.align = 1,
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

plot_NAm

ggsave(plot = plot_NAm, here("figs", "NAm.png"), width = 11, height = 8, dpi = 300)

#alternate: map for East Asia
EAsia <- c("China", "Japan", "Taiwan", "North Korea", "South Korea", "Mongolia", "Indonesia", "Philippines", "Brunei", "Cambodia", "Thailand", "Laos", "Vietnam", "Myanmar", "Malaysia", "Timor Leste", "India", "Sri Lanka", "Nepal", "Bangladesh", "Maldives", "Bhutan")

world_EAsia <- map_data('world', region = EAsia)

plot_EAsia <- ggplot(world_EAsia , aes(x=long, y=lat, group = group)) +
  geom_polygon(fill="#dda15e", colour="#886633")+
  xlim(60, 150) + ylim(-10 ,54) +
  geom_point(data = races_distance,inherit.aes=FALSE, aes(x = lon, y = lat, col = Distance), size = 2.5, alpha = 0.95)+
  scale_fill_viridis(name = "Distance (km)", option = "inferno", begin = 0.1, end = 0.9)+
  scale_colour_gradientn(colours = c("#332d40", "#394373", "#327fa6", "#49d1ba"), name = "Distance (km)", 
                         limits = c(149, 181), 
                         breaks = seq(150, 180, 10),
                         labels = seq(150, 180, 10),
                         guide = guide_colorbar(
                           direction = "horizontal",
                           barheight = unit(5, units = "mm"),
                           barwidth = unit(75, units = "mm"),
                           ticks.colour = "886633",
                           ticks.linewidth = 0,
                           title.position = 'top',
                           title.hjust = 0.5,
                           label.hjust = 0.5
                         ))+
  labs(x = "Longitude", y = "Latitude", title = "Ultra trail running races around South & East Asia")+
  theme(
    text = element_text(family = "mono"),
    plot.background = element_rect(fill = "#e9c587"),
    plot.title = element_text(colour = "#886633", hjust = 0.5, size = 22, face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "#e9c587"),
    legend.background = element_rect(fill = "#e9c587"),
    legend.text =  element_text(colour = "#886633", size = 14, face = "bold"),
    legend.title =  element_text(colour = "#886633", size = 14, face = "bold"),
    legend.title.align = 1,
    legend.position = "bottom",
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank()
  )

plot_EAsia

ggsave(plot = plot_EAsia, here("figs", "EAsia.png"), width = 10, height = 8, dpi = 300)