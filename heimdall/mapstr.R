library(ggplot2)
library(grid)
# Create an object containing the boundaries of California as 
# longuitude and lattitude.
map.data <- map_data("world",region = "India")
# We only need the long and lat values from the data. 
# These are put in a new object.
points <- data.frame(x = as.numeric(tweets.df$place_lon), 
                     y = as.numeric(tweets.df$place_lat))
# This line is needed for the second plot, when hashtags are added.
points$hashtags <- tweets.df$hashtags
# The next lines are just used to remove points that are not specified or 
# are incidental too far a way from California.
points[!is.na(tweets.df$lon), "x"] <- as.numeric(tweets.df$lon)[!is.na(tweets.df$lon)]
points[!is.na(tweets.df$lat), "y"] <- as.numeric(tweets.df$lat)[!is.na(tweets.df$lat)]
#points <- points[(points$y > 25 & points$y < 42), ]
#points <- points[points$x < -114,]
# The following code creates the graphic.
mapPlot <- ggplot(map.data) + # ggplot is the basic plotting function used.
  # The following lines define the map-areas.
  geom_map(aes(map_id = region), 
           map = map.data, 
           fill = "white", 
           color = "grey20", 
           size = 0.25) +  
  expand_limits(x = map.data$long, 
                y = map.data$lat) + 
  # The following parameters could be altered to insert axes, title, etc.
  theme(axis.line = element_blank(), 
        axis.text = element_blank(), 
        axis.ticks = element_blank(), 
        axis.title = element_blank(), 
        panel.background = element_blank(), 
        panel.border = element_blank(), 
        panel.grid.major = element_blank(), 
        plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + 
  # The next line plots points for each tweet. Size, transparency (alpha) 
  # and color could be altered.
  geom_point(data = points, 
             aes(x = x, y = y), 
             size = 2, 
             alpha = 1/20, 
             color = "steelblue")
mapPlot