library(rworldmap)
indplot<-function(){
  newmap <- getMap(resolution = "low")
  dfm <- tf.df[complete.cases(tf.df[,15:16]),]
  plot(newmap, xlim = c(60,100), ylim = c(-10,40), asp = 1)
  points(dfm$longitude, dfm$latitude, col = "red", cex = .6)
}
##source("http://biostat.jhsph.edu/~jleek/code/twitterMap.R")
twitterMap("socialmedia2day", fileName="Map_socialmedia2day.pdf", nMax=3000,plotType = "followers")
twitterMap("socialmedia2day", fileName="Map_socialmedia2day.pdf", nMax=3000,plotType = "followers")