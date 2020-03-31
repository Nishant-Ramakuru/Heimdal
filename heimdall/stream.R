
requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "oXYmcWvtxPf4EvXnEWLkndfa7" 
consumerSecret <- "zaSXKuEeiVs2EHQTQri57hTVGluj6xbRpsTgP2LfwAsrUhP5zh" 

my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)

my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")
load("my_oauth.Rdata")


###############                  SPECIFY PARAMETERS            ##########################
file = "tweetsi.json"
track = c("Modi","BJP")
follow = NULL
loc =NULL
lang = NULL
minutes = 0.5
time = NULL
tweets = 50


filterStream(file.name = file, 
             track = track,
             follow = follow, 
             locations = loc, 
             language = lang,
             tweets = tweets, 
             oauth = my_oauth)

#########################################################################################
tweets.df <- parseTweets(file)
# Now we can inspect the table and save it.
View(tweets.df)
save(file="tweetsDF.RDATA", tweets.df)

tweets.df$hashtags <- str_extract(tweets.df$text, "#[:alnum:]+")
tweets.df$hashtags <- as.factor(tweets.df$hashtags)
summary(tweets.df$hashtags)