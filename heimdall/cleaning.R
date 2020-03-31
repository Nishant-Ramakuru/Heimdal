
myCorpus <- Corpus(VectorSource(tf.df$text))
#remove url
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus <- tm_map(myCorpus, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp")
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# remove extra whitespace
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
# keep a copy for stem completion later
myCorpusCopy <- myCorpus

##STEM COMPLETION
myCorpus <- tm_map(myCorpus, stemDocument) # stem words
#writeLines(strwrap(myCorpus[[6]]$content, 60))

## r refer card data mine now provid link packag cran packag
## mapreduc hadoop ad
#stemCompletion2 <- function(x, dictionary) {
 # x <- unlist(strsplit(as.character(x), " "))
 # x <- x[x != ""]
# x <- stemCompletion(x, dictionary=dictionary)
# x <- paste(x, sep="", collapse=" ")
 # PlainTextDocument(stripWhitespace(x))
#}
#myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
#myCorpus <- Corpus(VectorSource(myCorpus))
#writeLines(strwrap(myCorpus$content[[1]], 60))


####              Building term document matrix      ########################
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
#(freq.terms <- findFreqTerms(tdm, lowfreq = 40))
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 20)
df <- data.frame(term = names(term.freq), freq = term.freq)