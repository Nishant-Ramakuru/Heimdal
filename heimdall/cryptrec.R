send<-read.csv("Santa.csv")

colnames(send)<-c("text")

myCorpus1 <- Corpus(VectorSource(send$text))
#remove url

removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus1 <- tm_map(myCorpus1, content_transformer(removeURL))
myCorpus1 <- tm_map(myCorpus1, removeNumbers)   
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus1 <- tm_map(myCorpus1, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp","list","character")
myCorpus1<- tm_map(myCorpus1, removeWords, myStopwords)
# remove extra whitespace
myCorpus1 <- tm_map(myCorpus1, stripWhitespace)
myCorpus1 <- tm_map(myCorpus1, content_transformer(tolower))
# keep a copy for stem completion later
#myCorpusCopy <- myCorpus

##STEM COMPLETION
myCorpus1 <- tm_map(myCorpus1, stemDocument) # stem words
#writeLines(strwrap(myCorpus[[6]]$content, 60))

## r refer card data mine now provid link packag cran packag
## mapreduc hadoop ad
#stemCompletion2 <- function(x, dictionary) {
##  x <- unlist(strsplit(as.character(x), " "))
#  x <- x[x != ""]
#  x <- stemCompletion(x, dictionary=dictionary)
#  x <- paste(x, sep="", collapse=" ")
#  PlainTextDocument(stripWhitespace(x))
#}
#myCorpus <- lapply(myCorpus, stemCompletion2, dictionary=myCorpusCopy)
#myCorpus <- Corpus(VectorSource(myCorpus))
#writeLines(strwrap(myCorpus$content[[1]], 60))


####              Building term document matrix      ########################
tdm <- TermDocumentMatrix(myCorpus1,control = list(wordLengths = c(1, Inf)))
freq.terms <- findFreqTerms(tdm, lowfreq = 5)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 5)
df <- data.frame(term = names(term.freq), freq = term.freq)
term<-c(freq.terms)
g<-term[1]
terma<-findAssocs(tdm,g,0.2)
assd<<-c(terma$g[1],terma$g[2],terma$g[3])

#########################################################################################

recv<-read.csv("rec.csv")
colnames(recv)<-c("text")
myCorpus2 <- Corpus(VectorSource(recv$text))
#remove url
removeURL <- function(x) gsub("http[^[:space:]]*", "", x)
myCorpus2 <- tm_map(myCorpus2, content_transformer(removeURL))
myCorpus2 <- tm_map(myCorpus2, removeNumbers)   
# remove anything other than English letters or space
removeNumPunct <- function(x) gsub("[^[:alpha:][:space:]]*", "", x)
myCorpus2 <- tm_map(myCorpus2, content_transformer(removeNumPunct))
# remove stopwords
myStopwords <- c(setdiff(stopwords('english'), c("r", "big")),
                 "use", "see", "used", "via", "amp","list","character")
myCorpus2<- tm_map(myCorpus2, removeWords, myStopwords)
# remove extra whitespace
myCorpus2 <- tm_map(myCorpus2, stripWhitespace)
myCorpus2 <- tm_map(myCorpus2, content_transformer(tolower))
# keep a copy for stem completion later
#myCorpusCopy <- myCorpus

##STEM COMPLETION
myCorpus2 <- tm_map(myCorpus2, stemDocument)
#####
tdm1 <- TermDocumentMatrix(myCorpus2,control = list(wordLengths = c(1, Inf)))
freq.terms1 <- findFreqTerms(tdm1, lowfreq = 5)
term.freq1 <- rowSums(as.matrix(tdm1))
term.freq1 <- subset(term.freq1, term.freq1 >= 3)
df1 <- data.frame(term = names(term.freq1), freq = term.freq1)
term1<-c(freq.terms1)
g1<-term[1]
terma1<-findAssocs(tdm1,g,0.2)
assd1<<-terma1$g[1]
checkas<-function(){
  b<-paste0("High possiblity of code being :- ",g1,"\n Code words may include in :- ",term[2],term[3])
  b
}

