# NLP - Text Parsing, Stemming, Stopword removal, Term Frequency Matrix

install.packages("tm")  # text mining
library(tm)

# for stemming
install.packages("SnowballC") # text stemming
library(SnowballC)

# read a file MLK_speech.txt
text <- readLines("MLK_Speech.txt")

text


# the file MLK_speech.txt contains text of a most famous speech of Martin Luther King.
# In his speech King called for an end to racism in 
# the United States before a crowd of more than 250,000 people.
# I Have a Dream” – Washington, D.C., April 28, 1963

#----------------------------------------------------
# Text parsing 
#----------------------------------------------------

# load text using the Corpus() function
# this is a list of documents (in our case it is one document).
# the VectorSource() function creates a corpus of character vectors:

docs <- Corpus(VectorSource(text))

docs

# check the document with inspect()
inspect(docs)

#----------------------------------------------------
# Preliminary cleaning
#----------------------------------------------------

# with a function tm_map () 
# replace special characters from text. 
# substituting "/", "@" i "|" by a space.

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

#----------------------------------------------------
# Cleaning text
#----------------------------------------------------

# The tm_map() function is also used to further clean the text
# a) to remove unnecessary spaces, punctuation and numbers
# remove unnecessary spaces
docs <- tm_map(docs, stripWhitespace)

# remove unnecessary punctuation
docs <- tm_map(docs, removePunctuation)

# remove unnecessary numbers
docs <- tm_map(docs, removeNumbers)

# b) change letters to lower case

# change to lowercase
docs <- tm_map(docs, content_transformer(tolower))


#----------------------------------------------------
# Stopword removal 
#----------------------------------------------------
# In the case of "stopwords" in the package tm 
# supported languages are: Danish, Dutch,
# English, Finnish, French, German, Hungarian, Italian,
# Norwegian, Portuguese, Russian, Spanish and Swedish.
# Language names are case-sensitive.

# remove English stopwords
docs <- tm_map(docs, removeWords, stopwords("english"))

# if necessaary: remove your own stopwords - as a vector of words:
docs <- tm_map(docs, removeWords, c("blabla1", "blabla2"))


#----------------------------------------------------
# Stemming 
#----------------------------------------------------

# Stemming reduces words to their root form
# For example, the reduction of words "move", "moved" 
# and "movement" to the core "move".

# stem document
docs <- tm_map(docs, stemDocument)

#----------------------------------------------------
# Term frequency matrix
#----------------------------------------------------

# Matrix is a table containing the frequency of words.
# The TermDocumentMatrix () function is used in the following way:

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)


d


head(d, 10)
