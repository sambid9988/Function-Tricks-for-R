install.packages("qdap")


#######function to corpus cleaning
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), "coffee"))
  corpus <- tm_map(corpus, content_transformer(tolower))
  return(corpus)
}
clean_corpus <- function(corpus){
  corpus <- tm_map(corpus, content_transformer(replace_abbreviation))
  corpus <- tm_map(corpus,stripWhitespace)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removeWords, c(stopwords("en"), c("coffee","mug")))
  corpus <- tm_map(corpus, content_transformer(tolower))
  
  return(corpus)
}


## coffee_tdm is still loaded in your workspace
coffee_tdm
# Create a matrix: coffee_m
coffee_m<-as.matrix(coffee_tdm)
dim(coffee_m)
# Calculate the rowSums: term_frequency
term_frequency<-rowSums(coffee_m)

# Sort term_frequency in descending order
term_frequency<-sort(term_frequency,decreasing=T)

# View the top 10 most common words
term_frequency[1:10]

# Plot a barchart of the 10 most common words
barplot(term_frequency[1:10],col="tan",las=2)




qdap

# Create frequency
frequency<-freq_terms(tweets$text,at.least=3,top=10,stopwords="Top200Words")

# Make a frequency barchart
plot(frequency)

# Create frequency2
frequency2<-freq_terms(tweets$text,top=10,at.least=3,tm::stopwords("english"))

# Make a frequency2 barchart
plot(frequency2)

######wordcloudnow


# Load wordcloud package
library(wordcloud)
# Print the first 10 entries in term_frequency
term_frequency[1:10]
names(term_frequency)
# Create word_freqs
word_freqs<-data.frame(term=names(term_frequency),num=term_frequency)

# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term,word_freqs$num,max.words=100,colors="red")











