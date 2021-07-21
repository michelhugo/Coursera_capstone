library(tokenizers)
library(textreg)
library(NLP)
library(tm)
library(dplyr)
library(stringr)


load('data/unigrams.Rda')
load('data/bigrams.Rda')
load('data/trigrams.Rda')
load('data/quadgrams.Rda')

freqTable <- table(unigrams$grams)

# Non NLP algorithm
quadgrams$input <- stringr::word(quadgrams$grams, 3)
quadgrams$output <- stringr::word(quadgrams$grams, -1)

trigrams$input <- stringr::word(trigrams$grams, 1, 2)
trigrams$output <- stringr::word(trigrams$grams, -1)

bigrams$input <- stringr::word(bigrams$grams, 1)
bigrams$output <- stringr::word(bigrams$grams, -1)

badWords <- names(read.csv('data/bad_words.csv'))
run <- function(sentence){
    
    # Process the raw input
    input <- processInput(sentence)
    triInput <- paste(tail(input$words, 3), collapse = " ")
    biInput <- paste(tail(input$words, 2), collapse = " ")
    unInput <- paste(tail(input$words, 1), collapse = " ")
    
    if (triInput %in% quadgrams$input){
        idx <- which(quadgrams$input == triInput)
        return (sort(quadgrams$output[idx]))
    }else if (biInput %in% trigrams$input){
        idx <- which(trigrams$input == biInput)
        return (sort(trigrams$output[idx]))
    }else if (unInput %in% bigrams$input){
        idx <- which(unique(bigrams$input == unInput))
        return (sort(bigrams$output[idx]))
    }else{
        return ('OOYOOOO')
    }
}

nextWord <- function(sentence){
    res <- run(sentence)
    freqOut <- freqTable[res]
    sort(freqOut, decreasing = TRUE)
}

useAllGrams <- function(sentence){
    input <- processInput(sentence)
    triInput <- paste(tail(input$words, 3), collapse = " ")
    biInput <- paste(tail(input$words, 2), collapse = " ")
    unInput <- paste(tail(input$words, 1), collapse = " ")
    
    totalRes <- NULL
    if (triInput %in% quadgrams$input){
        idx <- which(quadgrams$input == triInput)
        totalRes <- c(totalRes, quadgrams$output[idx])
    }
    if (biInput %in% trigrams$input){
        idx <- which(trigrams$input == biInput)
        totalRes <-  c(totalRes, trigrams$output[idx])
    }
    if (unInput %in% bigrams$input){
        idx <- which(unique(bigrams$input == unInput))
        totalRes <- c(totalRes, bigrams$output[idx])
    }
    
    totalRes <- sort(totalRes)
    totalRes
}
    

processInput <- function(sentence){
    procSentence <- iconv(sentence,"latin1","ASCII",sub="")
    corpus <- VCorpus(VectorSource(procSentence))
    corpus <- corpus %>%
        tm_map(content_transformer(tolower))%>%
        tm_map(removeNumbers)%>%
        tm_map(removePunctuation)%>%
        tm_map(removeWords, badWords) %>%
        #tm_map(removeWords, stopwords("english")) %>%
        tm_map(stripWhitespace) %>%
        tm_map(PlainTextDocument)
    out <- data.frame(unlist(tokenize_ngrams(convert.tm.to.character(corpus), 
                                      n = 1, n_min = 1)))
    names(out) <- c('words')
    out
}

createCorpus <- function(){
    news <- readLines('data/en_US.news.txt')
    blogs <- readLines('data/en_US.blogs.txt')
    tweets <- readLines('data/en_US.twitter.txt')
    set.seed(0)
    
    allText <- c(news, blogs, tweets)
    
    # sampling
    sampleTxt <- sample(allText, round(length(allText)*0.05))
    
    #select only words
    sampleTxt <- iconv(sampleTxt,"latin1","ASCII",sub="")
    
    # transform into a corpus
    corpus <- VCorpus(VectorSource(sampleTxt))
    
    # cleaning, involving: to lower case, remove numbers and punctuation, remove stop words, remove offensive/profane words
    corpus <- corpus %>%
        tm_map(content_transformer(tolower))%>%
        tm_map(removeNumbers)%>%
        tm_map(removePunctuation)%>%
        tm_map(removeWords, badWords) %>%
        #tm_map(removeWords, stopwords("english")) %>%
        tm_map(stripWhitespace) %>%
        tm_map(PlainTextDocument)
    corpus <- convert.tm.to.character(myCorpus)
    
    unigrams <- unlist(tokenize_ngrams(corpus, n = 1, n_min = 1))
    bigrams <- unlist(tokenize_ngrams(corpus, n = 2, n_min = 2))
    trigrams <- unlist(tokenize_ngrams(corpus, n = 3, n_min = 3))
    quadgrams <- unlist(tokenize_ngrams(corpus, n = 4, n_min = 4))
    
    unigrams <- data.frame(grams = unigrams)
    bigrams <- data.frame(grams = bigrams)
    trigrams <- data.frame(grams = trigrams)
    quadgrams <- data.frame(grams = quadgrams)
    
    save(unigrams, file='data/unigrams.Rda')
    save(bigrams, file='data/bigrams.Rda')
    save(trigrams, file='data/trigrams.Rda')
    save(quadgrams, file='data/quadgrams.Rda')
}