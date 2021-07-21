library(shiny)
library(tokenizers)
library(textreg)
library(NLP)
library(tm)
library(dplyr)
library(stringr)
library(ggplot2)

library(wordcloud)
library(RColorBrewer)


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


shinyServer(function(input, output) {
    
    rawInput <- reactive({input$input})
    tableFreq <- reactive({nextWord(input$input)})
    
    output$hist <- renderPlot({
        ggplot(data = tableFreq()[1:10, ], aes(x=word, y=freq, fill=freq)) +
            geom_bar(stat='identity') +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
    })
    
    output$wordClould <-  renderPlot({
        wordcloud(words = tableFreq()$word, freq = tableFreq()$freq, min.freq = 1,
                  max.words=50, random.order=FALSE, rot.per=0.35,
                  colors=brewer.pal(8, "Dark2"), scale= c(10,2))
        par(mar=rep(0.1, 4))
    })
    
    output$out <- renderDataTable({tableFreq()})
    
    output$outs <- renderText({
    paste(rawInput(), tableFreq()$word[1])
    })
    

})

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
        return (freqTable)
    }
}

nextWord <- function(sentence){
    res <- run(sentence)
    freqOut <- freqTable[unique(res)]
    freqOut <- sort(freqOut, decreasing = TRUE)
    print(length(freqOut))
    df <- data.frame(freqOut)
    names(df) <- c('word', 'freq')
    df
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