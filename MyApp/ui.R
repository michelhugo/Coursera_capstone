
library(shiny)


shinyUI(fluidPage(

    # Application title
    titlePanel("Next word prediction"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            textInput('input', 'Predict next word from: '),
            textOutput('outs')
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = 'tabs',
                        tabPanel('Output', br(), dataTableOutput('out')),
                        tabPanel('Histogram', br(), plotOutput('hist')),
                        tabPanel('Word Cloud', br(), plotOutput('wordClould')))
        )
)))
