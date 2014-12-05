library(shiny)
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(Hmisc))

shinyServer(function(input, output) {
    
    predWords <- reactive({
        triGramPred(BiTokenTabS, TriTokenTabS, input$words)
    })
    output$result1 <- renderText({
        predWords()[1]
    })
    output$result2 <- renderText({
        predWords()[2]
    })
})