library(shiny)

shinyUI(fluidPage(
    titlePanel("A Next Word Predicting App"),
    sidebarLayout(
        sidebarPanel(
            
            textInput("words", "Text Input:"),           
            submitButton("Submit"), 
            tags$hr(),
            h4("Please Read:"),
            h5("This app is able to predict the top 2 most likely words given the sentence typed in above text box.
               If you found the word predicted is like one of the below examples, don't teat them as errors, they have their individual meanings:"),
            h5("<.?!>     - The next word could be any endmark."),
            h5("<num>     - The next word could be any numbers."),
            h5("<email>   - The next word could be any emails."),
            h5("<website> - The next word could be any website links.")
            
        ),
        mainPanel(
            h4("Top 2 most likely words"),
            textOutput("result1"),
            textOutput("result2")
        )
    )
))