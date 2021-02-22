#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


ui <- fluidPage(
  
  titlePanel("What will the next word be?"),
  
  
  sidebarLayout(
    sidebarPanel(
      
      (" Please enter in an English phrase. You do not need to worry about spelling or punctuation"),
      
      textInput("phrase", "Please enter in a phrase"),
      submitButton("SUBMIT")
    ),
    
    
    mainPanel(
      ("Your next predicted word is..."),
      strong(textOutput("NextWord")),
      
      hr(),
      print("This prediction was trained from Corpera that was sourced from english news,blogs, and twitter.
            Algorithm uses the ngram model(5n for performance) combined with stupid back-off to account for unseen words in the corpera.")
    )
  )
)


#

   
  library(sbo)
  library(tm)
  library(RWeka)
  
 
  
  load("sbo_predictor.RData")
  
  x <- sbo_predictor(t)
  
  #Returns best prediction
  
  
  NextWord <- function(phrase) {
    # Clean up the phrase

    
    
    #ans <- (predict(x,phrase))
    #Exclude unknown words
    if ((predict(x,phrase))[1]=="<UNK>") { 
      print (predict(x,phrase)[2])
    }
    else{print(predict(x,phrase)[1])}
  }
  
  server <- (function(input, output) {
    
    prediction <- renderText({word <- NextWord(input$phrase)})
    
    output$NextWord <- renderText({prediction()})
    
  })
  
  
# Run the application 
shinyApp(ui = ui, server = server)

