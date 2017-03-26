library(shiny)


shinyUI(fluidPage(
  
  titlePanel("n-gram Text Prediction"),
  hr(),
  fluidRow("This Shiny app accepts a n-gram phrase or sentence and predicts the next word."),
  hr(),
  textInput('wordsInput', 'Type a few words here:',value="a lot of"),
  #actionButton('submitButton', 'Submit'),
  submitButton("Submit"),
  #actionButton("go", "Go"),
  hr(),
  fluidRow("The table below shows a few words that are 
           likely to come next!"),
  br(),
  tableOutput("nextWord")
  #textOutput("pred4word")
  
)
)
