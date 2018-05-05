#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
source("./BackoffModel.R", local = TRUE)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  
  prediction <- reactive({
    next_suggestion(input$caption)
  })
  
  
  text <- reactive({
    txt <- preprocess(input$caption)
    show_filter(txt. = txt)
  })
  
  output$words <- renderUI( {
    predictWords <- prediction()
    assign('savedWords', predictWords, envir=.GlobalEnv)
    n <- length(predictWords)
    if( n > 0 && nchar(predictWords) > 0) {
      buttons <- list()
      for(i in 1:n) {
        buttons <- list(buttons, list(
          actionButton(inputId = paste("word",i, sep = ""), label =predictWords[i])
        ))
      }
      
      tagList(
        buttons 
      )
    } else {
      tagList("") 
    }
  })
  
  
  observeEvent(input$word1, {
    if(text()==""){
      updateTextInput(session, "caption", value = paste(input$caption, get('savedWords', envir=.GlobalEnv)[1], " ", sep = ""))
    } else {
      split <- strsplit(input$caption, split = " ")[[1]]
      a <- paste(split[1:length(split)-1], collapse = " ")
      if (a==""){
        updateTextInput(session, "caption", value = paste(get('savedWords', envir=.GlobalEnv)[1], " ", sep = ""))
      } else{
        updateTextInput(session, "caption", value = paste(a, " ", get('savedWords', envir=.GlobalEnv)[1], " ", sep = ""))
      }
      
    }
    session$sendCustomMessage(type="refocus",message=list(NULL))
    
  })
  
  observeEvent(input$word2, {
    if(text()==""){
      updateTextInput(session, "caption", value = paste(input$caption, get('savedWords', envir=.GlobalEnv)[2], " ", sep = ""))
    } else {
      split <- strsplit(input$caption, split = " ")[[1]]
      a <- paste(split[1:length(split)-1], collapse = " ")
      
      if (a==""){
        updateTextInput(session, "caption", value = paste(get('savedWords', envir=.GlobalEnv)[2], " ", sep = ""))
      } else{
        updateTextInput(session, "caption", value = paste(a, " ", get('savedWords', envir=.GlobalEnv)[2], " ", sep = ""))
      }
      
    }
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  
  observeEvent(input$word3, {
    if(text()==""){
      updateTextInput(session, "caption", value = paste(input$caption, get('savedWords', envir=.GlobalEnv)[3], " ", sep = ""))
    } else {
      split <- strsplit(input$caption, split = " ")[[1]]
      a <- paste(split[1:length(split)-1], collapse = " ")
      
      if (a==""){
        updateTextInput(session, "caption", value = paste(get('savedWords', envir=.GlobalEnv)[3], " ", sep = ""))
      } else{
        updateTextInput(session, "caption", value = paste(a, " ", get('savedWords', envir=.GlobalEnv)[3], " ", sep = ""))
      }
    }
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  
  observeEvent(input$word4, {
    if(text()==""){
      updateTextInput(session, "caption", value = paste(input$caption, get('savedWords', envir=.GlobalEnv)[4], " ", sep = ""))
    } else {
      split <- strsplit(input$caption, split = " ")[[1]]
      a <- paste(split[1:length(split)-1], collapse = " ")
      
      if (a==""){
        updateTextInput(session, "caption", value = paste(get('savedWords', envir=.GlobalEnv)[4], " ", sep = ""))
      } else{
        updateTextInput(session, "caption", value = paste(a, " ", get('savedWords', envir=.GlobalEnv)[4], " ", sep = ""))
      }
    }
    session$sendCustomMessage(type="refocus",message=list(NULL))
  })
  
})
