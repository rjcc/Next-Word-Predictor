#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(markdown)

# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Next Word Predictor"),
    dashboardSidebar(collapsed = TRUE,
                     sidebarMenu(
                       menuItem("Application", tabName = "application", icon = icon("code")),
                       menuItem("About", icon = icon("user"), tabName = "about")
                     )),
    dashboardBody(
            #Tag to enable cursor focus on text area
            tags$head(tags$script(
              'Shiny.addCustomMessageHandler("refocus",
              function(NULL) {
              document.getElementById("caption").focus();
              });'
                                )
                    ),
    
            tabItems(
             ######## Application #############
                      tabItem(tabName = "application",
                              tabPanel("Prediction",
                                       fluidRow(
                                               column(12, align = "center",
                                                      textAreaInput("caption", "Insert English sentences below:", 
                                                                    width = "100%", height = "100%", resize = "none"),
                                                      h5("Next word predictions:"),
                                                      uiOutput("words"),
                                                      h6("Click to add to your sentences")
                                                    ) 
                                                )
                                      )
                              ),
            #################################
            
            ########## About ################
                      tabItem(tabName = "about",
                              fluidRow(
                                        column(2,
                                               p("")),
                                        column(8,
                                               includeMarkdown("./about.md")),
                                        column(2,
                                               p(""))
                                      )
                            )
                  ),
            #################################
            
            ##Footer#########################
            tags$hr(),
            tags$br(),
            tags$span(style="color:grey", 
                      tags$footer(("© 2018 - Stefan Putra Lionar"), 
                                  align = "center"),
                      tags$br()
                     )
            #################################
    
      )
    )
)