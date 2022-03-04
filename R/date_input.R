## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Candidates Selection Module
##
## Author:           David Granada  (dagrado@gmail.com)
##
## Creation date:     February 8th, 2021
##
## This version:      February 26th, 2022
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Time UI                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
today<-Sys.Date()
date_input <- function(id){
  ns <- NS(id)
  tagList(
      sliderInput(
        inputId = ns("selected_date"),
        label = "Date:", 
        min=  as.Date("2021-11-01"),
        max= today,
        value=today)
    )

}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Candidates SERVER                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

date_server <- function(id, glob, resetX){
  moduleServer(
    id,
    function(input, output, session){
      

      # Selecting date
      observe({
        # Saving values as globals
        glob$date_selected <- input$selected_date
      }) %>% 
        bindEvent(input$selected_date)
      

    }
  )
}
