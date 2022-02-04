## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Candidates Selection Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      December 31st, 2021
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1.  Candidates UI                                                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

candidates_input <- function(id){
  ns <- NS(id)
  tagList(
    pickerInput(
      inputId = ns("selected_candidate"),
      label = "Candidate:", 
      choices = candidates.ls %>%  map_chr(1),
      options = pickerOptions(
        actionsBox = T,
        size = 6,
        liveSearch = T)),
    pickerInput(
      inputId = ns("comparison_candidate"),
      label = "Compare with:", 
      choices = c("None" = "N/A", candidates.ls %>% map_chr(1)),
      multiple = F,
      options = pickerOptions(liveSearch = T,
                              size = 6))
  )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Candidates SERVER                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

candidates_server <- function(id, glob){
  moduleServer(
    id,
    function(input, output, session){
      
      # Defining selection list
      selection.ls <- c("None" = "N/A", candidates.ls %>% map_chr(1))
      
      # Updating selection values
      observeEvent(input$selected_candidate, {
        updatePickerInput(session = session,
                          inputId = "comparison_candidate",
                          choices = selection.ls[- which(selection.ls == input$selected_candidate)])
        
        # Saving values as globals
        glob$main_candidate <- input$selected_candidate
      })
      
      observeEvent(input$comparison_candidate, {
        glob$comp_candidate <- input$comparison_candidate
      })
    }
  )
}

