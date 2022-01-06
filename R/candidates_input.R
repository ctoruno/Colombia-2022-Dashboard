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
      inputId = ns("comparison_candidates"),
      label = "Compare with:", 
      choices = candidates.ls %>% map_chr(1),
      multiple = T,
      options = pickerOptions(
        liveSearch = T,
        size = 6,
        maxOptions = 4,
        maxOptionsText = "A maximum of four options is allowed"))
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
      
      # Updating selection values
      observeEvent(input$selected_candidate, {
        updatePickerInput(session = session,
                          inputId = "comparison_candidates",
                          choices = (candidates.ls %>% 
                                       map_chr(1))[- which((candidates.ls %>% 
                                                              map_chr(1)) == input$selected_candidate)])
        
        # Saving values as globals
        glob$main_candidate <- input$selected_candidate
      })
      
      observeEvent(input$comparison_candidates, {
        glob$sec_candidates <- input$comparison_candidates
      })
    }
  )
}

