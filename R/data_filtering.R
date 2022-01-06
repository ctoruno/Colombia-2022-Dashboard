## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Data Filtering Module
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
##                1.  Data Filtering UI                                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

submit_input <- function(id) {
  ns <- NS(id)
  tagList(
    actionBttn(
      inputId = ns("submit"),
      label = "Submit",
      style = "unite", 
      color = "success",
      size  =  "sm")
  )
}


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2.  Data Filtering SERVER                                                                 ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

filtering_server <- function(id, data, glob){
  moduleServer(
    id,
    function(input, output, session){
      eventReactive(input$submit, {
        
        # Which data are we gonna filter?
        if (data == "social"){
          dataset <- master_data.df
        } else {
          dataset <- timelines.df
        }
        
        glob$submitted <- paste(TRUE, glob$main_candidate, sep = "_")
              # I just created this give a glob input for eventReactive
        
        # Filtering data
        candidate <- glob$main_candidate
        others    <- glob$sec_candidates
        
        if (is.null(others)) {
          filtered_data <- dataset %>%
            mutate(candidate = if_else(str_detect(.data$text, candidate), 1,0)) %>%
            filter(candidate == 1)
        } else {
          filtered_data <- dataset %>%
            mutate(candidate = if_else(str_detect(.data$text, candidate),1,0),
                   comparison = if_else(str_detect(.data$text, others), 1,0)) %>%
            filter(candidate == 1 | comparison == 1)
        }
        
        return(filtered_data)
        
      })
    }
  )
}

