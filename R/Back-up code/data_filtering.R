## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Observatory Colombia: Shiny App
##                    Data Filtering Module
##
## Author:            Carlos A. Toruño Paniagua   (carlos.toruno@gmail.com)
##
## Creation date:     December 25th, 2021
##
## This version:      January 19th, 2022
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
        
        glob$submitted <- paste(TRUE, glob$main_candidate, glob$sec_candidates, sep = "_")
            # I just created this in order to give a glob input for eventReactives in other modules
        
        
        # Which data are we gonna filter?
        if (data == "social"){
          
          # Defining candidates variables
          selected_candidate <- glob$main_candidate
          others_selected    <- glob$sec_candidates
          
          # Filtering data
          if (is.null(others_selected)) {
            filtered_data <- master_data.df %>%
              mutate(candidate = if_else(str_detect(text, selected_candidate), 1, 0)) %>%
              filter(candidate == 1)
          } else {
            filtered_data <- master_data.df %>%
              mutate(candidate = if_else(str_detect(text, selected_candidate), 1, 0),
                     comparison = if_else(str_detect(text, others_selected), 1, 0)) %>%
              filter(candidate == 1 | comparison == 1)
          }
          
        } else {
          
          # Defining candidates variables
          selected_candidate <- glob$main_candidate %>% str_sub(2)
          others_selected    <- glob$sec_candidates %>% str_sub(2)
          
          # Filtering data
          if (is.null(others_selected)) {
            filtered_data <- timelines.df %>%
              mutate(candidate = if_else(screen_name %in% selected_candidate, 1, 0)) %>%
              filter(candidate == 1 & is_retweet == F)
          } else {
            filtered_data <- timelines.df %>%
              mutate(candidate = if_else(screen_name %in% selected_candidate, 1, 0),
                     comparison = if_else(screen_name %in% others_selected, 1, 0)) %>%
              filter((candidate == 1 | comparison == 1) & is_retweet == F)
          }
        }
        
        return(filtered_data)
        
      })
    }
  )
}

